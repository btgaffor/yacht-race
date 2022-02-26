module Main where

import Prelude

import Control.Monad.Reader (ReaderT(..), lift, runReaderT)
import Data.Array (range, singleton)
import Data.Either (Either(..))
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse_)
import Data.Tuple.Nested ((/\))
import Ecs (class ExplGet, class ExplMembers, class GetStore, Entity(..), Map, Not(..), SystemT, cfold, cfoldMap, cmap, cmapExternal, cmapM_, destroy, get, global, modifyGlobal, newEntity, set)
import EcsCanvas (arc, closePath, createPrerenderCanvas, fillPath, fillText, lineTo, moveTo, rect, renderEllipse, renderFromCanvas, setFillStyle, setFont)
import EcsGameLoop (GameSetup, RenderFrame, StepFrame, StepFrameKeys, StepFrameKeysStep, canvasHeight, canvasWidth, runGameEngine)
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Random (randomInt, randomRange)
import Effect.Ref (modify_, new, read)
import Effect.Unsafe (unsafePerformEffect)
import ForeignUtil (sequenceEffectArray)
import Graphics.Canvas (Context2D)
import Math (cos, pi, pow, sin, sqrt)
import Model (Accelerate(..), Alien(..), Bomb(..), Collision(..), GameState(..), GameStateValue(..), Level(..), Missile(..), MissileTimer(..), Particle(..), Player(..), Position(..), Score(..), Velocity(..), World, alienComponents, bombComponents, initWorld, missileComponents, notBombComponents, notMissileComponents, notParticalComponents, particleComponents)

generateRandomBombTimeout :: Effect Int
generateRandomBombTimeout = randomInt 25 1200

timeDelta :: Number -> Number
timeDelta value = value * 60.0 / 1000.0

gameSetup :: GameSetup World
gameSetup = do
  void $ newEntity $ Player
    /\ Position { x: 375.0, y: 520.0 }
    /\ Velocity { vx: 5.0, vy: 0.0 }
    /\ Collision { width: 30.0, height: 40.0 }
    /\ MissileTimer 0
  createEnemies

createEnemies :: GameSetup World
createEnemies = do
  traverse_
    ( \y ->
        traverse_
          ( \x -> do
              t <- lift $ generateRandomBombTimeout
              newEntity
                $ Alien
                /\ Accelerate
                /\ Position { x, y }
                /\ Velocity { vx: 2.0, vy: 0.0 }
                /\ Collision { width: 30.0, height: 30.0 }
                /\ MissileTimer t
          )
          [ 12.0, 60.0, 108.0, 156.0, 204.0, 252.0, 300.0, 348.0, 396.0, 444.0, 492.0 ]
    )
    [ 22.0, 70.0, 118.0, 166.0, 214.0, 262.0 ]

createEnemiesNew :: GameSetup World
createEnemiesNew = sequence_ do
  x <- [ 12.0, 60.0, 108.0, 156.0, 204.0, 252.0, 300.0, 348.0, 396.0, 444.0, 492.0 ]
  y <- [ 22.0, 70.0, 118.0, 166.0, 214.0, 262.0 ]
  pure $ do
    t <- lift $ generateRandomBombTimeout
    newEntity
      $ Alien
      /\ Accelerate
      /\ Position { x, y }
      /\ Velocity { vx: 2.0, vy: 0.0 }
      /\ Collision { width: 30.0, height: 30.0 }
      /\ MissileTimer t

---------------
-- StepFrame --
---------------
gameFrame :: StepFrameKeys World
gameFrame keysRef = do
  GameState state <- get (Entity 0)
  pure $ case state of
    Waiting -> [waitForSpace keysRef]
    Running ->
      [ decrementMissileTimers
      , movePlayer keysRef
      -- , playerShootMissile keysRef
      -- , aliensDropBombs
      , moveNotPlayers
      , bounceAliens
      , slowParticles
      , clampPlayer
      -- , checkMissileCollisions
      -- , checkBombCollisions
      , checkForVictory keysRef
      , penalizeMissedMissiles
      , clearOffScreen
      ]
    Won -> []
    Lost -> []

waitForSpace :: StepFrameKeysStep World
waitForSpace keysRef = do
  keys <- lift $ read keysRef
  modifyGlobal \(GameState state) ->
    if keys.space then
      GameState Running
    else
      GameState state

decrementMissileTimers :: SystemT World Effect Unit
decrementMissileTimers =
  cmap $ \(MissileTimer t) -> MissileTimer (t - 1)

movePlayerLeft :: SystemT World Effect Unit
movePlayerLeft =
  cmap $ \(Player /\ Position { x, y } /\ Velocity { vx }) -> Position { x: x - vx, y }

movePlayerRight :: SystemT World Effect Unit
movePlayerRight =
  cmap $ \(Player /\ Position { x, y } /\ Velocity { vx }) -> Position { x: x + vx, y }

movePlayer :: StepFrameKeysStep World
movePlayer keysRef = do
  keys <- lift $ read keysRef
  when keys.arrowLeft movePlayerLeft
  when keys.arrowRight movePlayerRight

playerShootMissile :: StepFrameKeysStep World
playerShootMissile keysRef = do
  keys <- lift $ read keysRef
  when keys.space do
    -- cmap \(Player /\ MissileTimer t) ->
    --   if (t <= 0) then
    --     Left $ MissileTimer 15
    --   else
    --     Right unit
    cmapM_ \(Player /\ Position { x, y } /\ MissileTimer t) -> do
      when (t <= 0) do
        _ <- newEntity
          $ Missile
          /\ Position { x: x + 12.0, y }
          /\ Velocity { vx: 0.0, vy: -10.0 }
          /\ Collision { width: 6.0, height: 9.0 }
        cmap $ \(Player /\ MissileTimer _) -> MissileTimer 15

aliensDropBombs :: StepFrame World
aliensDropBombs = do
  cmapM_ \(Alien /\ (e :: Entity) /\ Position { x, y } /\ MissileTimer t) -> do
    when (t <= 0) do
      void $ newEntity
        $ Bomb
        /\ Accelerate
        /\ Position { x: x + 12.0, y: y + 24.0 }
        /\ Velocity { vx: 0.0, vy: 8.0 }
        /\ Collision { width: 6.0, height: 6.0 }
      newTimer <- lift $ generateRandomBombTimeout
      set e (MissileTimer newTimer)

moveNotAccelerate :: StepFrame World
moveNotAccelerate =
  cmap $ \(Position { x, y } /\ (Not :: Not Player) /\ (Not :: Not Accelerate) /\ Velocity { vx, vy }) -> Position { x: x + vx, y: y + vy }

moveNotPlayers :: StepFrame World
moveNotPlayers = do
  moveNotAccelerate
  Level level <- get (Entity global)
  let multiplier = pow 1.1 (toNumber (level - 1))
  cmap
    $ \(Accelerate /\ Position { x, y } /\ Velocity { vx, vy }) ->
        Position { x: x + (vx * multiplier), y: y + (vy * multiplier) }

slowParticles :: StepFrame World
slowParticles =
  cmap
    $ \(Particle /\ (e :: Entity) /\ Velocity { vx, vy }) ->
        if (sqrt (vx * vx + vy * vy) <= 0.1) then
          Left notParticalComponents
        else
          Right (Velocity { vx: vx * 0.90, vy: vy * 0.90 })
  -- cmapM_
  --   $ \(Particle /\ (e :: Entity) /\ Velocity { vx, vy }) ->
  --       if (sqrt (vx * vx + vy * vy) <= 0.1) then
  --         destroy e particleComponents
  --       else
  --         set e (Velocity { vx: vx * 0.90, vy: vy * 0.90 })

reverseAlienVelocity :: StepFrame World
reverseAlienVelocity =
  cmap $ \(Alien /\ Velocity v) -> Velocity (v { vx = -v.vx })

clampAliens :: StepFrame World
clampAliens =
  cmap $ \(Alien /\ Position { x, y } /\ Collision { width }) -> Position { x: x # min (canvasWidth - width) # max 0.0, y }

bounceAliens :: StepFrame World
bounceAliens = do
  shouldBounce <-
    cfold
      ( \accumulator (Alien /\ Position { x } /\ Collision { width }) ->
          accumulator || x <= 0.0 || x + width >= canvasWidth
      )
      false
  when shouldBounce do
    reverseAlienVelocity
    clampAliens

clampPlayer :: SystemT World Effect Unit
clampPlayer =
  cmap $ \(Player /\ Position { x, y } /\ Collision { width }) -> Position { x: x # min (canvasWidth - width) # max 0.0, y }

checkMissileCollisions :: StepFrame World
checkMissileCollisions = do
  cmapM_
    $ \(Alien /\ (eAlien :: Entity) /\ (alienP :: Position) /\ (alienC :: Collision)) -> do
        cmapM_
          $ \(Missile /\ (eMissile :: Entity) /\ (missileP :: Position) /\ (missileC :: Collision)) -> do
              when (overlaps alienP alienC missileP missileC) do
                destroy eAlien alienComponents
                destroy eMissile missileComponents
                modifyGlobal $ \(Score score) -> Score (score + 50)
                generateAlienParticles alienP

generateAlienParticles :: Position -> SystemT World Effect Unit
generateAlienParticles (Position { x, y }) = do
  numParticles <- lift $ randomInt 3 5
  void
    $ for (range 1 numParticles) \_ -> do
        angle <- lift $ randomRange 0.0 (2.0 * pi)
        let
          vx = 3.0 * cos (angle)

          vy = 3.0 * sin (angle)
        void $ newEntity $ Particle /\ Position { x: x + 15.0, y: y + 15.0 } /\ Velocity { vx, vy }

checkBombCollisions :: StepFrame World
checkBombCollisions =
    cmapM_
    $ \(Player /\ (playerP :: Position) /\ (playerC :: Collision)) ->
        cmapM_
          $ \(Bomb /\ (bombP :: Position) /\ (bombC :: Collision)) ->
              when (overlaps playerP playerC bombP bombC) do
                modifyGlobal $ \(GameState _) -> GameState Lost

destroyParticles :: StepFrame World
destroyParticles =
  cmap $ \(Particle) -> notParticalComponents

destroyMissiles :: StepFrame World
destroyMissiles =
  cmap $ \(Missile) -> notMissileComponents

destroyBombs :: StepFrame World
destroyBombs =
  cmap $ \(Bomb) -> notBombComponents

checkForVictory :: StepFrameKeysStep World
checkForVictory keysRef = do
  numAliens <- cfold (\accumulator Alien -> accumulator + 1) 0
  GameState state <- get (Entity global)
  Level level <- get (Entity global)
  when ((numAliens == 0) && (state == Running)) do
    destroyParticles
    destroyMissiles
    destroyBombs
    set (Entity global) (GameState Waiting /\ Level (level + 1))
    createEnemies
    lift $ modify_ (\s -> s { space = false }) keysRef

overlaps :: Position -> Collision -> Position -> Collision -> Boolean
overlaps (Position p1) (Collision d1) (Position p2) (Collision d2) =
  let
    xOverlaps = p1.x + d1.width >= p2.x && p1.x <= p2.x + d2.width

    yOverlaps = p1.y + d1.height >= p2.y && p1.y <= p2.y + d2.height
  in
    xOverlaps && yOverlaps

penalizeMissedMissiles :: StepFrame World
penalizeMissedMissiles =
  cmap
    $ \(Missile /\ Position { x, y } /\ Collision { width, height } /\ Velocity _ /\ Score score) ->
        if (x < 0.0) || (x > canvasWidth - width) || (y < 0.0) || (y > 600.0 - height) then
          Right (Score (score - 10))
        else
          Left unit

clearOffScreen :: StepFrame World
clearOffScreen = do
  cmap
    $ \(Position { x, y } /\ Collision { width, height } /\ Velocity _) ->
        if (x < 0.0) || (x > canvasWidth - width) || (y < 0.0) || (y > canvasHeight - height) then do
          Left $ (Not :: Not Position) /\ (Not :: Not Collision) /\ (Not :: Not Velocity)
        else
          Right unit

-----------------
-- RenderFrame --
-----------------
renderFrame :: RenderFrame World
renderFrame = do
  GameState state <- get (Entity global)
  Score score <- get (Entity global)
  Level level <- get (Entity global)
  renderMissiles <- cmapExternal $ \(Missile /\ Position { x, y }) -> renderFromCanvas "missile" x y
  renderPlayers <- cmapExternal $ \(Player /\ Position { x, y }) -> renderFromCanvas "player" x y
  renderBombs <- cmapExternal $ \(Bomb /\ Position { x, y }) -> renderFromCanvas "bomb" x y
  renderAliens <- cmapExternal $ \(Alien /\ Position { x, y }) -> renderFromCanvas "alien" x y
  renderParticles <- cmapExternal $ \(Particle /\ Position { x, y }) -> renderFromCanvas "particle" x y
  pure $ case state of
      Waiting -> [renderWaiting level]
      Running -> renderMissiles <> renderPlayers <> renderAliens <> renderBombs <> renderParticles <> [renderScore score]
      Won -> [renderVictory score]
      Lost -> [renderLoss score]

preRenderPlayer :: Effect Unit
preRenderPlayer =
  createPrerenderCanvas "player" "30px" "40px" do
    setFillStyle "lightgray"
    fillPath do
      moveTo 15.0 0.0
      lineTo 30.0 35.0
      lineTo 0.0 35.0
      closePath
    setFillStyle "red"
    fillPath do
      moveTo 10.0 35.0
      lineTo 7.5 40.0
      lineTo 5.0 35.0
      closePath
    fillPath do
      moveTo 25.0 35.0
      lineTo 22.5 40.0
      lineTo 20.0 35.0
      closePath

preRenderMissile :: Effect Unit
preRenderMissile =
  createPrerenderCanvas "missile" "6px" "9px" do
    setFillStyle "gray"
    fillPath $ arc { x: 3.0, y: 3.0, radius: 3.0, start: 0.0, end: 365.0 }
    fillPath $ rect { x: 0.0, y: 3.0, width: 6.0, height: 6.0 }
    setFillStyle "red"
    fillPath do
      moveTo 5.0 9.0
      lineTo 3.0 13.0
      lineTo 1.0 9.0
      closePath

preRenderAlien :: Effect Unit
preRenderAlien =
  createPrerenderCanvas "alien" "30px" "30px" do
    setFillStyle "green"
    renderEllipse 15.0 12.0 24.0 12.0 -- horizontal body
    renderEllipse 3.0 15.0 6.0 18.0 -- left wing
    renderEllipse 27.0 15.0 6.0 18.0 -- right wing
    renderEllipse 15.0 15.0 12.0 30.0 -- vertical body
    setFillStyle "lightgray"
    renderEllipse 15.0 21.0 3.0 6.0 -- cockpit

preRenderBomb :: Effect Unit
preRenderBomb =
  createPrerenderCanvas "bomb" "6px" "6px" do
    setFillStyle "gray"
    fillPath $ arc { x: 3.0, y: 3.0, radius: 3.0, start: 0.0, end: 365.0 }

preRenderParticle :: Effect Unit
preRenderParticle =
  createPrerenderCanvas "particle" "6px" "6px" do
    setFillStyle "orange"
    fillPath $ arc { x: 3.0, y: 3.0, radius: 3.0, start: 0.0, end: 365.0 }

renderWaiting :: Int -> ReaderT Context2D Effect Unit
renderWaiting level = do
  setFillStyle "white"
  setFont "64px sans-serif"
  fillText ("Level: " <> show level) 260.0 256.0
  fillText "Press any key" 180.0 320.0
  fillText "to start." 270.0 384.0

renderScore :: Int -> ReaderT Context2D Effect Unit
renderScore score = do
  setFillStyle "white"
  setFont "12px sans-serif"
  fillText ("Score: " <> show score) 20.0 590.0

renderVictory :: Int -> ReaderT Context2D Effect Unit
renderVictory score = do
  setFillStyle "white"
  setFont "64px sans-serif"
  fillText ("Victory!") 270.0 280.0
  fillText ("Final Score: " <> show score) 150.0 350.0

renderLoss :: Int -> ReaderT Context2D Effect Unit
renderLoss score = do
  setFillStyle "white"
  setFont "64px sans-serif"
  fillText ("You died!") 240.0 280.0
  fillText ("Final Score: " <> show score) 150.0 350.0

----------
-- Main --
----------
mainOld :: Effect Unit
mainOld = do
  preRenderPlayer
  preRenderMissile
  preRenderAlien
  preRenderBomb
  preRenderParticle
  runGameEngine initWorld gameSetup gameFrame renderFrame


------------
-- Main 2 --
------------

gameSetup2 :: GameSetup World
gameSetup2 = do
  traverse_
    (\_ -> newEntity $ MissileTimer 0)
    (range 1 5000)
  traverse_
    (\_ -> newEntity $ MissileTimer 0)
    (range 1 5000)

doNothingWithPlayer :: Player -> Unit
doNothingWithPlayer Player = unit

precomputedMapPlayer :: SystemT World Effect Unit
precomputedMapPlayer = cmap $ \Player -> unit

precomputedMapMissleTimer :: SystemT World Effect Unit
precomputedMapMissleTimer = cmap $ \(MissileTimer t) -> (MissileTimer (t + 1))

gameFrame2 :: StepFrameKeys World
gameFrame2 _ = do
  GameState state <- get (Entity 0)
  pure [precomputedMapMissleTimer]

renderFrame2 :: RenderFrame World
renderFrame2 = do
  pure []
  -- pure $ \context -> pure unit

mainNew :: Effect Unit
mainNew = runGameEngine initWorld gameSetup2 gameFrame renderFrame2

main = mainOld
