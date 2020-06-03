module Main where

import Prelude

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Array (range)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse_)
import Data.Tuple.Nested ((/\))
import Ecs (Entity(..), Not(..), SystemT, cfold, cmap, cmapM_, destroy, get, global, modifyGlobal, newEntity, set)
import EcsCanvas (GameSetup, RenderFrame, StepFrame, StepFrameKeys, canvasHeight, canvasWidth, runGameEngine)
import Effect (Effect)
import Effect.Random (randomInt, randomRange)
import Effect.Ref (modify_, read)
import Graphics.Canvas (Context2D, arc, bezierCurveTo, closePath, fillPath, fillText, lineTo, moveTo, rect, setFillStyle, setFont, withContext)
import Math (cos, pi, pow, sin, sqrt)
import Model (Accelerate(..), Alien(..), Bomb(..), Collision(..), GameState(..), GameStateValue(..), Level(..), Missile(..), MissileTimer(..), Particle(..), Player(..), Position(..), Score(..), Velocity(..), World, alienComponents, bombComponents, initWorld, missileComponents, particleComponents)

generateRandomBombTimeout :: Effect Int
generateRandomBombTimeout = randomInt 25 300

gameSetup :: GameSetup World
gameSetup = do
  void $ newEntity $ Player
    /\ Position { x: 375.0, y: 520.0 }
    /\ Velocity { vx: 5.0, vy: 0.0 }
    /\ Collision { width: 50.0, height: 50.0 }
    /\ MissileTimer 0
  createEnemies

createEnemies :: GameSetup World
createEnemies = do
  Level level <- get (Entity global)
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
                /\ Collision { width: 50.0, height: 50.0 }
                /\ MissileTimer t
          )
          [ 20.0, 100.0, 180.0, 260.0, 340.0, 420.0, 500.0 ]
    )
    [ 20.0, 100.0 ]

---------------
-- StepFrame --
---------------
gameFrame :: StepFrameKeys World
gameFrame keysRef = do
  GameState state <- get (Entity 0)
  case state of
    Waiting -> waitForSpace keysRef
    Running -> do
      decrementMissileTimers
      movePlayer keysRef
      playerShootMissile keysRef
      aliensDropBombs
      moveNotPlayers
      bounceAliens
      slowParticles
      clampPlayer
      checkMissileCollisions
      checkBombCollisions
      checkForVictory keysRef
      penalizeMissedMissiles
      clearOffScreen
    Won -> pure unit
    Lost -> pure unit

waitForSpace :: StepFrameKeys World
waitForSpace keysRef = do
  keys <- lift $ read keysRef
  modifyGlobal \(GameState state) ->
    if keys.space then
      GameState Running
    else
      GameState state

decrementMissileTimers :: SystemT World Effect Unit
decrementMissileTimers = cmap $ \(MissileTimer t) -> MissileTimer (t - 1)

movePlayer :: StepFrameKeys World
movePlayer keysRef = do
  keys <- lift $ read keysRef
  when keys.arrowLeft do
    cmap $ \(Player /\ Position { x, y } /\ Velocity { vx, vy }) -> Position { x: x - vx, y }
  when keys.arrowRight do
    cmap $ \(Player /\ Position { x, y } /\ Velocity { vx, vy }) -> Position { x: x + vx, y }

playerShootMissile :: StepFrameKeys World
playerShootMissile keysRef = do
  keys <- lift $ read keysRef
  when keys.space do
    cmapM_ \(Player /\ Position { x, y } /\ MissileTimer t) -> do
      when (t <= 0) do
        void $ newEntity
          $ Missile
          /\ Position { x: x + 20.0, y }
          /\ Velocity { vx: 0.0, vy: -10.0 }
          /\ Collision { width: 10.0, height: 15.0 }
        cmap $ \(Player /\ MissileTimer _) -> MissileTimer 15

aliensDropBombs :: StepFrame World
aliensDropBombs = do
  cmapM_ \(Alien /\ (e :: Entity) /\ Position { x, y } /\ MissileTimer t) -> do
    when (t <= 0) do
      void $ newEntity
        $ Bomb
        /\ Accelerate
        /\ Position { x: x + 20.0, y: y + 30.0 }
        /\ Velocity { vx: 0.0, vy: 10.0 }
        /\ Collision { width: 10.0, height: 10.0 }
      newTimer <- lift $ generateRandomBombTimeout
      set e (MissileTimer newTimer)

moveNotPlayers :: StepFrame World
moveNotPlayers = do
  cmap $ \(Position { x, y } /\ Velocity { vx, vy } /\ (Not :: Not Player) /\ (Not :: Not Accelerate)) -> Position { x: x + vx, y: y + vy }
  Level level <- get (Entity global)
  let multiplier = pow 1.1 (toNumber (level - 1))
  cmap $ \(Position { x, y } /\ Velocity { vx, vy } /\ Accelerate) ->
    Position { x: x + (vx * multiplier), y: y + (vy * multiplier) }

slowParticles :: SystemT World Effect Unit
slowParticles = do
  cmapM_
    $ \(Particle /\ (e :: Entity) /\ Velocity { vx, vy }) ->
        if (sqrt (vx * vx + vy * vy) <= 0.1) then
          destroy e particleComponents
        else
          set e (Velocity { vx: vx * 0.90, vy: vy * 0.90 })

bounceAliens :: StepFrame World
bounceAliens = do
  shouldBounce <-
    cfold
      ( \accumulator (Alien /\ Position { x, y } /\ Collision { width, height }) ->
          accumulator || x <= 0.0 || x + width >= canvasWidth
      )
      false
  when shouldBounce do
    cmap $ \(Alien /\ Velocity v) -> Velocity (v { vx = -v.vx })
    cmap $ \(Alien /\ Position { x, y } /\ Collision { width }) -> Position { x: x # min (canvasWidth - width) # max 0.0, y }

clampPlayer :: SystemT World Effect Unit
clampPlayer = cmap $ \(Player /\ Position { x, y } /\ Collision { width }) -> Position { x: x # min (canvasWidth - width) # max 0.0, y }

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
    $ for (range 1 numParticles) \n -> do
        angle <- lift $ randomRange 0.0 (2.0 * pi)
        let
          vx = 3.0 * cos (angle)

          vy = 3.0 * sin (angle)
        void $ newEntity $ Particle /\ Position { x: x + 25.0, y: y + 25.0 } /\ Velocity { vx, vy }

checkBombCollisions :: StepFrame World
checkBombCollisions =
  do
    cmapM_
    $ \(Player /\ (ePlayer :: Entity) /\ (playerP :: Position) /\ (playerC :: Collision)) -> do
        cmapM_
          $ \(Bomb /\ (eBomb :: Entity) /\ (bombP :: Position) /\ (bombC :: Collision)) -> do
              when (overlaps playerP playerC bombP bombC) do
                modifyGlobal $ \(GameState _) -> GameState Lost

checkForVictory :: StepFrameKeys World
checkForVictory keysRef = do
  numAliens <- cfold (\accumulator Alien -> accumulator + 1) 0
  GameState state <- get (Entity global)
  Level level <- get (Entity global)
  when ((numAliens == 0) && (state == Running)) do
    cmapM_ $ \(Particle /\ (e :: Entity)) -> destroy e particleComponents
    cmapM_ $ \(Missile /\ (e :: Entity)) -> destroy e missileComponents
    cmapM_ $ \(Bomb /\ (e :: Entity)) -> destroy e bombComponents
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
    $ \all@(Missile /\ Position { x, y } /\ Collision { width, height } /\ Velocity _ /\ Score score) ->
        if (x < 0.0) || (x > canvasWidth - width) || (y < 0.0) || (y > 600.0 - width) then
          Right (Score (score - 10))
        else
          Left unit

clearOffScreen :: StepFrame World
clearOffScreen = do
  cmap
    $ \all@(Position { x, y } /\ Collision { width, height } /\ Velocity _) ->
        if (x < 0.0) || (x > canvasWidth - width) || (y < 0.0) || (y > canvasHeight - height) then do
          Nothing
        else
          Just all

-----------------
-- RenderFrame --
-----------------
renderFrame :: RenderFrame World
renderFrame = do
  -- TODO
  GameState state <- get (Entity global)
  renderMissiles <- cfold (\acc (Missile /\ (p :: Position)) -> acc <> renderMissile p) (pure unit)
  renderBombs <- cfold (\acc (Bomb /\ (p :: Position)) -> acc <> renderBomb p) (pure unit)
  renderAliens <- cfold (\acc (Alien /\ (p :: Position)) -> acc <> renderAlien p) (pure unit)
  renderParticles <- cfold (\acc (Particle /\ (p :: Position)) -> acc <> renderParticle p) (pure unit)
  renderPlayers <- cfold (\acc (Player /\ (p :: Position)) -> acc <> renderPlayer p) (pure unit)
  Score score <- get (Entity global)
  Level level <- get (Entity global)
  let
    render = case state of
      Waiting -> renderWaiting level
      Running -> do
        renderMissiles
        renderBombs
        renderAliens
        renderParticles
        renderPlayers
        renderScore score
      Won -> renderVictory score
      Lost -> renderLoss score
  pure $ \context -> runReaderT render context

renderWaiting :: Int -> ReaderT Context2D Effect Unit
renderWaiting level = do
  context <- ask
  lift
    $ withContext context do
        setFillStyle context "white"
        setFont context "64px sans-serif"
        fillText context ("Level: " <> show level) 260.0 256.0
        fillText context ("Press any key") 180.0 320.0
        fillText context ("to start.") 270.0 384.0
  pure unit

renderMissile :: Position -> ReaderT Context2D Effect Unit
renderMissile (Position { x, y }) = do
  context <- ask
  lift
    $ withContext context do
        setFillStyle context "gray"
        fillPath context $ arc context { x: x + 5.0, y: y + 5.0, radius: 5.0, start: 0.0, end: 365.0 }
        fillPath context $ rect context { x, y: y + 5.0, width: 10.0, height: 10.0 }
        setFillStyle context "red"
        fillPath context do
          moveTo context (x + 8.0) (y + 15.0)
          lineTo context (x + 5.0) (y + 21.0)
          lineTo context (x + 2.0) (y + 15.0)
          closePath context
  pure unit

renderBomb :: Position -> ReaderT Context2D Effect Unit
renderBomb (Position { x, y }) = do
  context <- ask
  lift
    $ withContext context do
        setFillStyle context "gray"
        fillPath context $ arc context { x: x + 5.0, y: y + 5.0, radius: 5.0, start: 0.0, end: 365.0 }
  pure unit

renderAlien :: Position -> ReaderT Context2D Effect Unit
renderAlien (Position { x, y }) = do
  context <- ask
  lift
    $ withContext context do
        setFillStyle context "green"
        renderEllipse context (x + 25.0) (y + 20.0) 40.0 20.0
        renderEllipse context (x + 5.0) (y + 25.0) 10.0 30.0
        renderEllipse context (x + 45.0) (y + 25.0) 10.0 30.0
        renderEllipse context (x + 25.0) (y + 25.0) 20.0 50.0
        setFillStyle context "lightgray"
        renderEllipse context (x + 25.0) (y + 35.0) 5.0 10.0
  pure unit

renderParticle :: Position -> ReaderT Context2D Effect Unit
renderParticle (Position { x, y }) = do
  context <- ask
  lift
    $ withContext context do
        setFillStyle context "orange"
        fillPath context $ arc context { x: x - 2.0, y: y - 2.0, radius: 4.0, start: 0.0, end: 365.0 }

renderPlayer :: Position -> ReaderT Context2D Effect Unit
renderPlayer (Position { x, y }) = do
  context <- ask
  lift
    $ withContext context do
        setFillStyle context "lightgray"
        fillPath context do
          moveTo context (x + 25.0) y
          lineTo context (x + 50.0) (y + 50.0)
          lineTo context x (y + 50.0)
          closePath context
        setFillStyle context "red"
        fillPath context do
          moveTo context (x + 20.0) (y + 50.0)
          lineTo context (x + 15.0) (y + 60.0)
          lineTo context (x + 10.0) (y + 50.0)
          closePath context
        fillPath context do
          moveTo context (x + 40.0) (y + 50.0)
          lineTo context (x + 35.0) (y + 60.0)
          lineTo context (x + 30.0) (y + 50.0)
          closePath context
  pure unit

renderScore :: Int -> ReaderT Context2D Effect Unit
renderScore score = do
  context <- ask
  lift
    $ withContext context do
        setFillStyle context "white"
        fillText context ("Score: " <> show score) 20.0 590.0
  pure unit

renderVictory :: Int -> ReaderT Context2D Effect Unit
renderVictory score = do
  context <- ask
  lift
    $ withContext context do
        setFillStyle context "white"
        setFont context "64px sans-serif"
        fillText context ("Victory!") 270.0 280.0
        fillText context ("Final Score: " <> show score) 150.0 350.0
  pure unit

renderLoss :: Int -> ReaderT Context2D Effect Unit
renderLoss score = do
  context <- ask
  lift
    $ withContext context do
        setFillStyle context "white"
        setFont context "64px sans-serif"
        fillText context ("You died!") 240.0 280.0
        fillText context ("Final Score: " <> show score) 150.0 350.0
  pure unit

renderEllipse :: Context2D -> Number -> Number -> Number -> Number -> Effect Unit
renderEllipse context x y width height =
  let
    widthOver2 = width / 2.0

    widthTwoThirds = width * 2.0 / 3.0

    heightOver2 = height / 2.0
  in
    fillPath context do
      moveTo context x (y - heightOver2)
      bezierCurveTo context { cp1x: x + widthTwoThirds, cp1y: y - heightOver2, cp2x: x + widthTwoThirds, cp2y: y + heightOver2, x, y: y + heightOver2 }
      bezierCurveTo context { cp1x: x - widthTwoThirds, cp1y: y + heightOver2, cp2x: x - widthTwoThirds, cp2y: y - heightOver2, x, y: y - heightOver2 }
      closePath context

----------
-- Main --
----------
main :: Effect Unit
main = runGameEngine initWorld gameSetup gameFrame renderFrame
