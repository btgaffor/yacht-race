module Main where

import Prelude

import Control.Monad.Reader (ReaderT, lift, runReaderT)
import Data.Array (range)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse_)
import Data.Tuple.Nested ((/\))
import Ecs (Entity(..), Not(..), SystemT, cfold, cfoldMap, cmap, cmapM_, destroy, get, global, modifyGlobal, newEntity, set)
import EcsCanvas (arc, beginPath, closePath, fill, fillText, lineTo, moveTo, rect, renderEllipse, setFillStyle, setFont)
import EcsGameLoop (GameSetup, RenderFrame, StepFrame, StepFrameKeys, canvasHeight, canvasWidth, runGameEngine)
import Effect (Effect)
import Effect.Random (randomInt, randomRange)
import Effect.Ref (modify_, read)
import Graphics.Canvas (Context2D)
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
  GameState state <- get (Entity global)
  Score score <- get (Entity global)
  Level level <- get (Entity global)

  renderMissiles <- cfoldMap $ \(Missile /\ (p :: Position)) -> renderMissile p
  renderBombs <- cfoldMap $ \(Bomb /\ (p :: Position)) -> renderBomb p
  renderAliens <- cfoldMap $ \(Alien /\ (p :: Position)) -> renderAlien p
  renderParticles <- cfoldMap $ \(Particle /\ (p :: Position)) -> renderParticle p
  renderPlayers <- cfoldMap $ \(Player /\ (p :: Position)) -> renderPlayer p
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
  setFillStyle "white"
  setFont "64px sans-serif"
  fillText ("Level: " <> show level) 260.0 256.0
  fillText "Press any key" 180.0 320.0
  fillText "to start." 270.0 384.0

renderMissile :: Position -> ReaderT Context2D Effect Unit
renderMissile (Position { x, y }) = do
  setFillStyle "gray"
  arc { x: x + 5.0, y: y + 5.0, radius: 5.0, start: 0.0, end: 365.0 }
  fill

  rect { x, y: y + 5.0, width: 10.0, height: 10.0 }
  fill

  setFillStyle "red"
  beginPath
  moveTo (x + 8.0) (y + 15.0)
  lineTo (x + 5.0) (y + 21.0)
  lineTo (x + 2.0) (y + 15.0)
  closePath
  fill

renderBomb :: Position -> ReaderT Context2D Effect Unit
renderBomb (Position { x, y }) = do
  setFillStyle "gray"
  arc { x: x + 5.0, y: y + 5.0, radius: 5.0, start: 0.0, end: 365.0 }
  fill

renderAlien :: Position -> ReaderT Context2D Effect Unit
renderAlien (Position { x, y }) = do
  setFillStyle "green"
  renderEllipse (x + 25.0) (y + 20.0) 40.0 20.0
  renderEllipse (x + 5.0) (y + 25.0) 10.0 30.0
  renderEllipse (x + 45.0) (y + 25.0) 10.0 30.0
  renderEllipse (x + 25.0) (y + 25.0) 20.0 50.0
  setFillStyle "lightgray"
  renderEllipse (x + 25.0) (y + 35.0) 5.0 10.0

renderParticle :: Position -> ReaderT Context2D Effect Unit
renderParticle (Position { x, y }) = do
  setFillStyle "orange"
  arc { x: x - 2.0, y: y - 2.0, radius: 4.0, start: 0.0, end: 365.0 }
  fill

renderPlayer :: Position -> ReaderT Context2D Effect Unit
renderPlayer (Position { x, y }) = do
  setFillStyle "lightgray"

  beginPath
  moveTo (x + 25.0) y
  lineTo (x + 50.0) (y + 50.0)
  lineTo x (y + 50.0)
  closePath
  fill

  setFillStyle "red"
  beginPath
  moveTo (x + 20.0) (y + 50.0)
  lineTo (x + 15.0) (y + 60.0)
  lineTo (x + 10.0) (y + 50.0)
  closePath
  fill

  beginPath
  moveTo (x + 40.0) (y + 50.0)
  lineTo (x + 35.0) (y + 60.0)
  lineTo (x + 30.0) (y + 50.0)
  closePath
  fill

renderScore :: Int -> ReaderT Context2D Effect Unit
renderScore score = do
  setFillStyle "white"
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
main :: Effect Unit
main = runGameEngine initWorld gameSetup gameFrame renderFrame
