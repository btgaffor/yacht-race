module Main where

import Prelude
import Control.Monad.Reader (class MonadAsk, ReaderT)
import Data.Array (range, (..), fold)
import Data.Either (Either(..))
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Ecs (class SequenceArray, Entity(..), Not(..), cfold, cmap, cmapAccumulate, cmapM, cmapM_, destroy, get, global, modifyGlobal, newEntity, set, sequenceArray_)
import EcsCanvas (arc, closePath, createPrerenderCanvas, fillPath, fillText, lineTo, moveTo, rect, renderEllipse, renderFromCanvas, setFillStyle, setFont, fillRect, rotate, withContext, translate, scale)
import ForeignUtil (copySvgToCanvas)
import EcsGameLoop (Keys, canvasHeight, canvasWidth, runGameEngine)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt, randomRange)
import Effect.Ref (Ref, modify_, read)
import Effect.Console (log, logShow)
import Graphics.Canvas (Context2D)
import Math (cos, pi, pow, sin, sqrt, abs, round)
import Model (GameState(..), GameStateValue(..), Level(..), Player(..), Position(..), System, Velocity(..), World, proxyWorld, Wind(..), WindValue)

----------
-- Util --
----------
generateRandomBombTimeout :: Effect Int
generateRandomBombTimeout = randomInt 25 1200

-----------
-- Setup --
-----------
gameSetup :: System Unit
gameSetup = do
  void $ newEntity $ Player
    /\ Position { x: 400.0, y: 3690.0, boatAngle: pi / 2.0, sailAngle: pi, speed: 0.0, diagnostic: "hi", zoom: 1.0 }
    /\ Velocity 0.0
  void $ newEntity $ Wind { direction: 0.0, velocity: 5.0 }

-----------
-- Frame --
-----------
gameFrame :: Ref Keys -> System Unit
gameFrame keysRef = do
  sequenceArray_ [ controlPlayer keysRef, clampBoatAngle, flipSailDownWind, clampSail, updatePlayerSpeed, movePlayer ]

-- sequenceArray_ [ controlPlayer keysRef, clampBoatAngle, flipSailDownWind, movePlayer ]
waitForSpace :: Ref Keys -> System Unit
waitForSpace keysRef = do
  keys <- liftEffect $ read keysRef
  modifyGlobal proxyWorld \(GameState state) ->
    if keys.space then
      GameState Running
    else
      GameState state

boatTurnSpeed :: Number
boatTurnSpeed = 0.03

sailTurnSpeed :: Number
sailTurnSpeed = 0.015

zoomSpeed :: Number
zoomSpeed = 1.03

controlPlayer :: Ref Keys -> System Unit
controlPlayer keysRef = do
  keys <- liftEffect $ read keysRef
  when keys.arrowLeft do
    cmap \(Player /\ Position p@{ boatAngle }) -> Position (p { boatAngle = boatAngle - boatTurnSpeed })
  when keys.arrowRight do
    cmap \(Player /\ Position p@{ boatAngle }) -> Position (p { boatAngle = boatAngle + boatTurnSpeed })
  when keys.arrowUp do
    cmap \(Player /\ Position p@{ sailAngle }) ->
      if sailAngle <= pi then
        Position (p { sailAngle = sailAngle - sailTurnSpeed })
      else
        Position (p { sailAngle = sailAngle + sailTurnSpeed })
  when keys.arrowDown do
    cmap \(Player /\ Position p@{ sailAngle }) ->
      if sailAngle <= pi then
        Position (p { sailAngle = sailAngle + sailTurnSpeed })
      else
        Position (p { sailAngle = sailAngle - sailTurnSpeed })
  when keys.space do
    cmap \(Player /\ Position p) -> Position (p { zoom = 0.2 })
  when (not keys.space) do
    cmap \(Player /\ Position p) -> Position (p { zoom = 1.0 })

clampBoatAngle :: System Unit
clampBoatAngle =
  cmap \(Player /\ Position p@{ boatAngle }) ->
    Position (p { boatAngle = clampAngle boatAngle })

-- | check if the wind is on the port or starboard side of the boat and make sure the sail is on the
--   downwind side
flipSailDownWind :: System Unit
flipSailDownWind =
  cmap \(Player /\ Position p@{ boatAngle, sailAngle } /\ Wind wind) ->
    -- wind angle is subtracted so that the angle being checked has its origin at straight up
    if (clampAngle (boatAngle - wind.direction)) <= pi && sailAngle > pi then
      Position (p { sailAngle = pi - (sailAngle - pi) })
    else if (clampAngle (boatAngle - wind.direction)) > pi && sailAngle < pi then
      Position (p { sailAngle = pi + (pi - sailAngle) })
    else
      Position p

-- | this keeps the sail from extending past halfway down the boat or retracting past the
--   centerline. It also accounts for close-hauled facing, where the sail won't go past directly
--   down-wind.
clampSail :: System Unit
clampSail =
  cmap \(Player /\ Position p@{ boatAngle, sailAngle } /\ Wind wind) ->
    if (clampAngle (boatAngle - wind.direction)) <= pi then
      Position
        ( p
            { sailAngle =
              sailAngle
                # clampMax pi -- fully extended, which is halfway up the boat
                # clampMin (pi - (clampAngle (boatAngle - wind.direction))) -- can't go past directly downwind
                # clampMin (pi / 2.0) -- fully retracted, which is parallel with the boat
            }
        )
    else
      Position
        ( p
            { sailAngle =
              sailAngle
                # clampMax (1.5 * pi) -- fully extended, which is halfway up the boat
                # clampMax (2.0 * pi - (clampAngle (boatAngle - wind.direction)) + pi) -- can't go past directly downwind
                # clampMin pi -- fully retracted, which is parallel with the boat
            }
        )

updatePlayerSpeed :: System Unit
updatePlayerSpeed = do
  Wind wind <- get (Entity global)
  cmap \(Player /\ Position p@{ boatAngle, sailAngle, speed }) -> do
    let
      sailToWindAngle = calculateSailToWindAngle (clampAngle (boatAngle + sailAngle)) wind.direction

      sailFraction = calculateSailForce sailToWindAngle

      sailForce = sailFraction * wind.velocity

      maxSpeed =
        if sailAngle <= pi then
          (cos (sailAngle - (pi / 2.0))) * sailForce
        else
          -(cos (sailAngle - (pi / 2.0))) * sailForce

      speed' = speed + ((maxSpeed - speed) * 0.01)
    Position (p { speed = speed', diagnostic = show sailAngle })

calculateSailToWindAngle :: Number -> Number -> Number
calculateSailToWindAngle sailAngle windAngle = abs (sailAngle - windAngle - pi)

movePlayer :: System Unit
movePlayer =
  cmap \(Player /\ Position p@{ x, y, boatAngle, speed }) ->
    let
      x' = x + ((sin boatAngle) * speed)

      y' = y - ((cos boatAngle) * speed)
    in
      Position (p { x = x', y = y' })

calculateSailForce sailToWindAngle =
  if sailToWindAngle >= 0.30 && sailToWindAngle < 0.52 then
    linear sailToWindAngle 0.30 0.52 0.3 0.7
  else if sailToWindAngle >= 0.52 && sailToWindAngle < 0.60 then
    linear sailToWindAngle 0.52 0.60 0.7 0.7
  else if sailToWindAngle >= 0.60 && sailToWindAngle < 0.66 then
    linear sailToWindAngle 0.60 0.66 0.7 0.6
  else if sailToWindAngle >= 0.66 && sailToWindAngle < 1.4 then
    linear sailToWindAngle 0.66 1.4 0.6 0.98
  else if sailToWindAngle >= 1.4 && sailToWindAngle <= (pi / 2.0) then
    linear sailToWindAngle 1.4 (pi / 2.0) 0.98 1.0
  else
    0.0

linear :: Number -> Number -> Number -> Number -> Number -> Number
linear angle angleStart angleEnd valueStart valueEnd = ((angle - angleStart) / (angleEnd - angleStart) * (valueEnd - valueStart)) + valueStart

clampAngle :: Number -> Number
clampAngle angle =
  if angle < 0.0 then
    clampAngle (angle + (2.0 * pi))
  else if angle > 2.0 * pi then
    clampAngle (angle - (2.0 * pi))
  else
    angle

-- | the pipe form of `value # min X # max Y` is confusingly opposite, so this helps make it clearer
clampMax :: forall a. Ord a => a -> a -> a
clampMax = min

-- | the pipe form of `value # min X # max Y` is confusingly opposite, so this helps make it clearer
clampMin :: forall a. Ord a => a -> a -> a
clampMin = max

-----------------
-- RenderFrame --
-----------------
renderFrame :: System (Array (ReaderT Context2D Effect Unit))
renderFrame = do
  -- Wind wind <- get (Entity global)
  setCamera <-
    cmapAccumulate \(Player /\ Position { x, y, zoom }) -> do
      scale { scaleX: zoom, scaleY: zoom }
      translate { translateX: (0.0 - x + (canvasWidth / 2.0 / zoom)), translateY: (0.0 - y + (canvasHeight / 2.0 / zoom)) }
  unsetCamera <-
    cmapAccumulate \(Player /\ Position { x, y, zoom }) -> do
      translate { translateX: (x - (canvasWidth / 2.0 / zoom)), translateY: (y - (canvasHeight / 2.0 / zoom)) }
      scale { scaleX: 1.0 / zoom, scaleY: 1.0 / zoom }
  renderPlayers' <- renderPlayers
  renderHUD' <- renderHUD
  pure $ [ renderWater ] <> setCamera <> [ renderIslands, renderWaiting, renderSailForceGraph ] <> renderPlayers' <> unsetCamera <> renderHUD'

renderWater :: ReaderT Context2D Effect Unit
renderWater = do
  setFillStyle "lightblue"
  fillRect { x: 0.0, y: 0.0, width: canvasWidth, height: canvasHeight }

renderHUD :: System (Array (ReaderT Context2D Effect Unit))
renderHUD =
  cmapAccumulate \(Player /\ Position { x, y, boatAngle, sailAngle, diagnostic, speed } /\ Wind wind) -> do
    setFillStyle "black"
    setFont "16px sans-serif"
    fillText ("Speed: " <> (truncateAt speed 2)) 680.0 565.0
    -- fillText (truncateAt speed 2) 740.0 565.0
    withContext do
      translate { translateX: canvasWidth / 2.0 - 9.0, translateY: 20.0 }
      rotate wind.direction
      setFillStyle "white"
      fillPath do
        moveTo 6.0 0.0
        lineTo 12.0 0.0
        lineTo 12.0 18.0
        lineTo 6.0 18.0
        closePath
      fillPath do
        moveTo 0.0 18.0
        lineTo 18.0 18.0
        lineTo 9.0 36.0
        closePath
    withContext do
      translate { translateX: canvasWidth / 2.0 - 18.0, translateY: 70.0 }
      setFillStyle "white"
      setFont "20px sans-serif"
      fillText (truncateAt wind.velocity 2) 0.0 0.0

truncateAt :: Number -> Int -> String
truncateAt value place =
  let
    power = pow 10.0 (toNumber place)
  in
    show $ value # (_ * power) # round # (_ / power)

renderIslands :: ReaderT Context2D Effect Unit
renderIslands = do
  renderFromCanvas "islands-canvas" (0.0) (0.0)

-- renderEllipse 600.0 600.0 300.0 200.0
-- renderEllipse 1200.0 600.0 300.0 200.0
-- renderEllipse 900.0 1200.0 200.0 300.0
renderSailForceGraph :: ReaderT Context2D Effect Unit
renderSailForceGraph = do
  withContext do
    setFillStyle "black"
    translate { translateX: 200.0, translateY: 200.0 }
    fillRect { x: -3.0, y: -100.0, width: 3.0, height: 100.0 }
    fillRect { x: 0.0, y: 0.0, width: 100.0, height: 3.0 }
    setFillStyle "red"
    fold $ (0 .. 156) <#> (\v -> (toNumber v) / 100.0) <#> renderForceGraphDot

renderForceGraphDot angle = do
  let
    f = calculateSailForce angle

    x = ((sin angle) * f * 100.0)

    y = ((0.0 - (cos angle)) * f * 100.0)
  fillRect { x: x - 1.0, y: y - 1.0, width: 2.0, height: 2.0 }

renderWaiting :: ReaderT Context2D Effect Unit
renderWaiting = do
  setFillStyle "white"
  setFont "64px sans-serif"
  fillText "Press space" 200.0 320.0
  fillText "to start." 270.0 384.0

renderPlayers :: System (Array (ReaderT Context2D Effect Unit))
renderPlayers =
  cmapAccumulate \(Player /\ Position { x, y, boatAngle, sailAngle, diagnostic }) -> do
    withContext do
      translate { translateX: x, translateY: y }
      rotate boatAngle
      -- renderFromCanvas "boat" (x - 10.0) (y - 25.0)
      renderFromCanvas "boat" (-8.0) (-20.0)
      rotate sailAngle
      renderFromCanvas "sail" (-1.5) (-24.0)

-- rotate (0.0 - boatAngle - sailAngle)
-- setFillStyle "black"
-- setFont "16px sans-serif"
-- fillText diagnostic 10.0 20.0
preRenderBoat :: Effect Unit
preRenderBoat =
  createPrerenderCanvas "boat" "16px" "40px" do
    setFillStyle "red"
    fillPath do
      moveTo 8.0 0.0
      lineTo 16.0 16.0
      lineTo 16.0 40.0
      lineTo 0.0 40.0
      lineTo 0.0 16.0
      closePath

preRenderSail :: Effect Unit
preRenderSail =
  createPrerenderCanvas "sail" "3px" "30px" do
    setFillStyle "black"
    fillRect { x: 0.0, y: 0.0, width: 3.0, height: 24.0 }

preRenderMark :: Effect Unit
preRenderMark =
  createPrerenderCanvas "mark" "32px" "28px" do
    setFillStyle "orange"
    fillPath do
      moveTo 16.0 0.0
      lineTo 32.0 28.0
      lineTo 0.0 28.0
      closePath

preRenderIslands :: Effect Unit
preRenderIslands = do
  _ <- copySvgToCanvas "islands-svg" "islands-canvas"
  pure unit

----------
-- Main --
----------
main :: Effect Unit
main = do
  preRenderBoat
  preRenderSail
  preRenderMark
  preRenderIslands
  unwrap (runGameEngine gameSetup gameFrame renderFrame)
