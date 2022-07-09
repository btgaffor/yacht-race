module Main where

import Prelude

import Control.Monad.Reader (ReaderT)
import Data.Array (fold, (..))
import Data.Int (toNumber)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Ecs (Entity(..), cmap, cmapAccumulate, get, global, modifyGlobal, newEntity, sequenceArray_)
import EcsCanvas (closePath, createPrerenderCanvas, fillPath, fillRect, fillText, lineTo, moveTo, renderFromCanvas, rotate, scale, setFillStyle, setFont, translate, withContext)
import EcsGameLoop (Keys, canvasHeight, canvasWidth, runGameEngine)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref (Ref, read)
import ForeignUtil (copySvgToCanvas)
import Graphics.Canvas (Context2D)
import Math (atan2, cos, pi, pow, round, sin, sqrt)
import Model (CartesianVector, GameState(..), GameStateValue(..), Player(..), PolarVector, Position(..), System, Wind(..), proxyWorld)

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
    /\ Position { x: 400.0, y: 3690.0, boatAngle: 0.0, sailAngle: 0.0, speed: 0.0, diagnostic: "", zoom: 1.0, apparentWind: { angle: 0.0, magnitude: 0.0 } }


-----------
-- Frame --
-----------
gameFrame :: Ref Keys -> System Unit
gameFrame keysRef = do
  sequenceArray_
    [ controlPlayer keysRef
    , clampSail
    , updatePlayerSpeed
    , movePlayer
    ]

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
    cmap \(Player /\ Position p@{ boatAngle }) -> Position (p { boatAngle = clampAngle (boatAngle + boatTurnSpeed) })
  when keys.arrowRight do
    cmap \(Player /\ Position p@{ boatAngle }) -> Position (p { boatAngle = clampAngle (boatAngle - boatTurnSpeed) })
  when keys.arrowUp do
    cmap \(Player /\ Position p@{ sailAngle }) ->
      Position (p { sailAngle = sailAngle + sailTurnSpeed })
  when keys.arrowDown do
    cmap \(Player /\ Position p@{ sailAngle }) ->
      Position (p { sailAngle = sailAngle - sailTurnSpeed })
  when keys.space do
    cmap \(Player /\ Position p) -> Position (p { zoom = 0.2 })
  when (not keys.space) do
    cmap \(Player /\ Position p) -> Position (p { zoom = 1.0 })

-- | this keeps the sail from extending past halfway down the boat or retracting past the
--   centerline. It also accounts for close-hauled facing, where the sail won't go past directly
--   down-wind.
clampSail :: System Unit
clampSail =
  cmap \(Player /\ Position p@{ boatAngle, sailAngle } /\ Wind wind) ->
    let
      sailAngle' = sailAngle # clampMin 0.0 # clampMax (pi / 2.0)
    in
      -- TODO: instead of actually clamping the angle, just treat it as clamped and show that it's luffing
      if clampAngle (boatAngle - wind.angle) <= pi then
        Position (p { sailAngle = sailAngle' # clampMax (clampAngle (boatAngle - wind.angle)) })
      else
        Position (p { sailAngle = sailAngle' # clampMax (clampAngle (wind.angle - boatAngle)) })

updatePlayerSpeed :: System Unit
updatePlayerSpeed = do
  Wind wind <- get (Entity global)
  cmap \(Player /\ Position p@{ boatAngle, sailAngle, speed }) -> do
    let
      apparentWind = calculateApparentWind { angle: boatAngle, magnitude: speed } wind

      sailToWindAngle = calculateSailToWindAngle boatAngle sailAngle apparentWind.angle

      sailFraction = calculateSailForce sailToWindAngle

      sailForce = sailFraction * apparentWind.magnitude

      maxSpeed = (sin sailAngle) * sailForce

      speed' = speed + ((maxSpeed - speed) * 0.01)
    Position (p { speed = speed', apparentWind = apparentWind })

calculateApparentWind :: PolarVector -> PolarVector -> PolarVector
calculateApparentWind boat wind =
    (addCartesianVectors (polarToCartesian boat) (polarToCartesian wind)) # cartesianToPolar

polarToCartesian :: PolarVector -> CartesianVector
polarToCartesian { angle, magnitude } = { x: (cos angle) * magnitude, y: (sin angle) * magnitude }

cartesianToPolar :: CartesianVector -> PolarVector
cartesianToPolar { x, y } =
  { angle: clampAngle (atan2 y x), magnitude: sqrt (x * x + y * y) }

addCartesianVectors :: CartesianVector -> CartesianVector -> CartesianVector
addCartesianVectors vec1 vec2 = { x: vec1.x + vec2.x, y: vec1.y + vec2.y }

calculateSailToWindAngle :: Number -> Number -> Number -> Number
calculateSailToWindAngle boatAngle sailAngle windAngle =
  if clampAngle (boatAngle - windAngle) < pi then
    clampAngle ((boatAngle - sailAngle) - windAngle)
  else
    clampAngle (windAngle + (2.0 * pi) - (boatAngle + sailAngle))

movePlayer :: System Unit
movePlayer =
  cmap \(Player /\ Position p@{ x, y, boatAngle, speed }) ->
    let
      x' = x + ((cos boatAngle) * speed)

      y' = y - ((sin boatAngle) * speed)
    in
      Position (p { x = x', y = y' })

calculateSailForce :: Number -> Number
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
  else if angle >= 2.0 * pi then
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
  -- TODO: zoom everything out a bit more, and then have this zoom to a fixed position of the whole course
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
  cmapAccumulate \(Player /\ Position { diagnostic, speed, apparentWind }) -> do
    setFillStyle "black"
    setFont "16px sans-serif"
    fillText ("Speed: " <> (truncateAt speed 2)) 680.0 565.0
    -- fillText (truncateAt speed 2) 740.0 565.0
    withContext do
      translate { translateX: canvasWidth / 2.0, translateY: 30.0 }
      rotate (clampAngle ((negate apparentWind.angle) + pi / 2.0))
      setFillStyle "white"
      fillPath do
        moveTo (-3.0) (-18.0)
        lineTo 3.0 (-18.0)
        lineTo 3.0 0.0
        lineTo 9.0 0.0
        lineTo 0.0 18.0
        lineTo (-9.0) 0.0
        lineTo (-3.0) 0.0
        closePath
    withContext do
      translate { translateX: canvasWidth / 2.0 - 14.0, translateY: 70.0 }
      setFillStyle "white"
      setFont "20px sans-serif"
      fillText (truncateAt apparentWind.magnitude 2) 0.0 0.0
    withContext do
      setFillStyle "black"
      setFont "20px sans-serif"
      fillText diagnostic 10.0 100.0

truncateAt :: Number -> Int -> String
truncateAt value place =
  let
    power = pow 10.0 (toNumber place)
  in
    show $ value # (_ * power) # round # (_ / power)

renderIslands :: ReaderT Context2D Effect Unit
renderIslands = do
  renderFromCanvas "islands-canvas" 0.0 0.0

renderSailForceGraph :: ReaderT Context2D Effect Unit
renderSailForceGraph = do
  withContext do
    setFillStyle "black"
    translate { translateX: 200.0, translateY: 200.0 }
    fillRect { x: -3.0, y: -100.0, width: 3.0, height: 100.0 }
    fillRect { x: 0.0, y: 0.0, width: 100.0, height: 3.0 }
    setFillStyle "red"
    fold $ (0 .. 156) <#> (\v -> (toNumber v) / 100.0) <#> renderForceGraphDot

renderForceGraphDot :: Number -> ReaderT Context2D Effect Unit
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
  cmapAccumulate \(Player /\ Position { x, y, boatAngle, sailAngle } /\ Wind wind) -> do
    withContext do
      translate { translateX: x, translateY: y }
      -- math angles start on the right and increase counter-clockwise
      -- canvas angles start on the top and increase clockwise
      rotate (clampAngle ((negate boatAngle) + pi / 2.0))
      renderFromCanvas "boat" (-8.0) (-20.0)
      -- figure out which side is downwind and render the sail on that side
      if clampAngle (boatAngle - wind.angle) < pi then
        rotate (pi + sailAngle)
      else
        rotate (pi - sailAngle)
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
