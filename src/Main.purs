module Main where

import Prelude

import Data.Int (toNumber)
import Data.Tuple.Nested ((/\))
import Ecs (Entity(..), cmap, get, newEntity)
import EcsCanvas (GameSetup, RenderFrame, StepFrame, runGameEngine)
import Effect (Effect)
import Graphics.Canvas (Context2D, fillRect)
import Model (Position(..), Velocity(..), World, initWorld)

gameSetup :: GameSetup World
gameSetup = do
  _ <- newEntity $ Position 20 /\ Velocity 10
  pure unit

gameFrame :: StepFrame World
gameFrame keys = do
  when keys.arrowLeft do cmap $ \(Position p /\ Velocity v) -> Position (p - v)
  when keys.arrowRight do cmap $ \(Position p /\ Velocity v) -> Position (p + v)

renderFrame :: RenderFrame World
renderFrame = do
  Position p <- get (Entity 0)
  pure $ \context ->
    renderPlayer p context

renderPlayer :: Int -> Context2D -> Effect Unit
renderPlayer position context =
  fillRect context { x: toNumber position, y: 5.0, width: 20.0, height: 20.0 }

main :: Effect Unit
main = runGameEngine initWorld gameSetup gameFrame renderFrame
