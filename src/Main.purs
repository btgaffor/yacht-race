module Main where

import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.State (evalStateT, gets, lift, modify_)
import CreateBox (createBoxElement, setStyleProp)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error, log, logShow)
import Ecs (class GetStore, class SaveStore, Entity(..), EntityCount, Global, SystemT, cmap, get, initStore, newEntity)
import Web.DOM.Document (createElement) as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (appendChild) as DOM
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement as HTML.HTMLElement
import Web.HTML.Window (document)
import Web.HTML.Window as HTML.Window

data Position
  = Position Int

data World
  = World
    { entityCounter :: Global EntityCount
    , positions :: Map Entity Position
    }

unWorld ::
  World ->
  { entityCounter :: Global EntityCount
  , positions :: Map Entity Position
  }
unWorld (World world) = world

initWorld :: Effect World
initWorld = do
  w <- HTML.window
  d <- HTML.Window.document w
  mBody <- HTML.body d
  defaultElem <- (DOM.createElement "span" (HTML.toDocument d))
  -- Maybe pattern matching
  let
    b = case mBody of
      Nothing -> DOM.Element.toNode (defaultElem)
      Just b' -> HTML.HTMLElement.toNode b'
  boxEl <- createBoxElement "the-box" $ HTML.toDocument d
  newBody <- DOM.appendChild (DOM.Element.toNode boxEl) b

  pure $ World
    { entityCounter: initStore
    , positions: initStore
    }

instance hasEntityCounter :: GetStore World EntityCount (Global EntityCount) where
  getStore _ = gets (unWorld >>> _.entityCounter)

instance saveStoreEntityCounter :: SaveStore World (Global EntityCount) where
  saveStore entityCounter = modify_ (unWorld >>> _ { entityCounter = entityCounter } >>> World)

instance hasPosition :: GetStore World Position (Map Entity Position) where
  getStore _ = gets (unWorld >>> _.positions)

instance saveStorePosition :: SaveStore World (Map Entity Position) where
  saveStore positions = modify_ (unWorld >>> _ { positions = positions } >>> World)

createEntities :: SystemT World (ContT Unit Effect) Unit
createEntities = do
  _ <- newEntity $ Position 20
  pure unit

-- setupInput :: SystemT World (ContT Unit Effect) Unit
-- setupInput = do
--   listener <- lift <<< lift $ eventListener (\e -> log "hi")
--   w <- HTML.window
--   d <- HTML.Window.document w
--   mBody <- HTML.body d
--   defaultElem <- (DOM.createElement "span" (HTML.toDocument d))
--   -- Maybe pattern matching
--   let
--     b = case mBody of
--       Nothing -> DOM.Element.toNode (defaultElem)
--       Just b' -> HTML.HTMLElement.toNode b'
--   addEventListener (EventType "keydown") listener false b

runGame :: SystemT World (ContT Unit Effect) Unit
runGame = do
  createEntities
  gameLoop

gameLoop :: SystemT World (ContT Unit Effect) Unit
gameLoop = do
  cmap $ \(Position p) -> Position (p + 1)
  Position p <- get (Entity 0)
  w <- lift <<< lift $ HTML.window
  -- rather than using a separate function - fold together rendering functions and pass that to rAF
  lift $ ContT $ \next -> void $ HTML.Window.requestAnimationFrame (render p next) w
  -- gameLoop
  pure unit

render :: Int -> (Unit -> Effect Unit) -> Effect Unit
render position next = do
  -- w <- HTML.window
  -- _ <- HTML.Window.requestAnimationFrame (render $ position + 1) w
  container <- getElementById "the-box" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> error "Could not find DOM node to mount on" *> pure unit
    Just node -> setStyleProp "transform" ("translate(" <> show position <> "px, 0)") node *> pure unit

  next unit

main :: Effect Unit
main = void $ do
  world <- initWorld
  runContT (evalStateT runGame world) pure
