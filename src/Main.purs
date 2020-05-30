module Main where

import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.State (evalStateT, gets, lift, modify_)
import CreateBox (createBoxElement, setStyleProp)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Ecs (class GetStore, class SaveStore, Entity(..), EntityCount, Global, SystemT, cmap, get, initStore, newEntity)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Ref (Ref, modify_, new, read) as Ref
import Graphics.Canvas (Context2D, clearRect, fillRect, getCanvasElementById, getContext2D)
import Web.DOM.Document (createElement) as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (appendChild) as DOM
import Web.DOM.Node (toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement as HTML.HTMLElement
import Web.HTML.Window (document)
import Web.HTML.Window as HTML.Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent

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
  pure
    $ World
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

type Keys
  = { arrowLeft :: Boolean
    , arrowRight :: Boolean
    }

handleKeydown :: Ref.Ref Keys -> Event -> Effect Unit
handleKeydown keys event = case KeyboardEvent.fromEvent event of
  Nothing -> pure unit
  -- Just keyboardEvent -> log $ "keydown: " <> KeyboardEvent.code keyboardEvent
  Just keyboardEvent -> case KeyboardEvent.code keyboardEvent of
    "ArrowLeft" -> Ref.modify_ (\s -> s { arrowLeft = true }) keys
    "ArrowRight" -> Ref.modify_ (\s -> s { arrowRight = true }) keys
    _ -> pure unit

handleKeyup :: Ref.Ref Keys -> Event -> Effect Unit
handleKeyup keys event = case KeyboardEvent.fromEvent event of
  Nothing -> pure unit
  Just keyboardEvent -> case KeyboardEvent.code keyboardEvent of
    "ArrowLeft" -> Ref.modify_ (\s -> s { arrowLeft = false }) keys
    "ArrowRight" -> Ref.modify_ (\s -> s { arrowRight = false }) keys
    _ -> pure unit

setupInput :: Ref.Ref Keys -> Effect Unit
setupInput keys = do
  keydownListener <- eventListener (handleKeydown keys)
  keyupListener <- eventListener (handleKeyup keys)
  mBody <- HTML.body =<< HTML.Window.document =<< HTML.window
  case mBody of
    Nothing -> error "no body element to attach event listener to"
    Just body' -> do
      addEventListener (EventType "keydown") keydownListener false (toEventTarget $ HTML.HTMLElement.toNode body')
      addEventListener (EventType "keyup") keyupListener false (toEventTarget $ HTML.HTMLElement.toNode body')

type StepFrame = Keys -> SystemT World (ContT Unit Effect) Unit
type RenderFrame = SystemT World (ContT Unit Effect) (Context2D -> Effect Unit)

runGame :: StepFrame -> RenderFrame -> SystemT World (ContT Unit Effect) Unit
runGame gameFrame'' renderFrame'' = do
  keysRef <- lift <<< lift $ Ref.new { arrowLeft: false, arrowRight: false }
  lift <<< lift $ setupInput keysRef
  createEntities
  gameLoop keysRef gameFrame'' renderFrame''

gameLoop :: Ref.Ref Keys -> StepFrame -> RenderFrame -> SystemT World (ContT Unit Effect) Unit
gameLoop keysRef gameFrame renderFrame = do
  keys <- lift <<< lift $ Ref.read keysRef

  gameFrame keys
  r <- renderFrame

  w <- lift <<< lift $ HTML.window
  lift $ ContT $ \next -> void $ HTML.Window.requestAnimationFrame (rAF r next) w

  gameLoop keysRef gameFrame renderFrame


gameFrame' :: StepFrame
gameFrame' keys = do
  when keys.arrowLeft do cmap $ \(Position p) -> Position (p - 5)
  when keys.arrowRight do cmap $ \(Position p) -> Position (p + 5)

renderFrame' :: RenderFrame
renderFrame' = do
  Position p <- get (Entity 0)
  pure $ \context ->
    renderPlayer p context

renderPlayer :: Int -> Context2D -> Effect Unit
renderPlayer position context =
  fillRect context { x: toNumber position, y: 5.0, width: 20.0, height: 20.0 }

rAF :: (Context2D -> Effect Unit) -> (Unit -> Effect Unit) -> Effect Unit
rAF render next = do
  mCanvas <- getCanvasElementById "canvas"
  case mCanvas of
    Nothing -> error "No canvas"
    Just canvas -> do
      context <- getContext2D canvas
      clearRect context { x: 0.0, y: 0.0, width: 800.0, height: 600.0 }
      render context

  -- container <- getElementById "the-box" =<< (map toNonElementParentNode $ document =<< window)
  -- case container of
  --   Nothing -> error "Could not find the box"
  --   Just node -> setStyleProp "transform" ("translate(" <> show position <> "px, 0)") node *> pure unit
  next unit

main :: Effect Unit
main = do
  world <- initWorld
  runContT (evalStateT (runGame gameFrame' renderFrame') world) pure
  pure unit
