module Model where

import Prelude

import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Newtype (class Newtype, unwrap)
import Ecs (class Store, class SequenceArray, class TraverseArray, EntityCount, Global(..), Map, Unique, initStore)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref (new)
import Effect.Unsafe (unsafePerformEffect)
import Type.Prelude (Proxy(..))
import Math (pi)

---------------------------
-- Tagless Final Wrapper --
---------------------------

newtype System a = System (Effect a)

derive instance newtypeSystem :: Newtype (System a) _
derive newtype instance functorSystem :: Functor System
derive newtype instance applySystem :: Apply System
derive newtype instance applicativeSystem :: Applicative System
derive newtype instance bindSystem :: Bind System
derive newtype instance monadSystem :: Monad System
derive newtype instance monadEffectSystem :: MonadEffect System

foreign import squishSystemArray :: forall a. Array (System a) -> System Unit
foreign import travelSystemArray :: forall r. Array (System r) -> System (Array r)

instance sequenceArraySystem :: SequenceArray System where
  sequenceArray_ = squishSystemArray

instance traverseArraySystem :: TraverseArray System where
  traverseArray = travelSystemArray

----------------
-- Components --
----------------

data Level
  = Level Int

data GameStateValue
  = Waiting
  | Running

derive instance eqGameStateValue :: Eq GameStateValue

data GameState
  = GameState GameStateValue

data Player
  = Player

data Position
  = Position { x :: Number, y :: Number, boatAngle :: Number, sailAngle :: Number, speed :: Number, diagnostic :: String, zoom :: Number }

data Velocity
  = Velocity Number

type WindValue = { direction :: Number, velocity :: Number }

data Wind
  = Wind WindValue

-----------
-- World --
-----------

type WorldData
  = { entityCounter :: Global EntityCount
    , level :: Global Level
    , gameState :: Global GameState
    , wind :: Global Wind
    , player :: Unique Player
    , position :: Map Position
    , velocity :: Map Velocity
    }

newtype World = World WorldData
derive instance newtypeWorld :: Newtype (World) _

proxyWorld = Proxy :: Proxy World

initWorld :: Effect World
initWorld = do
  level <- Global <$> new (Level 1)
  gameState <- Global <$> new (GameState Waiting)
  wind <- Global <$> new (Wind { direction: pi, velocity: 5.0 })
  entityCounter <- initStore
  player <- initStore
  position <- initStore
  velocity <- initStore
  pure
    $ World
        { entityCounter
        , level
        , gameState
        , wind
        , player
        , position
        , velocity
        }

world :: World
world = unsafePerformEffect initWorld

instance monadAskSystem :: MonadAsk World System where
  ask = System $ pure world

instance storeEntityCounter :: Store World (Global EntityCount) EntityCount where
  getStore _ = ask <#> unwrap <#> _.entityCounter

instance storePlayer :: Store World (Unique Player) Player where
  getStore _ = ask <#> unwrap <#> _.player

instance storePosition :: Store World (Map Position) Position where
  getStore _ = ask <#> unwrap <#> _.position

instance storeVelocity :: Store World (Map Velocity) Velocity where
  getStore _ = ask <#> unwrap <#> _.velocity

instance storeLevel :: Store World (Global Level) Level where
  getStore _ = ask <#> unwrap <#> _.level

instance storeGameState :: Store World (Global GameState) GameState where
  getStore _ = ask <#> unwrap <#> _.gameState

instance storeWind :: Store World (Global Wind) Wind where
  getStore _ = ask <#> unwrap <#> _.wind
