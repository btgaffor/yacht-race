module Model where

import Prelude

import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Ecs (class Store, class SequenceArray, class TraverseArray, EntityCount, Global(..), Map, Not(..), Unique, initStore)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref (new)
import Effect.Unsafe (unsafePerformEffect)
import Type.Prelude (Proxy(..))

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

data Score
  = Score Int

data Level
  = Level Int

data GameStateValue
  = Waiting
  | Running
  | Won
  | Lost

derive instance eqGameStateValue :: Eq GameStateValue

data GameState
  = GameState GameStateValue

data Player
  = Player

data Alien
  = Alien

alienComponents = Proxy :: Proxy (Tuple Alien (Tuple Position (Tuple Velocity (Tuple Collision Accelerate))))

notAlienComponents :: Tuple (Not Alien) (Tuple (Not Position) (Tuple (Not Velocity) (Tuple (Not Collision) (Not Accelerate))))
notAlienComponents = (Not :: Not Alien) /\ (Not :: Not Position) /\ (Not :: Not Velocity) /\ (Not :: Not Collision) /\ (Not :: Not Accelerate)

data Missile
  = Missile

missileComponents = Proxy :: Proxy (Tuple Missile (Tuple Position (Tuple Velocity Collision)))

notMissileComponents :: Tuple (Not Missile) (Tuple (Not Position) (Tuple (Not Velocity) (Not Collision)))
notMissileComponents = (Not :: Not Missile) /\ (Not :: Not Position) /\ (Not :: Not Velocity) /\ (Not :: Not Collision)

data Bomb
  = Bomb

bombComponents = Proxy :: Proxy (Tuple Bomb (Tuple Position (Tuple Velocity (Tuple Collision Accelerate))))

notBombComponents :: Tuple (Not Bomb) (Tuple (Not Position) (Tuple (Not Velocity) (Tuple (Not Collision) (Not Accelerate))))
notBombComponents = (Not :: Not Bomb) /\ (Not :: Not Position) /\ (Not :: Not Velocity) /\ (Not :: Not Collision) /\ (Not :: Not Accelerate)

data Particle
  = Particle

particleComponents = Proxy :: Proxy (Tuple Particle (Tuple Position Velocity))

notParticalComponents :: Tuple (Not Particle) (Tuple (Not Position) (Not Velocity))
notParticalComponents = (Not :: Not Particle) /\ (Not :: Not Position) /\ (Not :: Not Velocity)

data Accelerate
  = Accelerate

data Position
  = Position { x :: Number, y :: Number }

data Velocity
  = Velocity { vx :: Number, vy :: Number }

data Collision
  = Collision { width :: Number, height :: Number }

data MissileTimer
  = MissileTimer Int

-----------
-- World --
-----------

type WorldData
  = { entityCounter :: Global EntityCount
    , score :: Global Score
    , level :: Global Level
    , gameState :: Global GameState
    , player :: Unique Player
    , alien :: Map Alien
    , missile :: Map Missile
    , bomb :: Map Bomb
    , particle :: Map Particle
    , accelerate :: Map Accelerate
    , position :: Map Position
    , velocity :: Map Velocity
    , drawable :: Map Collision
    , missileTimer :: Map MissileTimer
    }

newtype World = World WorldData
derive instance newtypeWorld :: Newtype (World) _

proxyWorld = Proxy :: Proxy World

initWorld :: Effect World
initWorld = do
  score <- Global <$> new (Score 0)
  level <- Global <$> new (Level 1)
  gameState <- Global <$> new (GameState Waiting)
  entityCounter <- initStore
  player <- initStore
  alien <- initStore
  missile <- initStore
  bomb <- initStore
  particle <- initStore
  accelerate <- initStore
  position <- initStore
  velocity <- initStore
  drawable <- initStore
  missileTimer <- initStore
  pure
    $ World
        { entityCounter
        , score
        , level
        , gameState
        , player
        , alien
        , missile
        , bomb
        , particle
        , accelerate
        , position
        , velocity
        , drawable
        , missileTimer
        }

world :: World
world = unsafePerformEffect initWorld

instance monadAskSystem :: MonadAsk World System where
  ask = System $ pure world

instance storeEntityCounter :: Store World (Global EntityCount) EntityCount where
  getStore _ = ask <#> unwrap <#> _.entityCounter

instance storePlayer :: Store World (Unique Player) Player where
  getStore _ = ask <#> unwrap <#> _.player

instance storeAlien :: Store World (Map Alien) Alien where
  getStore _ = ask <#> unwrap <#> _.alien

instance storeMissile :: Store World (Map Missile) Missile where
  getStore _ = ask <#> unwrap <#> _.missile

instance storeBomb :: Store World (Map Bomb) Bomb where
  getStore _ = ask <#> unwrap <#> _.bomb

instance storeParticle :: Store World (Map Particle) Particle where
  getStore _ = ask <#> unwrap <#> _.particle

instance storeAccelerate :: Store World (Map Accelerate) Accelerate where
  getStore _ = ask <#> unwrap <#> _.accelerate

instance storePosition :: Store World (Map Position) Position where
  getStore _ = ask <#> unwrap <#> _.position

instance storeVelocity :: Store World (Map Velocity) Velocity where
  getStore _ = ask <#> unwrap <#> _.velocity

instance storeCollision :: Store World (Map Collision) Collision where
  getStore _ = ask <#> unwrap <#> _.drawable

instance storeMissileTimer :: Store World (Map MissileTimer) MissileTimer where
  getStore _ = ask <#> unwrap <#> _.missileTimer

instance storeScore :: Store World (Global Score) Score where
  getStore _ = ask <#> unwrap <#> _.score

instance storeLevel :: Store World (Global Level) Level where
  getStore _ = ask <#> unwrap <#> _.level

instance storeGameState :: Store World (Global GameState) GameState where
  getStore _ = ask <#> unwrap <#> _.gameState
