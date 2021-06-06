module Model where

import Prelude

import Control.Monad.State (gets)
import Data.Tuple (Tuple)
import Ecs (class GetStore, EntityCount, Global(..), Map, Unique, initStore)
import Effect (Effect)
import Effect.Ref (new)
import Type.Prelude (Proxy(..))

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

data Missile
  = Missile

missileComponents = Proxy :: Proxy (Tuple Missile (Tuple Position (Tuple Velocity Collision)))

data Bomb
  = Bomb

bombComponents = Proxy :: Proxy (Tuple Bomb (Tuple Position (Tuple Velocity (Tuple Collision Accelerate))))

data Particle
  = Particle

particleComponents = Proxy :: Proxy (Tuple Particle (Tuple Position Velocity))

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

type WorldInner
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

data World
  = World WorldInner

unWorld :: World -> WorldInner
unWorld (World world) = world

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

instance hasEntityCounter :: GetStore World EntityCount (Global EntityCount) where
  getStore _ = gets (unWorld >>> _.entityCounter)

instance hasPlayer :: GetStore World Player (Unique Player) where
  getStore _ = gets (unWorld >>> _.player)

instance hasAlien :: GetStore World Alien (Map Alien) where
  getStore _ = gets (unWorld >>> _.alien)

instance hasMissile :: GetStore World Missile (Map Missile) where
  getStore _ = gets (unWorld >>> _.missile)

instance hasBomb :: GetStore World Bomb (Map Bomb) where
  getStore _ = gets (unWorld >>> _.bomb)

instance hasParticle :: GetStore World Particle (Map Particle) where
  getStore _ = gets (unWorld >>> _.particle)

instance hasAccelerate :: GetStore World Accelerate (Map Accelerate) where
  getStore _ = gets (unWorld >>> _.accelerate)

instance getStorePosition :: GetStore World Position (Map Position) where
  getStore _ = gets (unWorld >>> _.position)

instance getStoreVelocity :: GetStore World Velocity (Map Velocity) where
  getStore _ = gets (unWorld >>> _.velocity)

instance getStoreCollision :: GetStore World Collision (Map Collision) where
  getStore _ = gets (unWorld >>> _.drawable)

instance getStoreMissileTimer :: GetStore World MissileTimer (Map MissileTimer) where
  getStore _ = gets (unWorld >>> _.missileTimer)

instance getStoreScore :: GetStore World Score (Global Score) where
  getStore _ = gets (unWorld >>> _.score)

instance getStoreLevel :: GetStore World Level (Global Level) where
  getStore _ = gets (unWorld >>> _.level)

instance getStoreGameState :: GetStore World GameState (Global GameState) where
  getStore _ = gets (unWorld >>> _.gameState)
