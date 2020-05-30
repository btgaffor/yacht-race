module Model where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.Map (Map)
import Ecs (class GetStore, class SaveStore, Entity, EntityCount, Global, initStore)
import Effect (Effect)

data Position
  = Position Int

data Velocity
  = Velocity Int

data World
  = World
    { entityCounter :: Global EntityCount
    , positions :: Map Entity Position
    , velocities :: Map Entity Velocity
    }

unWorld ::
  World ->
  { entityCounter :: Global EntityCount
  , positions :: Map Entity Position
  , velocities :: Map Entity Velocity
  }
unWorld (World world) = world

initWorld :: Effect World
initWorld = do
  pure
    $ World
        { entityCounter: initStore
        , positions: initStore
        , velocities: initStore
        }

instance hasEntityCounter :: GetStore World EntityCount (Global EntityCount) where
  getStore _ = gets (unWorld >>> _.entityCounter)

instance saveStoreEntityCounter :: SaveStore World (Global EntityCount) where
  saveStore entityCounter = modify_ (unWorld >>> _ { entityCounter = entityCounter } >>> World)

instance getStorePosition :: GetStore World Position (Map Entity Position) where
  getStore _ = gets (unWorld >>> _.positions)

instance saveStorePosition :: SaveStore World (Map Entity Position) where
  saveStore value = modify_ (unWorld >>> _ { positions = value } >>> World)

instance getStoreVelocity :: GetStore World Velocity (Map Entity Velocity) where
  getStore _ = gets (unWorld >>> _.velocities)

instance saveStoreVelocity :: SaveStore World (Map Entity Velocity) where
  saveStore value = modify_ (unWorld >>> _ { velocities = value } >>> World)
