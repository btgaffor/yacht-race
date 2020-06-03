module Model where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.Map (Map)
import Data.Tuple (Tuple)
import Ecs (class GetStore, class SaveStore, Entity, EntityCount, Global(..), Unique, initStore)
import Effect (Effect)
import Type.Prelude (Proxy(..))

data Score = Score Int

data GameStateValue = Waiting | Running | Won | Lost
derive instance eqGameStateValue :: Eq GameStateValue

data GameState = GameState GameStateValue

data Player
  = Player

data Alien = Alien
alienComponents = Proxy :: Proxy (Tuple Alien (Tuple Position (Tuple Velocity Collision)))

data Missile = Missile
missileComponents = Proxy :: Proxy (Tuple Missile (Tuple Position (Tuple Velocity Collision)))

data Bomb = Bomb
bombComponents = Proxy :: Proxy (Tuple Bomb (Tuple Position (Tuple Velocity Collision)))

data Particle = Particle
particleComponents = Proxy :: Proxy (Tuple Particle (Tuple Position Velocity))

data Position
  = Position { x :: Number, y :: Number }

data Velocity
  = Velocity { vx :: Number, vy :: Number }

data Collision
  = Collision { width :: Number, height :: Number }

data MissileTimer = MissileTimer Int

type WorldInner =
  { entityCounter :: Global EntityCount
  , score :: Global Score
  , gameState :: Global GameState
  , player :: Unique Player
  , alien :: Map Entity Alien
  , missile :: Map Entity Missile
  , bomb :: Map Entity Bomb
  , particle :: Map Entity Particle
  , position :: Map Entity Position
  , velocity :: Map Entity Velocity
  , drawable :: Map Entity Collision
  , missileTimer :: Map Entity MissileTimer
  }

data World = World WorldInner

unWorld :: World -> WorldInner
unWorld (World world) = world

initWorld :: Effect World
initWorld = do
  pure
    $ World
        { entityCounter: initStore
        , score: Global (Score 0)
        , gameState: Global (GameState Waiting)
        , player: initStore
        , alien: initStore
        , missile: initStore
        , bomb: initStore
        , particle: initStore
        , position: initStore
        , velocity: initStore
        , drawable: initStore
        , missileTimer: initStore
        }

-------------------
-- EntityCounter --
-------------------
instance hasEntityCounter :: GetStore World EntityCount (Global EntityCount) where
  getStore _ = gets (unWorld >>> _.entityCounter)

instance saveStoreEntityCounter :: SaveStore World (Global EntityCount) where
  saveStore value = modify_ (unWorld >>> _ { entityCounter = value } >>> World)

------------
-- Player --
------------
instance hasPlayer :: GetStore World Player (Unique Player) where
  getStore _ = gets (unWorld >>> _.player)

instance saveStorePlayer :: SaveStore World (Unique Player) where
  saveStore value = modify_ (unWorld >>> _ { player = value } >>> World)

-----------
-- Alien --
-----------
instance hasAlien :: GetStore World Alien (Map Entity Alien) where
  getStore _ = gets (unWorld >>> _.alien)

instance saveStoreAlien :: SaveStore World (Map Entity Alien) where
  saveStore value = modify_ (unWorld >>> _ { alien = value } >>> World)

-------------
-- Missile --
-------------
instance hasMissile :: GetStore World Missile (Map Entity Missile) where
  getStore _ = gets (unWorld >>> _.missile)

instance saveStoreMissile :: SaveStore World (Map Entity Missile) where
  saveStore value = modify_ (unWorld >>> _ { missile = value } >>> World)

----------
-- Bomb --
----------
instance hasBomb :: GetStore World Bomb (Map Entity Bomb) where
  getStore _ = gets (unWorld >>> _.bomb)

instance saveStoreBomb :: SaveStore World (Map Entity Bomb) where
  saveStore value = modify_ (unWorld >>> _ { bomb = value } >>> World)

--------------
-- Particle --
--------------
instance hasParticle :: GetStore World Particle (Map Entity Particle) where
  getStore _ = gets (unWorld >>> _.particle)

instance saveStoreParticle :: SaveStore World (Map Entity Particle) where
  saveStore value = modify_ (unWorld >>> _ { particle = value } >>> World)

--------------
-- Position --
--------------
instance getStorePosition :: GetStore World Position (Map Entity Position) where
  getStore _ = gets (unWorld >>> _.position)

instance saveStorePosition :: SaveStore World (Map Entity Position) where
  saveStore value = modify_ (unWorld >>> _ { position = value } >>> World)

--------------
-- Velocity --
--------------
instance getStoreVelocity :: GetStore World Velocity (Map Entity Velocity) where
  getStore _ = gets (unWorld >>> _.velocity)

instance saveStoreVelocity :: SaveStore World (Map Entity Velocity) where
  saveStore value = modify_ (unWorld >>> _ { velocity = value } >>> World)

---------------
-- Collision --
---------------
instance getStoreCollision :: GetStore World Collision (Map Entity Collision) where
  getStore _ = gets (unWorld >>> _.drawable)

instance saveStoreCollision :: SaveStore World (Map Entity Collision) where
  saveStore value = modify_ (unWorld >>> _ { drawable = value } >>> World)

------------------
-- MissileTimer --
------------------
instance getStoreMissileTimer :: GetStore World MissileTimer (Map Entity MissileTimer) where
  getStore _ = gets (unWorld >>> _.missileTimer)

instance saveStoreMissileTimer :: SaveStore World (Map Entity MissileTimer) where
  saveStore value = modify_ (unWorld >>> _ { missileTimer = value } >>> World)

-----------
-- Score --
-----------
instance getStoreScore :: GetStore World Score (Global Score) where
  getStore _ = gets (unWorld >>> _.score)

instance saveStoreScore :: SaveStore World (Global Score) where
  saveStore value = modify_ (unWorld >>> _ { score = value } >>> World)

---------------
-- GameState --
---------------
instance getStoreGameState :: GetStore World GameState (Global GameState) where
  getStore _ = gets (unWorld >>> _.gameState)

instance saveStoreGameState :: SaveStore World (Global GameState) where
  saveStore value = modify_ (unWorld >>> _ { gameState = value } >>> World)
