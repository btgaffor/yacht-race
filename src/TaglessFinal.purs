module TaglessFinal where

-- import Prelude

-- import Control.Monad.Reader.Class (class MonadAsk, ask)
-- import Control.Monad.State.Class (class MonadState, get, put)
-- import Data.Newtype (class Newtype, unwrap)
-- import Data.Tuple (Tuple(..))
-- import Ecs (EntityCount, Global(..), Map, Not(..), Unique, SystemT, initStore)
-- import Effect (Effect)
-- import Effect.Class (class MonadEffect)
-- import Effect.Class.Console (log)
-- import Effect.Ref (Ref, new, read, write)
-- import Effect.Unsafe (unsafePerformEffect)
-- import Model (Alien)
-- import Type.Prelude (Proxy)

-- newtype System a = System (Effect a)

-- derive instance newtypeSystem :: Newtype (System a) _
-- derive newtype instance functorSystem :: Functor System
-- derive newtype instance applySystem :: Apply System
-- derive newtype instance applicativeSystem :: Applicative System
-- derive newtype instance bindSystem :: Bind System
-- derive newtype instance monadSystem :: Monad System
-- derive newtype instance monadEffectSystem :: MonadEffect System

-- type WorldData = { alien :: Map Alien }

-- newtype World = World WorldData
-- derive instance newtypeWorld :: Newtype (World) _

-- initWorld :: Effect World
-- initWorld = do
--   alien <- initStore
--   pure $ World { alien }

-- world :: World
-- world = unsafePerformEffect initWorld

-- -- instance monadStateSystem :: MonadState World System where
-- --   state f = System do
-- --     Tuple a s <- f <$> read worldRef
-- --     write s worldRef
-- --     pure a

-- instance monadAskSystem :: MonadAsk World System where
--   ask = System $ pure world

-- class GetStore :: Type -> Type -> Type -> Constraint
-- class GetStore w c s | c -> s where
--   getStore :: ∀ m. MonadAsk w m => Proxy c -> m s

-- instance getStoreAlien :: GetStore World Alien (Map Alien) where
--   getStore _ = ask <#> unwrap <#> _.alien

-- -- getScore :: System Int
-- -- getScore = map _.score ask

-- -- getScore2 :: System Int
-- -- getScore2 =
-- --   ask <#> _.score

-- -- runApp :: System Unit
-- -- runApp = do
-- --   _world <- ask
-- --   log "hi there"
-- --   state <- get
-- --   put (state { score = state.score + 1 })
