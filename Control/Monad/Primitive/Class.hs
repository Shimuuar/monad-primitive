{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Primitive.Class (
    MonadPrim(..)
  ) where

import Control.Monad.ST          (ST)
import Control.Monad.Primitive   (PrimMonad)
import Control.Monad.Trans.Class (lift)
import Data.Monoid               (Monoid)

import Control.Monad.Trans.Cont               (ContT)
import Control.Monad.Trans.Error              (ErrorT,Error)
import Control.Monad.Trans.Identity           (IdentityT)
import Control.Monad.Trans.Maybe              (MaybeT)
import Control.Monad.Trans.Reader             (ReaderT)
import Control.Monad.Trans.RWS.Strict    as S (RWST)
import Control.Monad.Trans.RWS.Lazy      as L (RWST)
import Control.Monad.Trans.State.Strict  as S (StateT)
import Control.Monad.Trans.State.Lazy    as L (StateT)
import Control.Monad.Trans.Writer.Strict as S (WriterT)
import Control.Monad.Trans.Writer.Lazy   as L (WriterT)


-- | Monads in which computation based on primitive monad (instance of
--   'PrimMonad') could be embedded. Instances must obey following laws:
--
--  > liftPrim . return  = return
--  > liftPrim (m >>= f) = liftPrim m >>= liftPrim f
class (PrimMonad (BasePrimMonad m), Monad m) => MonadPrim m where
  type BasePrimMonad m :: * -> *
  liftPrim :: BasePrimMonad m a -> m a



instance MonadPrim IO where
  type BasePrimMonad IO = IO
  liftPrim = id

instance MonadPrim (ST s) where
  type BasePrimMonad (ST s) = (ST s)
  liftPrim = id

instance MonadPrim m => MonadPrim (ContT r m) where
  type BasePrimMonad (ContT r m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance (MonadPrim m, Error e) => MonadPrim (ErrorT e m) where
  type BasePrimMonad (ErrorT e m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance MonadPrim m => MonadPrim (IdentityT m) where
  type BasePrimMonad (IdentityT m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance MonadPrim m => MonadPrim (MaybeT m) where
  type BasePrimMonad (MaybeT m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance MonadPrim m => MonadPrim (ReaderT r m) where
  type BasePrimMonad (ReaderT r m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance (MonadPrim m, Monoid w) => MonadPrim (S.RWST r w s m) where
  type BasePrimMonad (S.RWST r w s m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance (MonadPrim m, Monoid w) => MonadPrim (L.RWST r w s m) where
  type BasePrimMonad (L.RWST r w s m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance MonadPrim m => MonadPrim (S.StateT s m) where
  type BasePrimMonad (S.StateT s m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance MonadPrim m => MonadPrim (L.StateT s m) where
  type BasePrimMonad (L.StateT s m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance (MonadPrim m, Monoid w) => MonadPrim (S.WriterT w m) where
  type BasePrimMonad (S.WriterT w m) = BasePrimMonad m
  liftPrim = lift . liftPrim

instance (MonadPrim m, Monoid w) => MonadPrim (L.WriterT w m) where
  type BasePrimMonad (L.WriterT w m) = BasePrimMonad m
  liftPrim = lift . liftPrim
