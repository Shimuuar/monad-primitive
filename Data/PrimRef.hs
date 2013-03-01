{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module    : Data.PrimRef
-- Copyright : (c) 2013 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : alexey.skladnoy@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Mutable references in monads which are instances of 'MonadPrim'.
module Data.PrimRef (
    -- * PrimRefs
    PrimRef
  , newPrimRef
  , readPrimRef
  , writePrimRef
  , modifyPrimRef
  , modifyPrimRef'
  ) where

import Control.Monad.Primitive (PrimMonad(..))
import GHC.Base                (MutVar#,newMutVar#,readMutVar#,writeMutVar#)
import Prelude                 (($),($!))


-- | Mutable variable which full analog of 'Data.IORef.IORef' or
--   'Data.STRef.STRef' but could use either of the monads.
--   Unfortunately there's no way to convert @PrimRef@ to @STRef@ or
--   @IORef@.
data PrimRef m a = PrimRef (MutVar# (PrimState m) a)

-- | Create new mutable variable with initial value @a@.
newPrimRef :: PrimMonad m => a -> m (PrimRef m a)
newPrimRef a = primitive $ \s1# ->
  case newMutVar# a s1# of
    (# s2#, var# #) -> (# s2#, PrimRef var# #)

-- | Read value of @PrimRef@.
readPrimRef :: PrimMonad m => PrimRef m a -> m a
readPrimRef (PrimRef var#)
  = primitive $ \s1# -> readMutVar# var# s1#

-- | Write value to @PrimRef@.
writePrimRef :: PrimMonad m => PrimRef m a -> a -> m ()
writePrimRef (PrimRef var#) a
  = primitive $ \s1# ->
      case writeMutVar# var# a s1# of
        s2# -> (# s2#, () #)

-- | Modify content of @PrimRef@ using function.
modifyPrimRef :: PrimMonad m => PrimRef m a -> (a -> a) -> m ()
modifyPrimRef var f = do
  x <- readPrimRef var
  writePrimRef var $ f x

-- | Modify content of @PrimRef@ using function and evaluate result of
--   function application to WHNF before storing it in the variable.
modifyPrimRef' :: PrimMonad m => PrimRef m a -> (a -> a) -> m ()
modifyPrimRef' var f = do
  x <- readPrimRef var
  writePrimRef var $! f x
