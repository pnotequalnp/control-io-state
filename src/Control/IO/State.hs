-- |
-- Module      : Control.IO.State
-- Description : Stateful Operations with IORefs
-- Copyright   : Kevin Mullins 2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
--
-- = IO State
-- This module provides a @MonadState@-like interface for @IORef@s with implicit parameters.
module Control.IO.State (
  IOState,
  runIOState,
  evalIOState,
  execIOState,
  get,
  put,
  modify,
  modify',
  gets,
  withState,
  withState',
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (IORef, modifyIORef, modifyIORef', newIORef, readIORef, writeIORef)

type IOState s = ?ioStateRef :: IORef s

runIOState :: MonadIO m => s -> (IOState s => m a) -> m (a, s)
runIOState x action = do
  ioStateRef <- liftIO (newIORef x)
  let ?ioStateRef = ioStateRef
  result <- action
  x' <- get
  pure (result, x')
{-# INLINEABLE runIOState #-}

evalIOState :: MonadIO m => s -> (IOState s => m a) -> m a
evalIOState x action = fst <$> runIOState x action
{-# INLINE evalIOState #-}

execIOState :: MonadIO m => s -> (IOState s => m a) -> m s
execIOState x action = snd <$> runIOState x action
{-# INLINE execIOState #-}

get :: (IOState s, MonadIO m) => m s
get = liftIO (readIORef ?ioStateRef)
{-# INLINE get #-}

put :: (IOState s, MonadIO m) => s -> m ()
put = liftIO . writeIORef ?ioStateRef
{-# INLINE put #-}

modify :: (IOState s, MonadIO m) => (s -> s) -> m ()
modify = liftIO . modifyIORef ?ioStateRef
{-# INLINE modify #-}

modify' :: (IOState s, MonadIO m) => (s -> s) -> m ()
modify' = liftIO . modifyIORef' ?ioStateRef
{-# INLINE modify' #-}

gets :: (IOState s, MonadIO m) => (s -> a) -> m a
gets f = f <$> get
{-# INLINE gets #-}

withState :: (IOState s, MonadIO m) => (s -> s) -> m a -> m a
withState f action = modify f *> action
{-# INLINE withState #-}

withState' :: (IOState s, MonadIO m) => (s -> s) -> m a -> m a
withState' f action = modify' f *> action
{-# INLINE withState' #-}
