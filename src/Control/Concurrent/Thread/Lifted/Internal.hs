{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.Thread.Lifted.Internal where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Thread (Result)
import Control.Monad.Base
import Control.Monad.Trans.Control

fixTypes :: MonadBaseControl IO m
         => (ThreadId, IO (Result (StM m a)))
         -> IO (ThreadId, m (Result a))
{-# INLINE fixTypes #-}
fixTypes = pure . fmap (\c -> liftBase c >>= traverse restoreM)
