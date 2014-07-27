module Halt where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

--------------------------------------------------------------------------------
newtype HaltT e m a
  = HaltT{ unHaltT :: m (Either e a) }

instance Monad m => Monad (HaltT e m) where
    HaltT m >>= f
      = HaltT $ m >>= \e -> case e of
            Left e' -> return (Left e')
            Right x -> unHaltT (f x)
    return x = HaltT (return (Right x))

instance Monad m => Applicative (HaltT e m) where
    mf <*> mx = do f <- mf; x <- mx; return (f x)
    pure      = return

instance Monad m => Functor (HaltT e m) where
    fmap f mx = do x <- mx; return (f x)

halt :: Monad m => e -> HaltT e m a
halt e = HaltT (return (Left e))

--------------------------------------------------------------------------------
instance MonadTrans (HaltT e) where
    lift m = HaltT (m >>= return . Right)

instance MonadIO m => MonadIO (HaltT e m) where
    liftIO = lift . liftIO
