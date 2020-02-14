module Control.Monad.Trans.Extra where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . return
