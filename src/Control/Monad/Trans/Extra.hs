module Control.Monad.Trans.Extra where

import Control.Monad.Trans.Maybe

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return
