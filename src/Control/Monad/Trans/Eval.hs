{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Monad.Trans.Eval where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

newtype EvalT s e m a = EvalT
  { runEvalT :: StateT s (ExceptT e m) a
  } deriving (Functor)

instance Monad m => Applicative (EvalT s e m) where
  pure = EvalT . pure
  (EvalT f) <*> (EvalT s) = EvalT (f <*> s)

instance Monad m => Monad (EvalT s e m) where
  (EvalT s) >>= f = EvalT (s >>= g)
    where
      g (f -> EvalT x) = x

instance MonadTrans (EvalT s e) where
  lift = EvalT . lift . lift

instance MonadIO m => MonadIO (EvalT s e m) where
  liftIO = EvalT . liftIO

instance (Monoid e, Monad m) => Alternative (EvalT s e m) where
  empty = EvalT empty
  EvalT s1 <|> EvalT s2 = EvalT (s1 <|> s2)

exceptEval :: Monad m => Either e a -> EvalT s e m a
exceptEval = EvalT . lift . except

liftExceptT :: Monad m => ExceptT e m a -> EvalT s e m a
liftExceptT = EvalT . lift

getEval :: Monad m => EvalT s e m s
getEval = EvalT get

modifyEval :: Monad m => (s -> s) -> EvalT s e m ()
modifyEval = EvalT . modify

throwExceptEval :: Monad m => e -> EvalT s e m a
throwExceptEval = EvalT . lift . throwE
