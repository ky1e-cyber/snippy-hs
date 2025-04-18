{-# LANGUAGE DeriveFunctor #-}

module Control.Monad.Crash (Crash (..)) where

import Control.Monad (ap)

newtype Crash a = Crash {runCrash :: a} deriving (Functor)

instance Applicative Crash where
  (<*>) = ap
  pure = Crash

instance Monad Crash where
  return = pure
  Crash x >>= f = f x

instance MonadFail Crash where
  fail = error
