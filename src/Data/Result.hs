module Data.Result (Result (..)) where

import Control.Monad (ap, liftM)
import Data.String (IsString, fromString)

data Result e a = Err e | Ok a
  deriving (Show)

instance Functor (Result e) where
  fmap = liftM

instance Applicative (Result e) where
  pure = Ok
  (<*>) = ap

instance Monad (Result e) where
  return = pure
  Err err >>= _ = Err err
  Ok val >>= g = g val

instance (IsString e) => MonadFail (Result e) where
  fail s = Err $ fromString s
