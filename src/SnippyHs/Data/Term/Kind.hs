{-# LANGUAGE DeriveDataTypeable #-}

module SnippyHs.Data.Term.Kind (Kind (..), applyKindM) where

import Data.Data (Data)

data Kind = Star | KFun Kind Kind
  deriving (Eq, Data)

instance Show Kind where
  show Star = "*"
  show (KFun k1 k2) =
    "(" ++ show k1 ++ " -> " ++ show k2 ++ ")"

applyKindM :: (MonadFail m) => Kind -> Kind -> m Kind
applyKindM Star k = fail $ "kind check failed: trying apply kind (*) to " ++ show k
applyKindM (KFun k1 k2) k =
  if k == k1
    then pure k2
    else
      fail $
        "kind check failed: applying "
          ++ show (KFun k1 k2)
          ++ " to "
          ++ show k
