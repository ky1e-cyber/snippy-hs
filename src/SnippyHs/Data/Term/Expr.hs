{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module SnippyHs.Data.Term.Expr
  ( Expr (..),
  )
where

data Expr i
  = EName i
  | ELambda [i] (Expr i)
  | App (Expr i) (Expr i)
  deriving (Eq, Functor, Foldable)
