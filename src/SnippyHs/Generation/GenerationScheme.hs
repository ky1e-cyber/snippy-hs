{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SnippyHs.Generation.GenerationScheme
  ( GenerationScheme (..),
    ExprGenScheme (..),
    TypeGenScheme (..),
  )
where

import Data.Data (Data)
import Data.Enumeration (Enumeration (..))

class (Eq s, Enumeration s) => GenerationScheme s where
  allSchemes :: [s]
  allSchemes = values

data ExprGenScheme
  = ExprApp
  | LambdaAbstr
  | LiteralInsertion
  | SymbolInsertion
  deriving
    ( Show,
      Eq,
      Data,
      Enumeration,
      GenerationScheme
    )

data TypeGenScheme
  = TypeApp
  | TypeVarInsertion
  | TypeConstInsertion
  deriving
    ( Show,
      Eq,
      Data,
      Enumeration,
      GenerationScheme
    )
