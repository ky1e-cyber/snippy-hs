module Data.Enumeration
  ( Enumeration (values),
  )
where

import Data.Data
  ( Data,
    dataTypeConstrs,
    dataTypeOf,
    fromConstr,
  )

class (Data a) => Enumeration a where
  -- This is unholy
  values' :: a -> [a]
  values' x =
    map fromConstr $ dataTypeConstrs (dataTypeOf x)
  values :: [a]
  values = values' (undefined :: a)
