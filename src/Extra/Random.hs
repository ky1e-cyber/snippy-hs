module Extra.Random
  ( shuffle,
    coinflip,
  )
where

import Data.List (nub)
import System.Random
  ( RandomGen,
    SplitGen,
    random,
    splitGen,
    uniformRs,
  )

shuffle :: (SplitGen g) => [a] -> g -> ([a], g)
shuffle xs gen0 =
  let l = length xs
      (gen, gen') = splitGen gen0
      inds = take l $ nub $ uniformRs (0, l - 1) gen'
   in ([xs !! i | i <- inds], gen)

coinflip :: (RandomGen g) => g -> (Bool, g)
coinflip = random
