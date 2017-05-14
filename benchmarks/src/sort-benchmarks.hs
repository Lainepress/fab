module Main where

import           Criterion.Main
import           Data.Sort
import qualified Data.Vector          as V
import           System.Random.MWC


main :: IO ()
main = do

  gen     <- initialize $ V.singleton 42
  rnd1000 <- V.toList <$> uniformVector gen 1000 :: IO [Int]

  defaultMain [
    bgroup "sort"
      [ bench "sort.rnd1000"    $ whnf sort        rnd1000
      , bench "sort.asc1000"    $ whnf sort        asc1000
      , bench "sortOn.rnd1000"  $ whnf (sortOn id) rnd1000
      , bench "sortOn.asc1000"  $ whnf (sortOn id) rnd1000
      ]
    ]

asc1000 :: [Int]
asc1000 = [1..1000]
