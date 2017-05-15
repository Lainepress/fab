module Main where

import           Criterion.Main
import           Data.List
import           Data.Monoid
import           Data.Sort
import qualified Data.Vector          as V
import           System.Random.MWC


main :: IO ()
main = do

  gen <- initialize $ V.singleton 42
  r1k <- V.toList `fmap` uniformVector gen 1000 :: IO [Int]

  defaultMain
    [ bgroup "sort"
      [ bench "sort.r1k"              $ whnf sort                       r1k
      , bench "sort.a1k"              $ whnf sort                       a1k
      , bench "sort.d1k"              $ whnf sort                       d1k
      , bench "sortON.r1k"            $ whnf (sortON id)                r1k
      , bench "sortON.a1k"            $ whnf (sortON id)                a1k
      , bench "groupSort.r1k"         $ whnf (groupSort grp)            r1k
      , bench "groupSort.a1k"         $ whnf (groupSort grp)            a1k
      , bench "monoidSort.r1k"        $ whnf monoidSort       $ map Sum r1k
      , bench "monoidSort.a1k"        $ whnf monoidSort       $ map Sum a1k
      , bench "monoidSortAssocs.r1k"  $ whnf monoidSortAssocs $ map ass r1k
      , bench "monoidSortAssocs.a1k"  $ whnf monoidSortAssocs $ map ass a1k
      ]
    ]

a1k :: [Int]
a1k = [1..1000]

d1k :: [Int]
d1k = [1000,999..1]

grp :: Int -> [Int] -> (Int,Int)
grp x l = (x,1+length l)

ass :: Int -> (Int,())
ass = flip (,) ()
