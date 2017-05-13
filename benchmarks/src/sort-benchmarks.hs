module Main where

import           Data.Sort


main :: IO ()
main = print $ (groupSort (:) [10,9..1] :: [[Int]])
