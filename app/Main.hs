module Main where

import           Text.Printf (printf)

main :: IO ()
main = printf "2 + 3 = %d\n" (2 + 3 :: Int)
