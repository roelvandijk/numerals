module Main where

import "criterion" Criterion.Main
import "numerals" Text.Numeral.Grammar ( defaultInflection )
import qualified "numerals" Text.Numeral.Language.NLD as NLD


main :: IO ()
main = defaultMain
       [bench "NLD" $ NLD.cardinal defaultInflection `nf` (10 ^ (42 :: Integer) - 1 :: Integer)]
