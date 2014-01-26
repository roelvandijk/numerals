{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax  #-}

module Main where

import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import "criterion" Criterion.Main
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import qualified "numerals" Text.Numeral.Language.NLD as NLD


main ∷ IO ()
main = defaultMain
       [bench "NLD" $ NLD.cardinal defaultInflection `nf` (10 ^ (42 ∷ ℤ) - 1 ∷ ℤ)]
