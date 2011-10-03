{-# LANGUAGE NoImplicitPrelude, PackageImports, UnicodeSyntax #-}

module Text.Numeral.Test ( TestData ) where

import "numerals-base" Text.Numeral.Grammar.Reified ( Inflection )

type TestData n s = [(s, Inflection, [(n, s)])]
