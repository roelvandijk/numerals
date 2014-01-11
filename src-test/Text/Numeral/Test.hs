{-# LANGUAGE NoImplicitPrelude, PackageImports, UnicodeSyntax #-}

module Text.Numeral.Test ( TestData ) where

import "this" Text.Numeral.Grammar.Reified ( Inflection )

type TestData n s = [(s, Inflection, [(n, s)])]
