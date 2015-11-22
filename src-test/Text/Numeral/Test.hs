{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Text.Numeral.Test ( TestData ) where

import "base" Data.String ( String )
import "numerals" Text.Numeral.Grammar ( Inflection )
import "text" Data.Text ( Text )

type TestData n = [(String, Inflection, [(n, Text)])]
