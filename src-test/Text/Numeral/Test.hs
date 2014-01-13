{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Text.Numeral.Test ( TestData ) where

import "base" Data.String ( String )
import "this" Text.Numeral.Grammar.Reified ( Inflection )
import "text" Data.Text ( Text )

type TestData n = [(String, Inflection, [(n, Text)])]
