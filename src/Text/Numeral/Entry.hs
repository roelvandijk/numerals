{-# LANGUAGE NoImplicitPrelude
           , PackageImports
           , UnicodeSyntax
  #-}

module Text.Numeral.Entry
    ( Conversion(..)
    , Entry(..)
    , emptyEntry
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Maybe  ( Maybe(Nothing) )
import "base" Data.String ( String )
import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import "this" Text.Numeral.Exp.Reified ( Exp )
import "this" Text.Numeral.Grammar.Reified ( Inflection )


--------------------------------------------------------------------------------
-- Language entry
--------------------------------------------------------------------------------

data Conversion α s = Conversion
    { toNumeral   ∷ Inflection → α → Maybe s
    , toStructure ∷ α → Exp Inflection
    }

data Entry s = Entry
    { entIso639_1       ∷ Maybe String
    , entIso639_2       ∷ [String]
    , entIso639_3       ∷ Maybe String
    , entNativeNames    ∷ [String]
    , entEnglishName    ∷ Maybe String
    , entVariant        ∷ Maybe String
    , entCardinal       ∷ Maybe (Conversion ℤ s)
    , entOrdinal        ∷ Maybe (Conversion ℤ s)
    , entPartitive      ∷ Maybe (Conversion (ℤ, ℤ) s)
    , entMultiplicative ∷ Maybe (Conversion ℤ s)
    }

emptyEntry ∷ Entry s
emptyEntry = Entry
    { entIso639_1       = Nothing
    , entIso639_2       = []
    , entIso639_3       = Nothing
    , entNativeNames    = []
    , entEnglishName    = Nothing
    , entVariant        = Nothing
    , entCardinal       = Nothing
    , entOrdinal        = Nothing
    , entPartitive      = Nothing
    , entMultiplicative = Nothing
    }
