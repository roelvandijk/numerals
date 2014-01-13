{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Text.Numeral.Entry
    ( Conversion(..)
    , Entry(..)
    , emptyEntry
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Maybe  ( Maybe(Nothing) )
import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import "this" Text.Numeral.Exp.Reified ( Exp )
import "this" Text.Numeral.Grammar.Reified ( Inflection )
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- Language entry
--------------------------------------------------------------------------------

data Conversion α = Conversion
    { toNumeral   ∷ Inflection → α → Maybe Text
    , toStructure ∷ α → Exp Inflection
    }

data Entry = Entry
    { entIso639_1       ∷ Maybe Text
    , entIso639_2       ∷ [Text]
    , entIso639_3       ∷ Maybe Text
    , entNativeNames    ∷ [Text]
    , entEnglishName    ∷ Maybe Text
    , entVariant        ∷ Maybe Text
    , entCardinal       ∷ Maybe (Conversion ℤ)
    , entOrdinal        ∷ Maybe (Conversion ℤ)
    , entPartitive      ∷ Maybe (Conversion (ℤ, ℤ))
    , entMultiplicative ∷ Maybe (Conversion ℤ)
    }

emptyEntry ∷ Entry
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
