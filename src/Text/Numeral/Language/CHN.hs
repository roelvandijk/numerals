{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2B@]       chn

[@ISO639-3@]        chn

[@Native name@]     chinuk wawa

[@English name@]    Chinook Jargon
-}

module Text.Numeral.Language.CHN
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.Exp as E
import           "this" Text.Numeral.Grammar ( Inflection )
import "this" Text.Numeral.Entry


--------------------------------------------------------------------------------
-- CHN
--------------------------------------------------------------------------------

entry ∷ (Monoid s, IsString s) ⇒ Entry s
entry = emptyEntry
    { entIso639_2    = ["chn"]
    , entIso639_3    = Just "chn"
    , entNativeNames = ["chinuk wawa"]
    , entEnglishName = Just "Chinook Jargon"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β) ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (  0, lit           )
                [ ( 11, step 10 10 R L)
                , (100, lit           )
                ]
                   100

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (0, 100)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprAdd   = Just $ \_ _ _ → " pe "
               , reprMul   = Just $ \_ _ _ → " "
               }
    where
      syms =
          M.fromList
          [ (1, const "ikt")
          , (2, const "mokst")
          , (3, const "klone")
          , (4, const "lakit")
          , (5, const "kwinnum")
          , (6, const "taghum")
          , (7, const "sinamokst")
          , (8, const "stotekin")
          , (9, const "kwaist")
          , (10, const "tahtlum")
          , (100, const "tukamonuk")
          ]
