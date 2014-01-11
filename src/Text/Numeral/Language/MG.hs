{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        mg

[@ISO639-2@]        mlg

[@ISO639-3@]        mlg

[@Native name@]     -

[@English name@]    Malagasy
-}

module Text.Numeral.Language.MG
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( ($), const, fix )
import "base" Data.List     ( map )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.Ord      ( (<) )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.Exp as E
import           "this" Text.Numeral.Grammar ( Inflection )
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry


-------------------------------------------------------------------------------
-- MG
-------------------------------------------------------------------------------

entry ∷ (Monoid s, IsString s) ⇒ Entry s
entry = emptyEntry
    { entIso639_1    = Just "mg"
    , entIso639_2    = ["mlg"]
    , entIso639_3    = Just "mlg"
    , entEnglishName = Just "Malagasy"
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
       $ findRule (0, lit)
                  [(n, step n 10 L L) | n ← map dec [1..6]]
                  (dec 7 - 1)

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (0, dec 7 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               }
    where
      (_ ⊞              Lit 10 ) _ = " ambin'ny "
      (_ ⊞ (Lit _ `Mul` Lit 10)) _ = " amby "
      (_ ⊞ _                   ) _ = " sy "

      (_ ⊡ Lit 10 ) _ = ""
      (_ ⊡ Lit 100) _ = ""
      (_ ⊡ _      ) _ = " "

      syms =
          M.fromList
          [ (0, const "haotra")
          , (1, \c → case c of
                       CtxAdd {} → "iraika"
                       _         → "iray"
            )
          , (2, mulForms "roa"    "roa"   "roan")
          , (3, mulForms "telo"   "telo"  "telon")
          , (4, mulForms "efatra" "efa"   "efa"  )
          , (5, mulForms "dimy"   "dimam" "diman")
          , (6, mulForms "enina"  "enim"  "enin" )
          , (7, mulForms "fito"   "fito"  "fiton")
          , (8, mulForms "valo"   "valo"  "valon")
          , (9, mulForms "sivy"   "sivi"  "sivin")
          , (10, \c → case c of
                        CtxMul _ (Lit n) _
                            | n < 9 → "polo"
                        _           → "folo"
            )
          , (100, \c → case c of
                         CtxMul {} → "jato"
                         _         → "zato"
            )
          , (1000, const "arivo")
          , (dec 4, const "alina")
          , (dec 5, const "hetsy")
          , (dec 6, const "tapitrisa")
          ]

      mulForms o t h = \c → case c of
                              CtxMul _ (Lit 10)  _ → t
                              CtxMul _ (Lit 100) _ → h
                              _                    → o
