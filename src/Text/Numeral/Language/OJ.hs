{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        oj

[@ISO639-2@]        oji

[@ISO639-3@]        oji

[@Native name@]     ᐊᓂᔑᓈᐯᒧᐎᓐ (Anishinaabemowin)

[@English name@]    Ojibwe
-}

module Text.Numeral.Language.OJ
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
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.Exp as E
import           "numerals-base" Text.Numeral.Grammar ( Inflection )
import "this" Text.Numeral.Entry


--------------------------------------------------------------------------------
-- OJ
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/ojibwa.html
  http://www.languagesandnumbers.com/how-to-count-in-ojibwa/en/oji/
-}

entry ∷ (Monoid s, IsString s) ⇒ Entry s
entry = emptyEntry
    { entIso639_1    = Just "oj"
    , entIso639_2    = ["oji"]
    , entIso639_3    = Just "oji"
    , entNativeNames = ["ᐊᓂᔑᓈᐯᒧᐎᓐ", "Anishinaabemowin"]
    , entEnglishName = Just "Ojibwe"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β)
       ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (   1, lit             )
                [ (  11, step   10 10 R L)
                , ( 100, step  100 10 R L)
                , (1000, step 1000  2 R L)
                ]
                   1999

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (1, 1999)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprAdd   = Just $ \_ _ _ → " shaa "
               , reprMul   = Just $ \_ _ _ → ""
               }
    where
      syms =
          M.fromList
          [ (0, const "kaagego"                              )
          , (1, const "bezhik"                               )
          , (2, const "niizh"                                )
          , (3, forms "nswi"        "nsim"        "ns"       )
          , (4, forms "niiwin"      "niim"        "nii"      )
          , (5, forms "naanan"      "naanmi"      "naan"     )
          , (6, forms "ngodwaaswi"  "ngodwaasmi"  "ngodwaas" )
          , (7, forms "niizhwaaswi" "niizhwaasmi" "niizhwaas")
          , (8, forms "nshwaaswi"   "nshwaasmi"   "nshwaas"  )
          , (9, forms "zhaangswi"   "zhaangsmi"   "zhaangs"  )
          , (10, \c → case c of
                        CtxMul {} → "taana"
                        _         → "mdaaswi"
            )
          , (100, \c → case c of
                         CtxMul {} → "waak"
                         _         → "ngodwaak"
            )
          , (1000, const "mdaaswaak")
          ]

      forms ∷ s → s → s → Ctx (Exp i) → s
      forms o t h = \c → case c of
                           CtxMul _ (Lit 10)  _ → t
                           CtxMul _ (Lit 100) _ → h
                           _                    → o
