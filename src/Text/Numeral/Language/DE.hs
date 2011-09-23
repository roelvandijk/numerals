{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        de

[@ISO639-2B@]       ger

[@ISO639-3@]        deu

[@Native name@]     Deutsch

[@English name@]    German
-}

module Text.Numeral.Language.DE
    ( cardinal
    , ordinal
    , struct
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.List.Unicode     ( (∈) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤), (≥) )
import "base-unicode-symbols" Prelude.Unicode       ( ℤ )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.BigNum      as BN
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- DE
-------------------------------------------------------------------------------

{-
Sources:
  http://de.wikipedia.org/wiki/Zahlennamen
  http://german.about.com/library/blzahlen.htm
-}

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

ordinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
ordinal = ordinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = pos
       $ fix
       $ findRule (   0, lit       )
               [ (  13, add 10 L  )
               , (  20, mul 10 L L)
               , ( 100, step 100    10 R L)
               , (1000, step 1000 1000 R L)
               ]
                 (dec 6 - 1)
         `combine` pelletierScale R L BN.rule

genericRepr ∷ (Monoid s, IsString s) ⇒ Repr s
genericRepr = defaultRepr
              { reprAdd   = Just (⊞)
              , reprMul   = Just (⊡)
              , reprNeg   = Just $ \_ _   → "minus "
              }
    where
      (Lit n ⊞ (_ `Mul` Lit 10)) _ | n ≤ 9 = "und"
      (_ ⊞ _ ) _ = ""

      (_ ⊡ Scale _ _ _) _ = " "
      (_ ⊡ _)           _ = ""


cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render genericRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = BN.pelletierRepr (BN.quantityName "illion"   "illionen")
                                              (BN.quantityName "illiarde" "illiarden")
                                              bigNumSyms
               }
    where
      syms =
          M.fromList
          [ (0, const "null")
          , (1, \c → case c of
                       CtxAdd R _ _     → "eins"
                       CtxAdd L _ _     → "ein"
                       CtxMul _ (Lit n) _
                           | n ≥ dec 6  → "eine"
                           | n ≥ 100    → "ein"
                       _                → "eins"
            )
          , (2, \c → case c of
                       CtxMul _ (Lit 10) _ → "zwan"
                       _                   → "zwei"
            )
          , (3, const "drei")
          , (4, const "vier")
          , (5, const "fünf")
          , (6, \c → case c of
                       CtxAdd _ (Lit 10) _ → "sech"
                       CtxMul _ (Lit 10) _ → "sech"
                       _                   → "sechs"
            )
          , (7, \c → case c of
                       CtxAdd _ (Lit 10) _ → "sieb"
                       CtxMul _ (Lit 10) _ → "sieb"
                       _                   → "sieben"
            )
          , (8, const "acht")
          , (9, const "neun")
          , (10, \c → case c of
                        CtxMul _ (Lit 3) _ → "ßig"
                        CtxMul R (Lit _) _ → "zig"
                        _                  → "zehn"
            )
          , (11, const "elf")
          , (12, const "zwölf")
          , (100, const "hundert")
          , (1000, const "tausend")
          ]

ordinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
ordinalRepr = render genericRepr
              { reprValue = \n → M.lookup n syms
              , reprScale = BN.pelletierRepr (BN.ordQuantityName "illion" "illionste"
                                                                 "illion" "illionste"
                                             )
                                             (BN.ordQuantityName "illiarde" "illiardste"
                                                                 "illiarde" "illiardste"
                                             )
                                             bigNumSyms
              }
    where
      syms =
          M.fromList
          [ (0, \c → case c of
                       CtxEmpty → "nullte"
                       _        → "null"
            )
          , (1, \c → case c of
                       _ | isOutside R c → "erste"
                       CtxAdd {}         → "ein"
                       CtxMul _ (Lit n) _
                           | n ≥ dec 6   → "eine"
                           | n ≥ 100     → "ein"
                       _                 → "eins"
            )
          , (2, \c → case c of
                       _ | isOutside R c   → "zweite"
                       CtxMul _ (Lit 10) _ → "zwan"
                       _                   → "zwei"
            )
          , (3, \c → if isOutside R c then "dritte" else "drei")
          , (4, \c → if isOutside R c then "vierte" else "vier")
          , (5, \c → if isOutside R c then "fünfte" else "fünf")
          , (6, \c → case c of
                       _ | isOutside R c   → "sechste"
                       CtxAdd _ (Lit 10) _ → "sech"
                       CtxMul _ (Lit 10) _ → "sech"
                       _                   → "sechs"
            )
          , (7, \c → case c of
                       _ | isOutside R c   → "siebte"
                       CtxAdd _ (Lit 10) _ → "sieb"
                       CtxMul _ (Lit 10) _ → "sieb"
                       _                   → "sieben"
            )
          , (8, \c → if isOutside R c then "achte"  else "acht")
          , (9, \c → if isOutside R c then "neunte" else "neun")
          , (10, \c → case c of
                        CtxMul _ (Lit 3) _ | isOutside R c → "ßigste"
                                           | otherwise     → "ßig"
                        CtxMul R (Lit _) _ | isOutside R c → "zigste"
                                           | otherwise     → "zig"
                        _                  | isOutside R c → "zehnte"
                                           | otherwise     → "zehn"
            )
          , (11, \c → if isOutside R c then "elfte"   else "elf")
          , (12, \c → if isOutside R c then "zwölfte" else "zwölf")
          , (100, \c → if isOutside R c then "hundertste" else "hundert")
          , (1000, \c → if isOutside R c then "tausendste" else "tausend")
          ]

bigNumSyms ∷ (IsString s) ⇒ [(ℤ, Ctx Exp → s)]
bigNumSyms =
    [ (8, BN.forms "okt" "okto" "okto" "okto" "oktin")
    , (10, \c → case c of
                  CtxAdd _ (Lit 100) _              → "dezi"
                  CtxMul _ _ (CtxAdd _ (Lit 100) _) → "ginta"
                  CtxMul {}                         → "gint"
                  _                                 → "dez"
      )
    , (100, \c → case c of
                   CtxMul _ (Lit n) _
                       | n ∈ [2,3,6] → "zent"
                       | otherwise   → "gent"
                   _                 → "zent"
      )
    ]
