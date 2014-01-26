{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        fr

[@ISO639-2B@]       fre

[@ISO639-3@]        fra

[@Native name@]     Français

[@English name@]    French
-}

module Text.Numeral.Language.FRA
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
    , ordinal
      -- * Structure
    , cardinalStruct
    , ordinalStruct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Ord      ( (<) )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode ( (≤), (≥) )
import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum  as BN
import qualified "this" Text.Numeral.Exp     as E
import qualified "this" Text.Numeral.Grammar as G
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- FRA
--------------------------------------------------------------------------------

entry ∷ Entry
entry = emptyEntry
    { entIso639_1    = Just "fr"
    , entIso639_2    = ["fre"]
    , entIso639_3    = Just "fra"
    , entNativeNames = ["Français"]
    , entEnglishName = Just "French"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = cardinalStruct
                       }
    , entOrdinal     = Just Conversion
                       { toNumeral   = ordinal
                       , toStructure = ordinalStruct
                       }
    }

cardinal ∷ (G.Feminine i, Integral α, E.Scale α) ⇒ i → α → Maybe Text
cardinal inf = cardinalRepr inf ∘ cardinalStruct

ordinal ∷ (G.Feminine i, Integral α, E.Scale α) ⇒ i → α → Maybe Text
ordinal inf = ordinalRepr inf ∘ ordinalStruct

cardinalStruct ∷ ( Integral α, E.Scale α
                 , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
                 )
               ⇒ α → β
cardinalStruct = pos $ fix $ rule `combine` pelletierScale1 R L BN.rule

ordinalStruct ∷ ( Integral α, E.Scale α
                , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
                )
              ⇒ α → β
ordinalStruct = pos $ fix $ rule `combine` pelletierScale R L BN.rule

rule ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β) ⇒ Rule α β
rule = findRule (   0, lit         )
              [ (  11, add   10 L  )
              , (  17, add   10 R  )
              , (  20, lit         )
              , (  21, add   20 R  )
              , (  30, mul   10 R L)
              , (  70, add   60 R  )
              , (  80, mul   20 R L)
              , (  89, add   80 R  )
              , ( 100, step  100   10 R L)
              , (1000, step 1000 1000 R L)
              ]
                (dec 6 - 1)

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 60000 - 1 in (negate x, x)

genericRepr ∷ Repr i
genericRepr = defaultRepr
              { reprAdd   = Just (⊞)
              , reprMul   = Just (⊡)
              , reprNeg   = Just $ \_ _ → "moins "
              }
    where
      (Lit n                ⊞ Lit 10) _       | n ≤ 6 = ""
      (Lit 10               ⊞ Lit n ) _       | n ≥ 7 = "-"
      ((Lit 4 `Mul` Lit 20) ⊞ _     ) _               = "-"
      (_                    ⊞ (Lit 1 `Add` Lit 10)) _ = " et "
      (_                    ⊞ Lit 1               ) _ = " et "
      ((Lit _ `Mul` Lit 10) ⊞ _                   ) _ = "-"
      (Lit 20               ⊞ _                   ) _ = "-"
      (_                    ⊞ _                   ) _ = " "

      (_ ⊡ Lit 10) _ = ""
      (_ ⊡ Lit 20) _ = "-"
      (_ ⊡ _     ) _ = " "

cardinalRepr ∷ (G.Feminine i) ⇒ i → Exp i → Maybe Text
cardinalRepr = render genericRepr
               { reprValue = \inf n → M.lookup n (syms inf)
               , reprScale = BN.pelletierRepr (BN.quantityName "illion"  "illions")
                                              (BN.quantityName "illiard" "illiards")
                                              bigNumSyms
               }
    where
      syms inf =
          M.fromList
          [ (0, const "zéro")
          , (1, \c → case c of
                       CtxAdd _ (Lit 10) _  → "on"
                       _ | G.isFeminine inf → "une"
                         | otherwise        → "un"
            )
          , (2, ten   "deux"   "dou"    "deux")
          , (3, ten   "trois"  "trei"   "tren")
          , (4, ten   "quatre" "quator" "quar")
          , (5, ten   "cinq"   "quin"   "cinqu")
          , (6, ten   "six"    "sei"    "soix")
          , (7, const "sept")
          , (8, const "huit")
          , (9, const "neuf")
          , (10, \c → case c of
                        CtxAdd _ (Lit n) _ | n < 7     → "ze"
                                           | otherwise → "dix"
                        CtxMul _ (Lit 3) _             → "te"
                        CtxMul R _       _             → "ante"
                        _                              → "dix"
            )
          , (20, \c → case c of
                        CtxMul _ _ CtxEmpty → "vingts"
                        _                   → "vingt"
            )
          , (100, \c → case c of
                         CtxMul R _ CtxEmpty → "cents"
                         _                   → "cent"
            )
          , (1000, const "mille")
          ]

      ten n a m ctx = case ctx of
                        CtxAdd _ (Lit 10) _ → a
                        CtxMul _ (Lit 10) _ → m
                        _                   → n

ordinalRepr ∷ (G.Feminine i) ⇒ i → Exp i → Maybe Text
ordinalRepr = render genericRepr
              { reprValue = \inf n → M.lookup n (syms inf)
              , reprScale = BN.pelletierRepr ( BN.ordQuantityName "illion"  "illionième"
                                                                  "illions" "illionième"
                                             )
                                             ( BN.ordQuantityName "illiard"  "illiardième"
                                                                  "illiards" "illiardième"
                                             )
                                             bigNumSyms
              }
    where
      syms inf =
          M.fromList
          [ (0, \c → case c of
                       CtxEmpty → "zéroth"
                       _        → "zéro"
            )
          , (1, \c → case c of
                       CtxEmpty
                         | G.isFeminine inf → "première"
                         | otherwise        → "premier"
                       CtxAdd _ (Lit 10) _  → "on"
                       _ | isOutside R c    → if G.isFeminine inf
                                              then "uneième"
                                              else "unième"
                         | G.isFeminine inf → "une"
                         | otherwise        → "un"
            )
          , (2, ten   "deuxième"  "deux"   "dou"    "deux")
          , (3, ten   "troisième" "trois"  "trei"   "tren")
          , (4, ten   "quatrième" "quatre" "quator" "quar")
          , (5, ten   "cinquième" "cinq"   "quin"   "cinqu")
          , (6, ten   "sixième"   "six"    "sei"    "soix")
          , (7, \c → if isOutside R c then "septième" else "sept")
          , (8, \c → if isOutside R c then "huitième" else "huit")
          , (9, \c → if isOutside R c then "neuvième" else "neuf")
          , (10, \c → case c of
                        CtxAdd R (Lit _) _ | isOutside R c → "zième"
                                           | otherwise     → "ze"
                        CtxAdd L (Lit _) _                 → "dix"
                        CtxMul _ (Lit 3) _ | isOutside R c → "tième"
                                           | otherwise     → "te"
                        CtxMul R _       _ | isOutside R c → "antième"
                                           | otherwise     → "ante"
                        _                  | isOutside R c → "dixième"
                                           | otherwise     → "dix"
            )
          , (20, \c → case c of
                        _ | isOutside R c → "vingtième"
                        CtxMul _ _ CtxEmpty → "vingts"
                        _                   → "vingt"
            )
          , (100, \c → case c of
                         _ | isOutside R c   → "centième"
                         CtxMul R _ CtxEmpty → "cents"
                         _                   → "cent"
            )
          , (1000, \c → if isOutside R c then "millième" else "mille")
          ]

      ten o n a m ctx =
          case ctx of
            CtxAdd _ (Lit 10) _ | isOutside R ctx → o
                                | otherwise       → a
            CtxMul _ (Lit 10) _ | isOutside R ctx → o
                                | otherwise       → m
            _                   | isOutside R ctx → o
                                | otherwise       → n

bigNumSyms ∷ [(ℤ, Ctx (Exp i) → Text)]
bigNumSyms =
    [ (1, BN.forms "m"  "uno" "uno"  ""    "")
    , (3, BN.forms "tr" "tré" "tres" "tri" "tre")
    , (10, \c → case c of
                  CtxAdd _ (Lit 100) _              → "deci"
                  CtxMul _ _ (CtxAdd _ (Lit 100) _) → "ginta"
                  CtxMul {}                         → "gint"
                  _                                 → "déc"
      )
    ]
