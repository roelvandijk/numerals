{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        fr

[@ISO639-2B@]       fre

[@ISO639-3@]        fra

[@Native name@]     Français

[@English name@]    French
-}

module Text.Numeral.Language.FR
    ( -- * Conversions
      cardinal
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
import "base" Data.Monoid   ( Monoid )
import "base" Data.Ord      ( (<) )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode ( (≤), (≥) )
import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.BigNum as BN
import qualified "numerals-base" Text.Numeral.Exp    as E


--------------------------------------------------------------------------------
-- FR
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/french.html
  http://www.french-linguistics.co.uk/tutorials/numbers/
  http://www.parisbypod.com/2007/10/23/french-ordinal-numbers/
-}

cardinal ∷ (Integral α, E.Scale α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ cardinalStruct

ordinal ∷ (Integral α, E.Scale α, Monoid s, IsString s) ⇒ i → α → Maybe s
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

genericRepr ∷ (Monoid s, IsString s) ⇒ Repr i s
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

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render genericRepr
               { reprValue = \_ n → M.lookup n syms
               , reprScale = BN.pelletierRepr (BN.quantityName "illion"  "illions")
                                              (BN.quantityName "illiard" "illiards")
                                              bigNumSyms
               }
    where
      syms =
          M.fromList
          [ (0, const "zéro")
          , (1, ten   "un"     "on"     "un")
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

ordinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
ordinalRepr = render genericRepr
              { reprValue = \_ n → M.lookup n syms
              , reprScale = BN.pelletierRepr ( BN.ordQuantityName "illion"  "illionième"
                                                                  "illions" "illionième"
                                             )
                                             ( BN.ordQuantityName "illiard"  "illiardième"
                                                                  "illiards" "illiardième"
                                             )
                                             bigNumSyms
              }
    where
      syms =
          M.fromList
          [ (0, \c → case c of
                       CtxEmpty → "zéroth"
                       _        → "zéro"
            )
          , (1, \c → case c of
                       CtxEmpty → "premier"
                       CtxAdd _ (Lit 10) _ → "on"
                       CtxMul _ (Lit 10) _ → "un"
                       _                   | isOutside R c → "unième"
                                           | otherwise     → "un"
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

bigNumSyms ∷ (IsString s) ⇒ [(ℤ, Ctx (Exp i) → s)]
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
