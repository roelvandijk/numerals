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
    ( cardinal
    , struct
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
import "base" Prelude       ( Integral, (-), Integer )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode ( (≤), (≥) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.BigNum      as BN
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- FR
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/french.html
  http://www.french-linguistics.co.uk/tutorials/numbers/
-}

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = pos $ fix $ rule `combine` pelletierScale1 R L BN.rule
    where
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

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = pelletierRepr
               , reprAdd   = Just (⊞)
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

pelletierRepr ∷ (IsString s, Monoid s)
              ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
pelletierRepr = BN.pelletierRepr
                  "illion" "illions"
                  "illiard" "illiards"
                  [ (1, BN.forms "m"  "uno" "uno"  ""    "")
                  , (3, BN.forms "tr" "tré" "tres" "tri" "tre")
                  , (10, \c → case c of
                                CtxAdd _ (Lit 100) _              → "deci"
                                CtxMul _ _ (CtxAdd _ (Lit 100) _) → "ginta"
                                CtxMul {}                         → "gint"
                                _                                 → "déc"
                    )
                  ]
