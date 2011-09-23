{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        it

[@ISO639-2B@]       ita

[@ISO639-3@]        ita

[@Native name@]     Italiano

[@English name@]    Italian
-}

module Text.Numeral.Language.IT
    ( cardinal
    , ordinal
    , struct
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.List.Unicode     ( (∈) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≥) )
import "base-unicode-symbols" Prelude.Unicode       ( ℤ )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.BigNum      as BN
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- IT
--------------------------------------------------------------------------------

-- Sources:
--   http://www.sf.airnet.ne.jp/~ts/language/number/italian.html
--   http://www.orbilat.com/Languages/Italian/Grammar/Italian-Numerals.html
--   http://italian.about.com/library/weekly/aa042600a.htm
--   http://www.suite101.com/content/how-to-count-in-italian-a146487

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

ordinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
ordinal = ordinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = pos $ fix $ rule `combine` pelletierScale R L BN.rule
    where
      rule = findRule (   0, lit         )
                    [ (  11, add   10 L  )
                    , (  17, add   10 R  )
                    , (  20, lit         )
                    , (  21, add   20 R  )
                    , (  30, mul   10 R L)
                    , ( 100, step  100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                      (dec 6 - 1)

genericRepr ∷ (Monoid s, IsString s) ⇒ Repr s
genericRepr = defaultRepr
              { reprAdd   = Just (⊞)
              , reprMul   = Just (⊡)
              , reprNeg   = Just $ \_ _ → "meno "
              }
    where
      (Lit 10                ⊞ Lit 7) _ = "as"
      (Lit 10                ⊞ Lit 9) _ = "an"
      ((_ `Mul` Scale _ _ _) ⊞ _    ) _ = " "
      (_                     ⊞ _    ) _ = ""


      (Lit n ⊡ Lit 10) _ | n ≥ 4 = "an"
      (_     ⊡ Scale _ _ _) _ = " "
      (_     ⊡ _          ) _ = ""

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render genericRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = BN.pelletierRepr
                               (BN.quantityName "ilione"  "ilioni")
                               (BN.quantityName "iliardo" "iliardi")
                               bigNumSyms
               }
    where
      syms =
          M.fromList
          [ (0, const "zero")
          , (1, forms "uno" "un" "uno")
          , (2, forms "due" "do" "du")
          , (3, \c → case c of
                       CtxAdd R _        _ → "tré"
                       CtxMul _ (Lit 10) _ → "tren"
                       _                   → "tre"
            )
          , (4, forms "quattro" "quattor" "quar")
          , (5, forms "cinque"  "quin"    "cinqu")
          , (6, forms "sei"     "se"      "sess")
          , (7, forms "sette"   "sette"   "sett")
          , (8, forms "otto"    "otto"    "ott")
          , (9, forms "nove"    "nove"    "nov")
          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ → "dici"
                        -- Last vowel removed because of a phonetic rule:
                        CtxMul _ (Lit _) (CtxAdd _ (Lit n) _)
                            | n ∈ [1,8]    → "t"
                        CtxMul R (Lit _) _ → "ta"
                        _                  → "dieci"
            )
          , (20, \c → case c of
                        CtxAdd _ (Lit n) _
                            | n ∈ [1,8]   → "vent"
                        _                 → "venti"
            )
          , ( 100
            , let f c = case c of
                          CtxAdd _ (Lit 8)                      _  → "cent"
                          CtxAdd _ (Lit 8 `Mul` Lit 10)         _  → "cent"
                          CtxAdd _ (Lit 8 `Mul` Lit 10 `Add` _) _  → "cent"
                          CtxMul _ (Lit _) c2 → f c2
                          _ → "cento"
              in f
            )
          , (1000, \c → case c of
                          CtxMul {} → "mila"
                          _         → "mille"
            )
          ]

      forms ∷ s → s → s → Ctx Exp → s
      forms n a m c = case c of
                        CtxAdd _ (Lit 10) _ → a
                        CtxMul _ (Lit 10) _ → m
                        _                   → n

ordinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
ordinalRepr = render genericRepr
              { reprValue = \n → M.lookup n syms
              , reprScale = BN.pelletierRepr
                              ( BN.ordQuantityName "ilione" "ilionesimo"
                                                   "ilioni" "ilionesimo"
                              )
                              ( BN.ordQuantityName "iliardo" "iliardesimo"
                                                   "iliardi" "iliardesimo"
                              )
                              bigNumSyms
              }
    where
      syms =
          M.fromList
          [ (0, const "zero")
          , (1, forms "primo"   "unesimo" "uno" "un" "uno")
          , (2, forms "secondo" "duesimo" "due" "do" "due")
          , (3, \c → case c of
                       CtxEmpty            → "terzo"
                       _ | isOutside R c   → "treesimo"
                       CtxAdd R _        _ → "tré"
                       CtxMul _ (Lit 10) _ → "tren"
                       _                   → "tre"
            )
          , (4, forms "quarto"  "quattresimo" "quattro" "quattor" "quar")
          , (5, forms "quinto"  "cinquesimo"  "cinque"  "quin"    "cinqu")
          , (6, forms "sesto"   "seiesimo"    "sei"     "se"      "sess")
          , (7, forms "settimo" "settesimo"   "sette"   "sette"   "sett")
          , (8, forms "ottavo"  "ottesimo"    "otto"    "otto"    "ott")
          , (9, forms "nono"    "novesimo"    "nove"    "nove"    "nov")
          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ | isOutside R c → "dicesimo"
                                           | otherwise     → "dici"
                        -- Last vowel removed because of a phonetic rule:
                        CtxMul _ (Lit _) (CtxAdd _ (Lit n) _)
                            | n ∈ [1,8]    → "t"
                        CtxMul R (Lit _) _ | isOutside R c → "tesimo"
                                           | otherwise     → "ta"
                        _                  | isOutside R c → "decimo"
                                           | otherwise     → "dieci"
            )
          , (20, \c → case c of
                        _ | isOutside R c  → "ventesimo"
                        CtxAdd _ (Lit n) _
                          | n ∈ [1,8]      → "vent"
                        _                  → "venti"
            )
          , ( 100
            , let f c = case c of
                          _ | isOutside R c                        → "centesimo"
                          CtxAdd _ (Lit 8)                      _  → "cent"
                          CtxAdd _ (Lit 8 `Mul` Lit 10)         _  → "cent"
                          CtxAdd _ (Lit 8 `Mul` Lit 10 `Add` _) _  → "cent"
                          CtxMul _ (Lit _) c2                      → f c2
                          _                                        → "cento"
              in f
            )
          , (1000, \c → case c of
                          _ | isOutside R c → "millesimo"
                          CtxMul {}         → "mila"
                          _                 → "mille"
            )
          ]

      forms ∷ s → s → s → s → s → Ctx Exp → s
      forms o1 o2 n a m c =
          case c of
            CtxEmpty            → o1
            _ | isOutside R c   → o2
            CtxAdd _ (Lit 10) _ → a
            CtxMul _ (Lit 10) _ → m
            _                   → n

bigNumSyms ∷ (IsString s) ⇒ [(ℤ, Ctx Exp → s)]
bigNumSyms =
  [ (6, BN.forms "sest" "sex"    "ses"    "sexa"   "ses")
  , (7, BN.forms "sett" "septen" "septem" "septua" "septin")
  , (8, BN.forms "ott"  "otto"   "otto"   "otto"   "ottin")
  ]
