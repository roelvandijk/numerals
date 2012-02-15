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
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
    , ordinal
      -- * Structure
    , struct
    , rule
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
import "base" Data.Ord      ( Ord )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.List.Unicode     ( (∈) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≥) )
import "base-unicode-symbols" Prelude.Unicode       ( ℤ )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.BigNum  as BN
import qualified "numerals-base" Text.Numeral.Exp     as E
import qualified "numerals-base" Text.Numeral.Grammar as G
import           "numerals-base" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry


--------------------------------------------------------------------------------
-- IT
--------------------------------------------------------------------------------

entry ∷ (Monoid s, IsString s) ⇒ Entry s
entry = emptyEntry
    { entIso639_1       = Just "it"
    , entIso639_2       = ["ita"]
    , entIso639_3       = Just "ita"
    , entNativeNames    = ["Italiano"]
    , entEnglishName    = Just "Italian"
    , entCardinal       = Just Conversion
                          { toNumeral   = cardinal
                          , toStructure = struct
                          }
    , entOrdinal        = Just Conversion
                          { toNumeral   = ordinal
                          , toStructure = struct
                          }
    }

cardinal ∷ ( G.Masculine i, G.Feminine i
           , Integral α, E.Scale α
           , Monoid s, IsString s
           )
         ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

ordinal ∷ ( G.Masculine i, G.Feminine i
          , Integral α, E.Scale α
          , Monoid s, IsString s
          )
        ⇒ i → α → Maybe s
ordinal inf = ordinalRepr inf ∘ struct

rule ∷ ( Integral α, Ord α
       , E.Unknown β, E.Lit β, E.Add β, E.Mul β
       )
     ⇒ Rule α β
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

struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
         )
       ⇒ α → β
struct = pos $ fix $ rule `combine` pelletierScale R L BN.rule

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 60000 - 1 in (negate x, x)

genericRepr ∷ (Monoid s, IsString s) ⇒ Repr i s
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

cardinalRepr ∷ (G.Feminine i, G.Masculine i, Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render genericRepr
               { reprValue = \inf n → M.lookup n (syms inf)
               , reprScale = BN.pelletierRepr
                               (BN.quantityName "ilione"  "ilioni")
                               (BN.quantityName "iliardo" "iliardi")
                               bigNumSyms
               }
    where
      syms inf =
          M.fromList
          [ (0, const "zero")
          , (1, \c → case c of
                       CtxAdd _ (Lit 10) _   → "un"
                       _ | G.isFeminine  inf → "una"
                         | G.isMasculine inf → "un"
                         | otherwise         → "uno"
            )
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

      forms ∷ s → s → s → Ctx (Exp i) → s
      forms n a m c = case c of
                        CtxAdd _ (Lit 10) _ → a
                        CtxMul _ (Lit 10) _ → m
                        _                   → n

ordinalRepr ∷ (G.Feminine i, Monoid s, IsString s) ⇒ i → Exp i → Maybe s
ordinalRepr = render genericRepr
              { reprValue = \inf n → M.lookup n (syms inf)
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
      syms inf =
          M.fromList
          [ (0, const "zero")
          , (1, forms "prima" "primo" "unesimo" "uno" "un" "un")
          , (2, forms "seconda" "secondo" "duesimo" "due" "do" "due")
          , (3, \c → case c of
                       CtxEmpty
                         | G.isFeminine inf → "terza"
                         | otherwise        → "terzo"
                       _ | isOutside R c    → "treesimo"
                       CtxAdd R _        _  → "tré"
                       CtxMul _ (Lit 10) _  → "tren"
                       _                    → "tre"
            )
          , (4, forms "quarta"  "quarto"  "quattresimo" "quattro" "quattor" "quar")
          , (5, forms "quinta"  "quinto"  "cinquesimo"  "cinque"  "quin"    "cinqu")
          , (6, forms "sesta"   "sesto"   "seiesimo"    "sei"     "se"      "sess")
          , (7, forms "settimo" "settimo" "settesimo"   "sette"   "sette"   "sett")
          , (8, forms "ottave"  "ottavo"  "ottesimo"    "otto"    "otto"    "ott")
          , (9, forms "nono"    "nono"    "novesimo"    "nove"    "nove"    "nov")
          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ | isOutside R c → "dicesimo"
                                           | otherwise     → "dici"
                        -- Last vowel removed because of a phonetic rule:
                        CtxMul _ (Lit _) (CtxAdd _ (Lit n) _)
                            | n ∈ [1,8]                    → "t"
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
          where
            forms ∷ s → s → s → s → s → s → Ctx (Exp i) → s
            forms fo1 o1 o2 n a m c =
                case c of
                  CtxEmpty
                    | G.isFeminine inf → fo1
                    | otherwise        → o1
                  _ | isOutside R c    → o2
                  CtxAdd _ (Lit 10) _  → a
                  CtxMul _ (Lit 10) _  → m
                  _                    → n

bigNumSyms ∷ (IsString s) ⇒ [(ℤ, Ctx (Exp i) → s)]
bigNumSyms =
  [ (6, BN.forms "sest" "sex"    "ses"    "sexa"   "ses")
  , (7, BN.forms "sett" "septen" "septem" "septua" "septin")
  , (8, BN.forms "ott"  "otto"   "otto"   "otto"   "ottin")
  ]
