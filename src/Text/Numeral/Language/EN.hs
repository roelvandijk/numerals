{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        en

[@ISO639-2B@]       eng

[@ISO639-3@]        eng

[@Native name@]     English

[@English name@]    English
-}

module Text.Numeral.Language.EN
    ( -- * Conversions
      uk_cardinal
    , uk_ordinal
    , ukPelletier_cardinal
    , us_cardinal
    , us_ordinal
      -- * Structure
    , shortScaleStruct
    , pelletierScaleStruct
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
import "base" Prelude       ( (+), (-), subtract, negate, (^), error, Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Prelude.Unicode ( ℤ, (⋅) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.Exp.Classes as C
import qualified "numerals-base" Text.Numeral.BigNum as BN


--------------------------------------------------------------------------------
-- EN
--------------------------------------------------------------------------------

uk_cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
uk_cardinal = render (cardinalRepr uk_add) ∘ shortScaleStruct

uk_ordinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
uk_ordinal = render (ordinalRepr uk_add) ∘ shortScaleStruct

ukPelletier_cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s)
                     ⇒ α → Maybe s
ukPelletier_cardinal = render (cardinalRepr uk_add) { reprScale = pelletierRepr }
                     ∘ pelletierScaleStruct
  where
    pelletierRepr = BN.pelletierRepr (const "illion")
                                     (const "illiard")
                                     []

us_cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
us_cardinal = render (cardinalRepr us_add) ∘ shortScaleStruct

us_ordinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
us_ordinal = render (ordinalRepr us_add) ∘ shortScaleStruct

shortScaleStruct ∷ ( Integral α, C.Scale α
                   , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
                   )
                 ⇒ α → β
shortScaleStruct = pos $ fix $ rule `combine` shortScale1 R L BN.rule

pelletierScaleStruct ∷ ( Integral α, C.Scale α
                       , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
                       )
                     ⇒ α → β
pelletierScaleStruct = pos $ fix $ rule `combine` pelletierScale1 R L BN.rule

rule ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β) ⇒ Rule α β
rule = findRule (   0, lit       )
              [ (  13, add 10 L  )
              , (  20, mul 10 R L)
              , ( 100, step1  100   10 R L)
              , (1000, step1 1000 1000 R L)
              ]
                (dec 6 - 1)

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 30003 - 1 in (negate x, x)

genericRepr ∷ (Monoid s, IsString s) ⇒ (Exp → Exp → Ctx Exp → s) → Repr s
genericRepr f =
    defaultRepr
    { reprAdd   = Just f
    , reprMul   = Just (⊞)
    , reprNeg   = Just $ \_ _ → "minus "
    }
    where
      (_ ⊞ Lit 10) _ = ""
      (_ ⊞ _     ) _ = " "

eval ∷ Exp → ℤ
eval (Lit x)       = x
eval (Add x y)     = eval x + eval y
eval (Mul x y)     = eval x ⋅ eval y
eval (Sub x y)     = subtract (eval x) (eval y)
eval (Neg x)       = negate (eval x)
eval (Scale b o r) = 10 ^ (eval r ⋅ b + o)
eval (Dual x)      = eval x
eval (Plural x)    = eval x
eval Unknown       = error "eval: unknown"


uk_add ∷ (IsString s) ⇒ Exp → Exp → Ctx Exp → s
((_ `Mul` Lit 10) `uk_add` _) _ = "-"
((_ `Mul` _     ) `uk_add` x) _
    | eval x < (100 ∷ ℤ)        = " and "
    | otherwise                 = " "
(_                `uk_add` _) _ = ""

us_add ∷ (IsString s) ⇒ Exp → Exp → Ctx Exp → s
((_ `Mul` Lit 10) `us_add` _) _ = "-"
((_ `Mul` _     ) `us_add` _) _ = " "
(_                `us_add` _) _ = ""

cardinalRepr ∷ (Monoid s, IsString s) ⇒ (Exp → Exp → Ctx Exp → s) → Repr s
cardinalRepr f = (genericRepr f)
                 { reprValue = \n → M.lookup n syms
                 , reprScale = BN.scaleRepr (const "illion") []
                 }
    where
      syms =
          M.fromList
          [ (0, const "zero")
          , (1, const "one")
          , (2, ten   "two"   "two"  "twen")
          , (3, ten   "three" "thir" "thir")
          , (4, ten   "four"  "four" "for")
          , (5, ten   "five"  "fif"  "fif")
          , (6, const "six")
          , (7, const "seven")
          , (8, ten   "eight" "eigh" "eigh")
          , (9, const "nine")
          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ → "teen"
                        CtxMul R _       _ → "ty"
                        _                  → "ten"
            )
          , (11,   const "eleven")
          , (12,   const "twelve")
          , (100,  const "hundred")
          , (1000, const "thousand")
          ]

      ten ∷ s → s → s → Ctx Exp → s
      ten n a m = \c → case c of
                         CtxAdd _ (Lit 10) _ → a
                         CtxMul _ (Lit 10) _ → m
                         _                   → n

ordinalRepr ∷ (Monoid s, IsString s) ⇒ (Exp → Exp → Ctx Exp → s) → Repr s
ordinalRepr f = (genericRepr f)
                { reprValue = \n → M.lookup n syms
                , reprScale = BN.scaleRepr (BN.ordQuantityName "illion" "illionth"
                                                               "illion" "illionth"
                                           )
                                           []
                }
    where
      syms =
          M.fromList
          [ (0, const "zeroth")
          , (1, \c → case c of
                       _ | isOutside R c → "first"
                         | otherwise     → "one"
            )
          , (2, ten   "second" "two"   "two"  "twen")
          , (3, ten   "third"  "three" "thir" "thir")
          , (4, ten   "fourth" "four"  "four" "for")
          , (5, ten   "fifth"  "five"  "fif"  "fif")
          , (6, \c → if isOutside R c then "sixth"   else "six")
          , (7, \c → if isOutside R c then "seventh" else "seven")
          , (8, ten   "eighth" "eight" "eigh" "eigh")
          , (9, \c → if isOutside R c then "ninth"   else "nine")
          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ | isOutside R c → "teenth"
                                           | otherwise     → "teen"
                        CtxMul R _       _ | isOutside R c → "tieth"
                                           | otherwise     → "ty"
                        _                  | isOutside R c → "tenth"
                                           | otherwise     → "ten"
            )
          , (11,   \c → if isOutside R c then "eleventh"   else "eleven")
          , (12,   \c → if isOutside R c then "twelfth"    else "twelf")
          , (100,  \c → if isOutside R c then "hundreth"   else "hundred")
          , (1000, \c → if isOutside R c then "thousandth" else "thousand")
          ]

      ten ∷ s -- ^ Ordinal form.
          → s -- ^ Cardinal form; normal.
          → s -- ^ Cardinal form; added to ten.
          → s -- ^ Cardinal form; multiplied with ten.
          → Ctx Exp
          → s
      ten o n a m ctx = case ctx of
                          _ | isOutside R ctx → o
                          CtxAdd _ (Lit 10) _ → a
                          CtxMul _ (Lit 10) _ → m
                          _                   → n
