{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        en

[@ISO639-2B@]       eng

[@ISO639-3@]        eng

[@Native name@]     English

[@English name@]    English
-}

module Text.Numeral.Language.EN
    ( uk_cardinal
    , ukPelletier_cardinal
    , us_cardinal
    , shortScaleStruct
    , longScaleStruct
    , pelletierScaleStruct
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Bool     ( otherwise )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, (-), Integer )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.Exp.Classes as C
import qualified Text.Numeral.BigNum as BN ( rule, scaleRepr, pelletierRepr )


--------------------------------------------------------------------------------
-- EN
--------------------------------------------------------------------------------

uk_cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
uk_cardinal = shortScaleStruct >=> textify uk_cardinalRepr'

ukPelletier_cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s)
                     ⇒ α → Maybe s
ukPelletier_cardinal =
    pelletierScaleStruct >=> textify uk_cardinalRepr'
                             { reprScale = pelletierRepr }
  where
    pelletierRepr = BN.pelletierRepr "illion"  "illion"
                                     "illiard" "illiard"
                                     []

us_cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
us_cardinal = shortScaleStruct >=> textify (cardinalRepr (⊞))
  where
    ((_ `Mul` Lit 10) ⊞ _) _ = "-"
    ((_ `Mul` _     ) ⊞ _) _ = " "
    (_                ⊞ _) _ = ""

shortScaleStruct ∷ ( Integral α, C.Scale α
                   , C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
                   )
                 ⇒ α → Maybe β
shortScaleStruct = pos $ fix $ rule `combine` shortScale1 R L BN.rule

longScaleStruct ∷ ( Integral α, C.Scale α
                  , C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
                  )
                ⇒ α → Maybe β
longScaleStruct = pos $ fix $ rule `combine` longScale1 R L BN.rule

pelletierScaleStruct ∷ ( Integral α, C.Scale α
                       , C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
                       )
                     ⇒ α → Maybe β
pelletierScaleStruct = pos $ fix $ rule `combine` pelletierScale1 R L BN.rule

rule ∷ (Integral α, C.Lit β, C.Add β, C.Mul β) ⇒ Rule α β
rule = findRule (   0, lit       )
              [ (  13, add 10 L  )
              , (  20, mul 10 R L)
              , ( 100, step1  100   10 R L)
              , (1000, step1 1000 1000 R L)
              ]
                (dec 6 - 1)

uk_cardinalRepr' ∷ (Monoid s, IsString s) ⇒ Repr s
uk_cardinalRepr' = cardinalRepr (⊞)
  where
    ((_ `Mul` Lit 10) ⊞ _) _       = "-"
    ((_ `Mul` _     ) ⊞ x) _
        | eval x < (100 ∷ Integer) = " and "
        | otherwise                = " "
    (_                ⊞ _) _       = ""

cardinalRepr ∷ (Monoid s, IsString s) ⇒ (Exp → Exp → Ctx Exp → s) → Repr s
cardinalRepr f =
    defaultRepr
    { reprValue = \n → M.lookup n syms
    , reprScale = BN.scaleRepr "illion" "illion" []
    , reprAdd   = Just f
    , reprMul   = Just (⊞)
    , reprNeg   = Just $ \_ _ → "minus "
    }
    where
      (_ ⊞ Lit 10) _ = ""
      (_ ⊞ _     ) _ = " "

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
