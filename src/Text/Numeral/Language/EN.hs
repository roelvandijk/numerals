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
    , us_cardinal
    , shortScaleStruct
    , longScaleStruct
    , pelletierScaleStruct
    , uk_cardinalRepr
    , ukPelletier_cardinalRepr
    , us_cardinalRepr
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
import Prelude       ( Integral, Integer )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import qualified Text.Numeral.Exp.Classes as C
import qualified Text.Numeral.BigNum as BN ( rule, scaleRepr, pelletierRepr )


--------------------------------------------------------------------------------
-- EN
--------------------------------------------------------------------------------

uk_cardinal ∷ (Monoid s, IsString s, Integral α, C.Scale α) ⇒ α → Maybe s
uk_cardinal = shortScaleStruct >=> uk_cardinalRepr

us_cardinal ∷ (Monoid s, IsString s, Integral α, C.Scale α) ⇒ α → Maybe s
us_cardinal = shortScaleStruct >=> us_cardinalRepr

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
rule = findRule (   0, lit          )
              [ (  13, add    10 L  )
              , (  20, mul    10 R L)
              , ( 100, lit1         )
              , ( 101, add   100 R  )
              , ( 200, mul1  100 R L)
              , (1000, lit1         )
              , (1001, add  1000 R  )
              , (2000, mul1 1000 R L)
              ]
               999999

us_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
us_cardinalRepr = textify $ cardinalRepr (⊞)
  where
    (_ `Mul` Lit 10) ⊞ _ = Just "-"
    (_ `Mul` _     ) ⊞ _ = Just " "
    _                ⊞ _ = Just ""

uk_cardinalRepr' ∷ (Monoid s, IsString s) ⇒ Repr s
uk_cardinalRepr' = cardinalRepr (⊞)
  where
    (_ `Mul` Lit 10) ⊞ _             = Just "-"
    (_ `Mul` _     ) ⊞ x
        | eval x < (100 ∷ Integer) = Just " and "
        | otherwise                = Just " "
    _ ⊞ _ = Just ""

uk_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
uk_cardinalRepr = textify $ uk_cardinalRepr'

ukPelletier_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
ukPelletier_cardinalRepr = textify uk_cardinalRepr'
                                   { reprScale = pelletierRepr }
    where
      pelletierRepr = BN.pelletierRepr "illion"  "illion"
                                       "illiard" "illiard"
                                       []

cardinalRepr ∷ (Monoid s, IsString s) ⇒ (Exp → Exp → Maybe s) → Repr s
cardinalRepr f =
    defaultRepr
    { reprValue = \n → M.lookup n symMap
    , reprScale = BN.scaleRepr "illion" "illion" []
    , reprAdd   = f
    , reprMul   = (⊞)
    , reprNeg   = \_ → Just "minus "
    }
    where
      _ ⊞ (Lit 10) = Just ""
      _ ⊞ _        = Just " "

      symMap = M.fromList
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
                             CtxAddR (Lit _) _ → "teen"
                             CtxMulR {}        → "ty"
                             _                 → "ten"
                 )
               , (11,   const "eleven")
               , (12,   const "twelve")
               , (100,  const "hundred")
               , (1000, const "thousand")
               ]

      ten ∷ s → s → s → Ctx Exp → s
      ten n a m = \c → case c of
                         CtxAddL (Lit 10) _ → a
                         CtxMulL (Lit 10) _ → m
                         _                  → n
