{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        en

[@ISO639-2B@]       eng

[@ISO639-3@]        eng

[@Native name@]     English

[@English name@]    English

[@French name@]     anglais

[@Spanish name@]    inglés

[@Chinese name@]    英语

[@Russian name@]    английский

[@German name@]     Englisch

[@Language family@] Indo-European,
                    Germanic,
                    West Germanic,
                    Anglo-Frisian,
                    Anglic,
                    English

[@Scope@]           Individual language

[@Type@]            Living
-}

module Text.Numeral.Language.EN
    ( uk_cardinal
    , us_cardinal
    , shortScaleRule
    , longScaleRule
    , pelletierScaleRule
    , uk_cardinalRepr
    , ukPelletier_cardinalRepr
    , us_cardinalRepr
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( otherwise )
import Data.Function ( const )
import Data.Functor  ( (<$>) )
import Data.Maybe    ( Maybe(Nothing, Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Num, Integral, Integer )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import qualified Text.Numeral.Language.BigNum as BN ( rule, cardinalRepr )

--------------------------------------------------------------------------------
-- EN
--------------------------------------------------------------------------------

{-
TODO? Other interesting number names in English:

Base 20:
20      score           20
40      twoscore        2 × 20
60      threescore      3 × 20
80      fourscore       4 × 20
100     fivescore       5 × 20
120     sixscore        6 × 20
140     sevenscore      7 × 20
160     eightscore      8 × 20
180     ninescore       9 × 20
200     tenscore        10 × 20

Base 12:
6       half dozen      ½ × 12
12      dozen           12
13      baker's dozen   baker's 12
13      long dozen      long 12
72      half gross      ½ × 144
120     long hundred    long 100 (12 × 10)
144     gross           144 (12²)
156     long gross      long 144 (12 × 13)
1200    long thousand   long 1000 (12 × 10²)
1728    great gross     1728 (12³)
-}

uk_cardinal ∷ (Monoid s, IsString s, Integral α, Scale α)
            ⇒ α → Maybe s
uk_cardinal = mkCardinal shortScaleRule uk_cardinalRepr

us_cardinal ∷ (Monoid s, IsString s, Integral α, Scale α)
            ⇒ α → Maybe s
us_cardinal = mkCardinal shortScaleRule us_cardinalRepr

shortScaleRule ∷ (Integral α, Scale α, Num β, Scale β) ⇒ Rule α β
shortScaleRule = rule `combine` shortScale R L BN.rule

longScaleRule ∷ (Integral α, Scale α, Num β, Scale β) ⇒ Rule α β
longScaleRule = rule `combine` longScale  R L BN.rule

pelletierScaleRule ∷ (Integral α, Scale α, Num β, Scale β) ⇒ Rule α β
pelletierScaleRule = rule `combine` pelletierScale  R L BN.rule

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule (   0, atom         )
              [ (  13, add    10 L  )
              , (  20, mul    10 R L)
              , ( 100, atom1        )
              , ( 101, add   100 R  )
              , ( 200, mul1  100 R L)
              , (1000, atom1        )
              , (1001, add  1000 R  )
              , (2000, mul1 1000 R L)
              ]
               999999

us_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
us_cardinalRepr = cardinalRepr (⊞)
  where
    (_ :*: C 10) ⊞ _ = Just "-"
    (_ :*: _   ) ⊞ _ = Just " "
    _            ⊞ _ = Just ""

uk_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
uk_cardinalRepr = cardinalRepr (⊞)
  where
    (_ :*: C 10) ⊞ _ = Just "-"
    (_ :*: _   ) ⊞ x | eval x < (100 ∷ Integer) = Just " and "
                     | otherwise    = Just " "
    _            ⊞ _ = Just ""

ukPelletier_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
ukPelletier_cardinalRepr = uk_cardinalRepr { reprScale = pelletier }
    where
      pelletier _ 0 e = big "illion"  e
      pelletier _ 3 e = big "illiard" e
      pelletier _ _ _ = Nothing

      big s e = (⊕ s)  <$> textify BN.cardinalRepr e

cardinalRepr ∷ (Monoid s, IsString s) ⇒ (Exp → Exp → Maybe s) → Repr s
cardinalRepr f =
    defaultRepr
    { reprValue = \n → M.lookup n symMap
    , reprScale = \_ _ e → (⊕ "illion") <$> textify BN.cardinalRepr e
    , reprAdd   = f
    , reprMul   = (⊞)
    , reprNeg   = \_ → Just "minus "
    }
    where
      _ ⊞ (C 10) = Just ""
      _ ⊞ _      = Just " "

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
                             AddR (C _) _ → "teen"
                             MulR {}      → "ty"
                             _            → "ten"
                 )
               , (11,   const "eleven")
               , (12,   const "twelve")
               , (100,  const "hundred")
               , (1000, const "thousand")
               ]

      ten ∷ s → s → s → SymbolContext → s
      ten n a m = \c → case c of
                         AddL (C 10) _ → a
                         MulL (C 10) _ → m
                         _             → n
