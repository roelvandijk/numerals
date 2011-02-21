{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.EN
    ( uk_cardinal
    , us_cardinal
    , rules
    , uk_cardinalRepr
    , us_cardinalRepr
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool(True) )
import Data.Function ( const )
import Data.List     ( map )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, fromInteger )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Ord.Unicode      ( (≥) )
import Data.Monoid.Unicode   ( (⊕) )

-- from containers:
import qualified Data.IntMap as IM ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier ( scale )


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

uk_cardinal ∷ (Monoid s, IsString s, Integral i) ⇒ i → Maybe s
uk_cardinal = textify uk_cardinalRepr ∘ deconstruct rules

us_cardinal ∷ (Monoid s, IsString s, Integral i) ⇒ i → Maybe s
us_cardinal = textify us_cardinalRepr ∘ deconstruct rules

rules ∷ (Integral i) ⇒ Rules i
rules = Rules { rsFindRule = findRule rs
              , rsMulOne   = (≥ 100)
              }
    where
      rs = map atom [1..12]
         ⊕ [ add 10 10 10 LeftAdd True
           , mul 10 20 10 RightAdd
           ]
         ⊕ [mul 100 100 10 RightAdd]
         ⊕ scale RightAdd 3

us_cardinalRepr ∷ (IsString s) ⇒ Repr s
us_cardinalRepr = cardinalRepr (⊞)
  where
    (_ :⋅: C 10) ⊞ _ = "-"
    (_ :⋅: _   ) ⊞ _ = " "
    _            ⊞ _ = ""

uk_cardinalRepr ∷ (IsString s) ⇒ Repr s
uk_cardinalRepr = cardinalRepr (⊞)
  where
    (_ :⋅: C 10) ⊞ _ = "-"
    (_ :⋅: _   ) ⊞ _ = " and "
    _            ⊞ _ = ""

cardinalRepr ∷ (IsString s) ⇒ (Exp → Exp → s) → Repr s
cardinalRepr f =
    Repr { reprValue = \n → IM.lookup (fromInteger n) symMap
         , reprAdd   = f
         , reprMul   = (⊡)
         , reprZero  = "zero"
         , reprNeg   = "minus "
         }
    where
      _ ⊡ C 10 = ""
      _ ⊡ _    = " "

      symMap = IM.fromList
               [ (1, const "one")
               , (2, ten   "two"   "two"  "twen")
               , (3, ten   "three" "thir" "thir")
               , (4, ten   "four"  "four" "for")
               , (5, ten   "five"  "fif"  "fif")
               , (6, const "six")
               , (7, const "seven")
               , (8, ten   "eight" "eigh" "eigh")
               , (9, const "nine")
               , (10, \c → case c of
                             RA (C _) _ → "teen"
                             RM {} → "ty"
                             _     → "ten"
                 )
               , (11,   const "eleven")
               , (12,   const "twelve")
               , (100,  const "hundred")
               , (1000, const "thousand")
               ]

      ten ∷ s → s → s → SymbolContext → s
      ten n a m = \c → case c of
                         LA (C 10) _ → a
                         LM (C 10) _ → m
                         _           → n
