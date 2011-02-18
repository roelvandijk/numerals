{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.EN
    ( ukCardinal
    , usCardinal
    , rules
    , ukCardinalRepr
    , usCardinalRepr
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

ukCardinal ∷ (Monoid s, IsString s, Integral i) ⇒ i → Maybe s
ukCardinal = textify ukCardinalRepr ∘ deconstruct rules

usCardinal ∷ (Monoid s, IsString s, Integral i) ⇒ i → Maybe s
usCardinal = textify usCardinalRepr ∘ deconstruct rules

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

usCardinalRepr ∷ (IsString s) ⇒ Repr s
usCardinalRepr = cardinalRepr (⊞)
  where
    (_ :⋅: C 10) ⊞ _ = "-"
    (_ :⋅: _   ) ⊞ _ = " "
    _            ⊞ _ = ""

ukCardinalRepr ∷ (IsString s) ⇒ Repr s
ukCardinalRepr = cardinalRepr (⊞)
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
