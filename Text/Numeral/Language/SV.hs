{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.SV
    ( cardinal
    , rules
    , cardinalRepr
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

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
import Data.Monoid.Unicode   ( (⊕) )
import Data.Ord.Unicode      ( (≥) )

-- from containers:
import qualified Data.IntMap as IM ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier ( scale )


-------------------------------------------------------------------------------
-- SV
-------------------------------------------------------------------------------


cardinal ∷ (Monoid s, IsString s, Integral i) ⇒ i → Maybe s
cardinal = textify cardinalRepr ∘ deconstruct rules

rules ∷ (Integral i) ⇒ Rules i
rules = Rules { rsFindRule = findRule rs
              , rsMulOne   = (≥ 100)
              }
    where
      rs = map atom [1..12]
         ⊕ [ add 10 13  7 LeftAdd  True
           , add 20 20 10 RightAdd True
           , mul 10 30 10 RightAdd
           , mul 100 100 10 RightAdd
           ]
         ⊕ scale RightAdd 3

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → IM.lookup (fromInteger n) symMap
         , reprAdd = (⊞)
         , reprMul = \_ _ → ""
         , reprZero = "noll"
         , reprNeg  = "minus "
         }
    where
      _ ⊞ _ = ""

      symMap = IM.fromList
               [ (1,  const "ett")
               , (2,  const "två")
               , (3,  ten   "tre"  "tret" "tret")
               , (4,  ten   "fyra" "fjor" "fyr")
               , (5,  const "fem")
               , (6,  const "sex")
               , (7,  ten   "sju"  "sjut" "sjut")
               , (8,  ten   "åtta" "ar"   "åt")
               , (9,  ten   "nio"  "nit"  "nit")
               , (10, \c → case c of
                             RA {} → "ton"
                             _     → "tio"
                 )
               , (11, const "elva")
               , (12, const "tolv")
               , (20, const "tjugo")
               , (100, const "hundra")
               , (1000, const "tusen")
               ]

      ten n a m = \c → case c of
                         LA (C 10) _ → a
                         LM (C 10) _ → m
                         _           → n
