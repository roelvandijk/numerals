{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.NO
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
import Prelude       ( Integral, Num, fromInteger )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Monoid.Unicode   ( (⊕) )
import Data.Ord.Unicode      ( (≥) )

-- from containers:
import qualified Data.IntMap as IM ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc      ( dec )
import Text.Numeral.Pelletier ( scale )


-------------------------------------------------------------------------------
-- NO
-------------------------------------------------------------------------------

-- Sources:
--   http://en.wikibooks.org/wiki/Norwegian_Numbers
--   http://www.sf.airnet.ne.jp/~ts/language/number/norwegian.html

cardinal ∷ (Monoid s, IsString s, Integral i) ⇒ i → Maybe s
cardinal = textify cardinalRepr ∘ deconstruct rules

rules ∷ (Integral i) ⇒ Rules i
rules = Rules { rsFindRule = findRule rs
              , rsMulOne   = (≥ dec 6)
              }
    where
      rs = map atom [1..9]
         ⊕ map atom [11..12]
         ⊕ [ add 10 10 10 LeftAdd  True
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
         , reprZero = "null"
         , reprNeg  = " " -- TODO
         }
    where
      _ ⊞ _ = ""

      symMap = IM.fromList
               [ (1,  const "en")
               , (2,  const "to")
               , (3,  ten   "tre"  "tret" "tret")
               , (4,  ten   "fire" "fjor" "før")
               , (5,  const "fem")
               , (6,  const "seks")
               , (7,  ten   "syv"  "syt"  "syt")
               , (8,  ten   "åtte" "at"   "åt")
               , (9,  ten   "ni"   "nit"  "nit")
               , (10, \c → case c of
                             RA {} → "ten"
                             _     → "ti"
                 )
               , (11, const "elleve")
               , (12, const "tolv")
               , (20, const "tjue")
               , (100, const "hundre")
               , (1000, const "tusen")
               ]

      ten n a m = \c → case c of
                         LA (C 10) _ → a
                         LM (C 10) _ → m
                         _           → n
