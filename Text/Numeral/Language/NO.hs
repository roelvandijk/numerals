{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.NO
    ( cardinal
    , findRule
    , cardinalRepr
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>>=) )
import Data.Function ( const )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier ( scale )
import Text.Numeral.Rules     ( Side(L, R), atom, add, mul )


-------------------------------------------------------------------------------
-- NO
-------------------------------------------------------------------------------

-- Sources:
--   http://en.wikibooks.org/wiki/Norwegian_Numbers
--   http://www.sf.airnet.ne.jp/~ts/language/number/norwegian.html

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal n = deconstruct findRule n >>= textify cardinalRepr

findRule ∷ (Integral α, Num β) ⇒ FindRule α β
findRule = mkFindRule rules (scale 3 R L)

rules ∷ (Integral α, Num β) ⇒ Rules α β
rules = [ ((  0,  12), atom)
        , (( 13,  19), add 10 L)
        , (( 20,  20), atom)
        , (( 21,  29), add 20 R)
        , (( 30,  99), mul 10 R L)
        , ((100, 100), atom)
        , ((101, 199), add 100 R)
        , ((200, 999), mul 100 R L)
        ]

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd   = \_ _ → ""
         , reprMul   = \_ _ → ""
         , reprSub   = \_ _ → ""
         , reprNeg   = " " -- TODO
         }
    where
      symMap = M.fromList
               [ (0,  const "null")
               , (1,  const "en")
               , (2,  const "to")
               , (3,  ten   "tre"  "tret" "tret")
               , (4,  ten   "fire" "fjor" "før")
               , (5,  const "fem")
               , (6,  const "seks")
               , (7,  ten   "syv"  "syt"  "syt")
               , (8,  ten   "åtte" "at"   "åt")
               , (9,  ten   "ni"   "nit"  "nit")
               , (10, \c → case c of
                             AddR {} → "ten"
                             _     → "ti"
                 )
               , (11, const "elleve")
               , (12, const "tolv")
               , (20, const "tjue")
               , (100, const "hundre")
               , (1000, const "tusen")
               ]

      ten n a m = \c → case c of
                         AddL (C 10) _ → a
                         MulL (C 10) _ → m
                         _           → n
