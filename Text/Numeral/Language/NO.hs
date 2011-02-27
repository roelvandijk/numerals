{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        nb

[@ISO639-2B@]       nob

[@ISO639-3@]        nob

[@Native name@]     Bokmål

[@English name@]    Norwegian Bokmål

[@French name@]     Norvégien bokmål

[@Spanish name@]    Noruego bokmål

[@Chinese name@]    挪威布克莫尔语; 书面挪威语; 波克莫尔语

[@Russian name@]    букмол

[@German name@]     Norwegische Bokmål

[@Language family@] Indo-European,
                    Germanic,
                    North Germanic,
                    Mainland or East Scandinavian,
                    Norwegian

[@Scope@]           Individual language

[@Type@]            Living
-}

module Text.Numeral.Language.NO
    ( cardinal
    , rule
    , cardinalRepr
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Function ( const )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral


-------------------------------------------------------------------------------
-- NO
-------------------------------------------------------------------------------

-- Sources:
--   http://en.wikibooks.org/wiki/Norwegian_Numbers
--   http://www.sf.airnet.ne.jp/~ts/language/number/norwegian.html

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = mkCardinal rule cardinalRepr

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule rules

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

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
cardinalRepr = defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = \_ _ → Just ""
               , reprMul   = \_ _ → Just ""
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
                             _       → "ti"
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
                         _             → n
