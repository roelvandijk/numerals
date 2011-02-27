{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        sv

[@ISO639-2B@]       swe

[@ISO639-3@]        swe

[@Native name@]     svenska

[@English name@]    Swedish

[@French name@]     suédois

[@Spanish name@]    sueco

[@Chinese name@]    瑞典语

[@Russian name@]    шведский

[@German name@]     Schwedisch

[@Language family@] Indo-European,
                    Germanic,
                    North Germanic,
                    East Scandinavian,
                    Swedish

[@Scope@]           Individual language

[@Type@]            Living
-}

module Text.Numeral.Language.SV
    ( cardinal
    , rule
    , cardinalRepr
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Function ( const )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral


-------------------------------------------------------------------------------
-- SV
-------------------------------------------------------------------------------

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
        , ((100, 100), atom1)
        , ((101, 199), add 100 R)
        , ((200, 999), mul1 100 R L)
        ]

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd   = (⊞)
         , reprMul   = \_ _ → ""
         , reprSub   = \_ _ → ""
         , reprNeg   = "minus "
         }
    where
      _ ⊞ _ = ""

      symMap = M.fromList
               [ (0,  const "noll")
               , (1,  const "ett")
               , (2,  const "två")
               , (3,  ten   "tre"  "tret" "tret")
               , (4,  ten   "fyra" "fjor" "fyr")
               , (5,  const "fem")
               , (6,  const "sex")
               , (7,  ten   "sju"  "sjut" "sjut")
               , (8,  ten   "åtta" "ar"   "åt")
               , (9,  ten   "nio"  "nit"  "nit")
               , (10, \c → case c of
                             AddR {} → "ton"
                             _       → "tio"
                 )
               , (11, const "elva")
               , (12, const "tolv")
               , (20, const "tjugo")
               , (100, const "hundra")
               , (1000, const "tusen")
               ]

      ten n a m = \c → case c of
                         AddL (C 10) _ → a
                         MulL (C 10) _ → m
                         _             → n
