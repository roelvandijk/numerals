{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        -

[@ISO639-2B@]       -

[@ISO639-3@]        nqm

[@Native name@]     -

[@English name@]    Ndom

[@French name@]     -

[@Spanish name@]    -

[@Chinese name@]    -

[@Russian name@]    -

[@German name@]     -

[@Language family@] Trans-New Guinea, Kolopom, Ndom

[@Scope@]           Individual

[@Type@]            Living
-}

module Text.Numeral.Language.NQM
    ( cardinal
    , rule
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( const )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Num, Integral )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral


--------------------------------------------------------------------------------
-- NQM
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = mkCardinal rule cardinalRepr

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule rules

rules ∷ (Integral α, Num β) ⇒ Rules α β
rules = [ ((  1,   6), atom)
        , ((  7,  11), add 6 R)
        , (( 12,  17), mul 6 R R)
        , (( 18,  18), atom)
        , (( 19,  35), add 18 R)
        , (( 36,  36), atom)
        , (( 37,  71), add 36 R)
        , (( 72, 107), mul 36 R R)
        ]

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd   = \_ _ → " abo "
         , reprMul   = (⊡)
         , reprSub   = \_ _ → ""
         , reprNeg   = ""
         }
    where
      C 36 ⊡ _ = " "
      _    ⊡ _ = " an "

      symMap = M.fromList
               [ (1, const "sas")
               , (2, const "thef")
               , (3, const "ithin")
               , (4, const "thonith")
               , (5, const "meregh")
               , (6, const "mer")
               , (18, const "tondor")
               , (36, const "nif")
               ]
