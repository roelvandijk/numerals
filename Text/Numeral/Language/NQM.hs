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
import Data.Maybe    ( Maybe(Just) )
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
rule = findRule (1, atom        )
              [ (  1, atom      )
              , (  7, add  6 R  )
              , ( 12, mul  6 R R)
              , ( 18, atom      )
              , ( 19, add 18 R  )
              , ( 36, atom      )
              , ( 37, add 36 R  )
              , ( 72, mul 36 R R)
              ]
                 107

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
cardinalRepr = defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = \_ _ → Just " abo "
               , reprMul   = (⊡)
               }
    where
      C 36 ⊡ _ = Just " "
      _    ⊡ _ = Just " an "

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
