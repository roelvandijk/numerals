{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.EO
    ( cardinal
    , findRule
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>>=) )
import Data.Function ( const )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Num, Integral )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier ( scale )


--------------------------------------------------------------------------------
-- EO
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal n = deconstruct findRule n >>= textify cardinalRepr

findRule ∷ (Integral α, Num β) ⇒ FindRule α β
findRule = mkFindRule rules (scale 3 R L)

rules ∷ (Integral α, Num β) ⇒ Rules α β
rules = [ ((  0,  10), atom)
        , (( 11,  19), add 10 R)
        , (( 20,  99), mul 10 R L)
        , ((100, 100), atom)
        , ((101, 199), add 100 R)
        , ((200, 999), mul 100 R L)
        ]

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd  = \_ _ → " "
         , reprMul  = \_ _ → ""
         , reprNeg  = "ne " -- ???
         }
    where
      symMap = M.fromList
               [ (0, const "nul")
               , (1, const "unu")
               , (2, const "du")
               , (3, const "tri")
               , (4, const "kvar")
               , (5, const "kvin")
               , (6, const "ses")
               , (7, const "sep")
               , (8, const "ok")
               , (9, const "naŭ")
               , (10, const "dek")
               , (100, const "cent")
               , (1000, const "mil")
               ]
