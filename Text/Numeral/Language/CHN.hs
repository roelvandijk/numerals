{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]

[@ISO639-2B@]       chn

[@ISO639-3@]        chn

[@Native name@]

[@English name@]    Chinook Jargon

[@French name@]

[@Spanish name@]

[@Chinese name@]

[@Russian name@]

[@German name@]

[@Language family@] Various - Mainly Wakashan, Chinookan and Indo-European

[@Scope@]           Individual

[@Type@]            Living
-}

module Text.Numeral.Language.CHN
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
import Text.Numeral.Rules     ( Side(L, R), atom, add, mul )


--------------------------------------------------------------------------------
-- CHN
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal n = deconstruct findRule n >>= textify cardinalRepr

findRule ∷ (Integral α, Num β) ⇒ FindRule α β
findRule = mkFindRule rules (scale 3 R L)

rules ∷ (Integral α, Num β) ⇒ Rules α β
rules = [ ((  1,  10), atom)
        , (( 11,  19), add 10 R)
        , (( 20,  99), mul 10 R L)
        , ((100, 100), atom)
        ]

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd   = \_ _ → " pe "
         , reprMul   = \_ _ → " "
         , reprSub   = \_ _ → ""
         , reprNeg   = "" -- TODO: probably not supported
         }
    where
      symMap = M.fromList
               [ (1, const "ikt")
               , (2, const "mokst")
               , (3, const "klone")
               , (4, const "lakit")
               , (5, const "kwinnum")
               , (6, const "taghum")
               , (7, const "sinamokst")
               , (8, const "stotekin")
               , (9, const "kwaist")
               , (10, const "tahtlum")
               , (100, const "tukamonuk")
               ]
