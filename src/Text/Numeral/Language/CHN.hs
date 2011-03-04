{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        -

[@ISO639-2B@]       chn

[@ISO639-3@]        chn

[@Native name@]     -

[@English name@]    Chinook Jargon

[@French name@]     -

[@Spanish name@]    -

[@Chinese name@]    -

[@Russian name@]    -

[@German name@]     -

[@Language family@] Various - Mainly Wakashan, Chinookan and Indo-European

[@Scope@]           Individual

[@Type@]            Living
-}

module Text.Numeral.Language.CHN
    ( cardinal
    , struct
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Num, Integral )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral


--------------------------------------------------------------------------------
-- CHN
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, Num β) ⇒ α → Maybe β
struct = positive (fix rule)

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule (  0, atom      )
              [ (  1, atom      )
              , ( 11, add 10 R  )
              , ( 20, mul 10 R L)
              , (100, atom      )
              ]
                 100

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = \_ _ → Just " pe "
               , reprMul   = \_ _ → Just " "
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
