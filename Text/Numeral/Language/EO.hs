{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        eo

[@ISO639-2B@]       epo

[@ISO639-3@]        epo

[@Native name@]     Esperanto

[@English name@]    Esperanto

[@French name@]     espéranto

[@Spanish name@]    esperanto

[@Chinese name@]    世界语

[@Russian name@]    эсперанто

[@German name@]     Esperanto

[@Language family@] Constructed

[@Scope@]           Individual language

[@Type@]            Living
-}

module Text.Numeral.Language.EO
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
-- EO
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, Num β) ⇒ α → Maybe β
struct = positive (fix rule)

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule (  0, atom       )
              [ ( 11, add  10 R  )
              , ( 20, mul  10 R L)
              , (100, atom       )
              , (101, add 100 R  )
              , (200, mul 100 R L)
              ]
                1000

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = \_ _ → Just " "
               , reprMul   = \_ _ → Just ""
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
