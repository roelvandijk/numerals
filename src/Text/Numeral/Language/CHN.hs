{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        -

[@ISO639-2B@]       chn

[@ISO639-3@]        chn

[@Native name@]     -

[@English name@]    Chinook Jargon
-}

module Text.Numeral.Language.CHN
    ( cardinal
    , struct
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
import Prelude       ( Integral )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import qualified Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- CHN
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = checkPos (fix rule)

rule ∷ (Integral α, C.Lit β, C.Add β, C.Mul β) ⇒ Rule α β
rule = findRule (  0, lit           )
              [ ( 11, step 10 10 R L)
              , (100, lit           )
              ]
                 100

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = \_ _ → Just " pe "
               , reprMul   = \_ _ → Just " "
               }
    where
      syms =
          M.fromList
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
