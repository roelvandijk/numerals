{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        nl

[@ISO639-2B@]       dut

[@ISO639-3@]        nld

[@Native name@]     Nederlands

[@English name@]    Dutch

[@French name@]     Néerlandais

[@Spanish name@]    Neerlandés

[@Chinese name@]    荷兰语

[@Russian name@]    нидерландский

[@German name@]     Niederländisch

[@Language family@] Indo-European,
                    Germanic,
                    West Germanic,
                    Low Franconian,
                    Dutch

[@Scope@]           Individual language

[@Type@]            Living
-}

module Text.Numeral.Language.NL
    ( cardinal
    , rule
    , cardinalRepr
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( otherwise )
import Data.Function ( const )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from base-unicode-symbols:
import Data.Bool.Unicode     ( (∨) )
import Data.Eq.Unicode       ( (≡) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral


--------------------------------------------------------------------------------
-- NL
--------------------------------------------------------------------------------
-- scale 3 R L

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = mkCardinal rule cardinalRepr

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule rules

rules ∷ (Integral α, Num β) ⇒ Rules α β
rules = [ ((  0,  12), atom)
        , (( 13,  19), add 10 L)
        , (( 20,  99), mul 10 L L)
        , ((100, 100), atom)
        , ((101, 199), add 100 R)
        , ((200, 999), mul 100 R L)
        , ((1000, 1000), atom)
        , ((1001, 1999), add 1000 R)
        , ((2000, 999999), mul 1000 R L)
        ]

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd   = (⊞)
         , reprMul   = \_ _ → ""
         , reprSub   = \_ _ → ""
         , reprNeg   = "min "
         }
    where
      _   ⊞ C 10 = ""
      C n ⊞ _ | n ≡ 2 ∨ n ≡ 3 = "ën"
              | n < 10        = "en"
              | otherwise     = ""
      _   ⊞ _ = " "

      symMap = M.fromList
               [ (0, const "nul")
               , (1, const "een")
               , (2, ten   "twee" "twin")
               , (3, ten   "drie" "der")
               , (4, ten   "vier" "veer")
               , (5, const "vijf")
               , (6, const "zes")
               , (7, const "zeven")
               , (8, \c → case c of
                            MulL (C 10) _ → "tach"
                            AddL (C 10) _ → "ach"
                            _             → "acht"
                 )
               , (9, const "negen")
               , (10, \c → case c of
                             MulR {} → "tig"
                             _       → "tien"
                 )
               , (11, const "elf")
               , (12, const "twaalf")
               , (100, const "honderd")
               , (1000, const "duizend")
               ]

      ten n t ctx = case ctx of
                      MulL (C 10) _ → t
                      AddL (C 10) _ → t
                      _             → n
