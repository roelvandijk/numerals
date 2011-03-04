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
    , struct
    , cardinalRepr
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Bool     ( otherwise )
import Data.Function ( ($), const, fix )
import Data.Functor  ( (<$>) )
import Data.Maybe    ( Maybe(Nothing, Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from base-unicode-symbols:
import Data.Bool.Unicode   ( (∨) )
import Data.Eq.Unicode     ( (≡) )
import Data.Monoid.Unicode ( (⊕) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import qualified Text.Numeral.Language.BigNum as BN ( rule, cardinalRepr )


--------------------------------------------------------------------------------
-- NL
--------------------------------------------------------------------------------
-- scale 3 R L

cardinal ∷ (Monoid s, IsString s, Integral α, Scale α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, Scale α, Num β, Scale β) ⇒ α → Maybe β
struct = positive (fix $ rule `combine` pelletierScale R L BN.rule)

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule (   0, atom        )
              [ (  13, add   10 L  )
              , (  20, mul   10 L L)
              , ( 100, atom        )
              , ( 101, add  100 R  )
              , ( 200, mul  100 R L)
              , (1000, atom        )
              , (1001, add 1000 R  )
              , (2000, mul 1000 R L)
              ]
               999999

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprScale = pelletier
               , reprAdd   = (⊞)
               , reprMul   = \_ _ → Just ""
               , reprNeg   = \_   → Just "min "
               }
    where
      pelletier _ 0 e _ = big "iljoen" e
      pelletier _ 3 e _ = big "iljard" e
      pelletier _ _ _ _ = Nothing

      big s e = (⊕ s) <$> textify BN.cardinalRepr e

      _   ⊞ C 10 = Just ""
      C n ⊞ _ | n ≡ 2 ∨ n ≡ 3 = Just "ën"
              | n < 10        = Just "en"
              | otherwise     = Just ""
      _   ⊞ _ = Just " "

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
