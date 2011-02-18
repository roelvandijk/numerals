{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.NL
    ( cardinal
    , rules
    , cardinalRepr
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool(False), otherwise )
import Data.Function ( const )
import Data.List     ( map )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, fromInteger )

-- from base-unicode-symbols:
import Data.Bool.Unicode     ( (∨) )
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Data.Monoid.Unicode   ( (⊕) )

-- from containers:
import qualified Data.IntMap as IM ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier ( scale )


--------------------------------------------------------------------------------
-- NL
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral i) ⇒ i → Maybe s
cardinal = textify cardinalRepr ∘ deconstruct rules

rules ∷ (Integral i) ⇒ Rules i
rules = Rules { rsFindRule = findRule rs
              , rsMulOne   = const False
              }
    where
      rs = map atom [1..9]
         ⊕ [mul 10  10  10 LeftAdd]
         ⊕ map atom [11..12]
         ⊕ [mul 100 100 10 RightAdd]
         ⊕ scale RightAdd 3

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → IM.lookup (fromInteger n) symMap
         , reprAdd   = (⊞)
         , reprMul   = \_ _ → ""
         , reprZero  = "nul"
         , reprNeg   = "min "
         }
    where
      _   ⊞ C 10 = ""
      C n ⊞ _ | n ≡ 2 ∨ n ≡ 3 = "ën"
              | n < 10        = "en"
              | otherwise     = ""
      _   ⊞ _ = " "

      symMap = IM.fromList
               [ (1, const "een")
               , (2, ten   "twee" "twin")
               , (3, ten   "drie" "der")
               , (4, ten   "vier" "veer")
               , (5, const "vijf")
               , (6, const "zes")
               , (7, const "zeven")
               , (8, \c → case c of
                            LM (C 10) _ → "tach"
                            LA (C 10) _ → "ach"
                            _           → "acht"
                 )
               , (9, const "negen")
               , (10, \c → case c of
                             RM {} → "tig"
                             _     → "tien"
                 )
               , (11, const "elf")
               , (12, const "twaalf")
               , (100, const "honderd")
               , (1000, const "duizend")
               ]

      ten ∷ s → s → SymbolContext → s
      ten n t ctx = case ctx of
                      LM (C 10) _ → t
                      LA (C 10) _ → t
                      _           → n
