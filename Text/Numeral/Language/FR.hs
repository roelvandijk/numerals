{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.FR (rules, repr) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool(True, False), otherwise )
import Data.Function ( const )
import Data.List     ( map )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Num, fromInteger )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )

-- from containers:
import qualified Data.IntMap as IM ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier ( scale )


--------------------------------------------------------------------------------
-- FR
--------------------------------------------------------------------------------

rules ∷ Rules
rules = Rules { rsFindRule = findRule rs
              , rsMulOne   = const False
              }
    where
      rs = map atom [1..9]
           ⊕ [ mul  10  10 10 LeftAdd
             , mul  10  17 10 RightAdd
             , add  20  20 10 RightAdd True
             , add  60  60 20 RightAdd False
             , mul  20  80  1 RightAdd
             , mul 100 100 10 RightAdd
             ]
           ⊕ scale RightAdd 3

repr ∷ (IsString s) ⇒ Repr s
repr = Repr { reprValue = \n → IM.lookup (fromInteger n) symMap
            , reprAdd   = (⊞)
            , reprMul   = (⊡)
            , reprZero  = "zeró"
            , reprNeg   = "moins"
            }
    where
      C _  ⊞ C 10           = ""
      _    ⊞ C 10           = "-"
      (C 4 :⋅: C 20) ⊞ _    = "-"
      _    ⊞ C 1            = " et "
      _    ⊞ (C 1 :+: C 10) = " et "
      C 10 ⊞ _              = "-"
      _    ⊞ _              = "-"

      _ ⊡ 20 = "-"
      _ ⊡ _  = ""


      symMap = IM.fromList
               [ (0, const "zéro")
               , (1, ten   "un"     "on"     "un")
               , (2, ten   "deux"   "dou"    "deux")
               , (3, ten   "trois"  "trei"   "tren")
               , (4, ten   "quatre" "quator" "quar")
               , (5, ten   "cinq"   "quin"   "cinqu")
               , (6, ten   "six"    "sei"    "soix")
               , (7, const "sept")
               , (8, const "huit")
               , (9, const "neuf")
               , (10, \c → case c of
                             RA (C n) _ | n < 7     → "ze"
                                        | otherwise → "dix"
                             RM (C 3) _ → "te"
                             RM {}      → "ante"
                             _          → "dix"
                 )
               , (20,   \c → case c of
                               RM _ Empty → "vingts"
                               _          → "vingt"
                 )
               , (100,  const "cent")
               , (1000, const "mille")
               ]

      ten ∷ s → s → s → SymbolContext → s
      ten n a m ctx = case ctx of
                        LA (C 10) _ → a
                        LM (C 10) _ → m
                        _           → n
