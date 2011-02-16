{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.DE (de, rules, de_repr) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool(False), otherwise )
import Data.Function ( const )
import Data.List     ( map )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Num, fromInteger )

-- from base-unicode-symbols:
import Data.Bool.Unicode   ( (∨) )
import Data.Eq.Unicode     ( (≡) )
import Data.Monoid.Unicode ( (⊕) )
import Data.Ord.Unicode    ( (≥) )

-- from containers:
import qualified Data.IntMap as IM ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc      ( dec )
import Text.Numeral.Pelletier ( scale )


-------------------------------------------------------------------------------
-- DE
-------------------------------------------------------------------------------

de ∷ (IsString s) ⇒ (Rules, Repr s)
de = (rules, de_repr)

rules ∷ Rules
rules = Rules { rsFindRule = findRule rs
              , rsMulOne   = (≥ 100)
              }
    where
      rs = map atom [1..9]
         ⊕ [mul 10 10 10 LeftAdd]
         ⊕ map atom [11..12]
         ⊕ [mul 100 100 10 RightAdd]
         ⊕ scale RightAdd 3

de_repr ∷ (IsString s) ⇒ Repr s
de_repr =
    Repr { reprValue = \n → IM.lookup (fromInteger n) symMap
         , reprAdd   = (⊞)
         , reprMul   = \_ _ → ""
         , reprZero  = "null"
         , reprNeg   = "minus"
         }
    where
      _   ⊞ C 10 = ""
      C n ⊞ _ | n < 10        = "und"
              | otherwise     = ""
      _   ⊞ _ = " "

      symMap = IM.fromList
               [ (1, \c → case c of
                            LA {} → "ein"
                            LM (C n) _ | n ≥ dec 6 → "eine"
                                       | n ≥ 100   → "ein"
                            _ → "eins"
                 )
               , (2, \c → case c of
                            LM (C 10) _ → "zwan"
                            _           → "zwei"
                 )
               , (3, const "drei")
               , (4, const "vier")
               , (5, const "fünf")
               , (6, const "sechs")
               , (7, \c → case c of
                            LA (C 10) _ → "sieb"
                            LM (C 10) _ → "sieb"
                            _           → "sieben"
                 )
               , (8, const "acht")
               , (9, const "neun")
               , (10, \c → case c of
                             RM (C 3) _ → "ßig"
                             RM (C _) _ → "zig"
                             _          → "zehn"
                 )
               , (11, const "elf")
               , (12, const "zwölf")
               , (100, const "hundert")
               , (1000, const "tausend")
               ]
