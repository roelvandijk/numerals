{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.EO
    ( cardinal
    , rules
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool(False) )
import Data.Function ( const )
import Data.List     ( map )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, fromInteger )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Monoid.Unicode   ( (⊕) )

-- from containers:
import qualified Data.IntMap as IM ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier ( scale )


--------------------------------------------------------------------------------
-- EO
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral i) ⇒ i → Maybe s
cardinal = textify cardinalRepr ∘ deconstruct rules

rules ∷ (Integral i) ⇒ Rules i
rules = Rules { rsFindRule = findRule rs
              , rsMulOne   = const False
              }
    where
      rs = map atom [1..9]
         ⊕ [ mul 10  10  10 RightAdd
           , mul 100 100 10 RightAdd
           ]
         ⊕ scale RightAdd 3

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → IM.lookup (fromInteger n) symMap
         , reprAdd  = (⊞)
         , reprMul  = (⊡)
         , reprZero = "nul"
         , reprNeg  = "ne " -- ???
         }
    where
      _ ⊞ _ = " "
      _ ⊡ _ = ""

      symMap = IM.fromList
               [ (1, const "unu")
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
