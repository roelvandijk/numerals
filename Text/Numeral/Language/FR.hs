{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.FR
    ( cardinal
    , findRule
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>>=) )
import Data.Bool     ( otherwise )
import Data.Function ( const )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier ( scale )


--------------------------------------------------------------------------------
-- FR
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/french.html
  http://www.french-linguistics.co.uk/tutorials/numbers/
-}

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal n = deconstruct findRule n >>= textify cardinalRepr

findRule ∷ (Integral α, Num β) ⇒ FindRule α β
findRule = mkFindRule rules (scale 3 R L)

rules ∷ (Integral α, Num β) ⇒ Rules α β
rules = [ ((  0,  10), atom)
        , (( 11,  16), add 10 L)
        , (( 17,  19), add 10 R)
        , (( 20,  20), atom)
        , (( 21,  29), add 20 R)
        , (( 30,  69), mul 10 R L)
        , (( 70,  79), add 60 R)
        , (( 80,  89), mul 20 R L)
        , (( 89,  99), add 80 R)
        , ((100, 100), atom)
        , ((101, 199), add 100 R)
        , ((200, 999), mul 100 R L)
        ]

cardinalRepr ∷ IsString s ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd   = (⊞)
         , reprMul   = (⊡)
         , reprNeg   = "moins "
         }
    where
      C _  ⊞ C 10           = ""
      _    ⊞ C 10           = "-"
      (C 4 :*: C 20) ⊞ _    = "-"
      _    ⊞ C 1            = " et "
      _    ⊞ (C 1 :+: C 10) = " et "
      C 10 ⊞ _              = "-"
      _    ⊞ _              = "-"

      _ ⊡ 20 = "-"
      _ ⊡ _  = ""

      symMap = M.fromList
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

      ten n a m ctx = case ctx of
                        LA (C 10) _ → a
                        LM (C 10) _ → m
                        _           → n
