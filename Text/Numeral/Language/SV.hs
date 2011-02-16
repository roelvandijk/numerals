{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.SV (sv, rules, sv_repr) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

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


-------------------------------------------------------------------------------
-- SV
-------------------------------------------------------------------------------

-- Sources:
--   http://www.lysator.liu.se/language/Languages/Swedish/Grammar.html
--   http://en.wikibooks.org/wiki/Swedish/Numerals
--   http://longstrom.com/swedishtoenglish.htm#numbers

--   http://www.cs.chalmers.se/~aarne/GF/
--   http://www.cs.chalmers.se/~aarne/GF/lib/resource/norwegian/NumeralNor.gf


rules ∷ Rules
rules = Rules { rsFindRule = findRule rs
              , rsMulOne   = const False
              }
    where
      rs = map atom [1..9]
           ⊕ [ mul  10  10 10 LeftAdd
             , mul 100 100 10 RightAdd
             ]
           ⊕ scale RightAdd 3

sv ∷ (Monoid s, IsString s) ⇒ NumConfig s
sv = NumConfig { ncNeg      = ("minus" <+>)
               , ncOne      = svOne
               , ncAdd      = svAdd
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym svTable
               }

svOne ∷ (Monoid s, IsString s) ⇒ (Integer, s) → s
svOne (v, vs) | v ≥ 100   = "ett" <> vs
              | otherwise  = vs

svAdd ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
svAdd (x, x') (_, y') | x < 20    = y' <> x'
                      | otherwise = x' <> y'

svTable ∷ (Monoid s, IsString s) ⇒ [NumSymbol s]
svTable = [ term 0        $ const "noll"
          , term 1        $ const "ett"
          , term 2        $ const "två"
          , term 3        $ tenForms "tre"  "tret" "tret"
          , term 4        $ tenForms "fyra" "fjor" "fyr"
          , term 5        $ const "fem"
          , term 6        $ const "sex"
          , term 7        $ tenForms "sju"  "sjut" "sjut"
          , term 8        $ tenForms "åtta" "ar"   "åt"
          , term 9        $ tenForms "nio"  "nit"  "nit"
          , mul  10       $ \ctx → case ctx of
                                     LA {} → "ton"
                                     _     → "tio"
          , term 11       $ const "elva"
          , term 12       $ const "tolv"
          , add  20    10 $ const "tjugo"
          , mul  100      $ const "hundra"
          , mul  (d 3)    $ const "tusen"
          , mul  (d 6)    $ mulForms "miljon"       "miljoner"
          , mul  (d 9)    $ mulForms "miljard"      "miljarder"
          , mul  (d 12)   $ mulForms "biljon"       "biljoner"
          , mul  (d 15)   $ mulForms "biljard"      "biljarder"
          , mul  (d 18)   $ mulForms "triljon"      "triljoner"
          , mul  (d 21)   $ mulForms "triljard"     "triljarder"
          , mul  (d 24)   $ mulForms "kvadriljon"   "kvadriljoner"
          , mul  (d 27)   $ mulForms "kvadriljard"  "kvadriljarder"
          , mul  (d 30)   $ mulForms "kvintriljon"  "kvintriljoner"
          , mul  (d 33)   $ mulForms "kvintriljard" "kvintriljarder"
          ]
