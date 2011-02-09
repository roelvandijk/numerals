-- -*- coding: utf-8 -*-

{-# LANGUAGE CPP, OverloadedStrings #-}

module Text.Numeral.Language.SV (sv) where

import Data.String
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Misc (d, withSnd)



-- Sources:
--   http://www.lysator.liu.se/language/Languages/Swedish/Grammar.html
--   http://en.wikibooks.org/wiki/Swedish/Numerals
--   http://longstrom.com/swedishtoenglish.htm#numbers

--   http://www.cs.chalmers.se/~aarne/GF/
--   http://www.cs.chalmers.se/~aarne/GF/lib/resource/norwegian/NumeralNor.gf

sv :: (IsString s, Joinable s) => NumConfig s
sv = NumConfig { ncNeg      = ("minus" <+>)
               , ncOne      = svOne
               , ncAdd      = svAdd
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym svTable
               }

svOne :: (IsString s, Joinable s) => (Integer, s) -> s
svOne (v, vs) | v >= 100   = "ett" <> vs
              | otherwise  = vs

svAdd :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
svAdd (x, x') (_, y') | x < 20    = y' <> x'
                      | otherwise = x' <> y'

svTable :: (IsString s, Joinable s) => [NumSymbol s]
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
          , mul  10       $ \ctx -> case ctx of
                                       LA {} -> "ton"
                                       _     -> "tio"
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
