-- -*- coding: utf-8 -*-

{-# LANGUAGE OverloadedStrings #-}

module Text.Numeral.Language.SP (sp) where

import Data.String
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Misc (d)


-- Sources:
--   http://spanish.about.com/cs/forbeginners/a/cardinalnum_beg.htm
--   http://www.learn-spanish-help.com/count-in-spanish.html
--   http://www.donquijote.org/spanishlanguage/numbers/numbers1.asp

spAdd :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
spAdd (x, x') (y, y') | x == 10 && y < 6 = y' <> x'
                      | x == 10    = x' <> y'
                      | x < 30     = x' <>  "i" <>  y'
                      | x < 100    = x' <+> "y" <+> y'
                      | otherwise  = x' <+> y'

spMul :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
spMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

spTable :: (IsString s, Joinable s) => [NumSymbol s]
spTable = [ term  0         $ const "cero"
          , termG 1         $ tenFormsG (genderN "uno" "un" "una") (const "on") (const "uno")
          , term  2         $ \ctx -> case ctx of
                                         RA 10 _ -> "do"
                                         RA 20 _ -> "dós"
                                         _       -> "dos"
          , term  3         $ \ctx -> case ctx of
                                         RA n _ | n == 10 -> "tre"
                                                | n < 100 -> "trés"
                                         LM 10 _ -> "trein"
                                         _       -> "tres"
          , term  4         $ tenForms "cuatro" "cator" "cuaren"
          , term  5         $ tenForms "cinco"  "quin"  "cincuen"
          , term  6         $ \ctx -> case ctx of
                                         RA n _ | n <= 20 -> "séis"
                                         LM 10 _ -> "sesen"
                                         _       -> "seis"
          , term  7         $ tenForms' "siete" "siete" "seten" "sete"
          , term  8         $ tenForms  "ocho"  "ocho"  "ochen"
          , term  9         $ tenForms' "nueve" "nueve" "noven" "novo"
          , mul   10        $ \ctx -> case ctx of
                                         LA n _ | n < 6     -> "ce"
                                                | otherwise -> "dieci"
                                         RM _ _ -> "ta"
                                         _      -> "diez"
          , add   20    10  $ const "veint"
          , mul   100       $ \ctx -> case ctx of
                                         RM _ _ -> "cientos"
                                         LA _ _ -> "ciento"
                                         _      -> "cien"
          , add   500   100 $ const "quinientos"
          , mul   1000      $ const "mil"
          , mul   (d 6)     $ mulForms "millón" "millones"
          , mul   (d 12)    $ mulForms "billón" "billones"
          ]

sp :: (IsString s, Joinable s) => NumConfig s
sp = NumConfig { ncNeg      = error "spNeg: undefined"
               , ncOne      = snd
               , ncAdd      = spAdd
               , ncMul      = spMul
               , ncCardinal = findSym spTable
               }
