-- -*- coding: utf-8 -*-

{-# LANGUAGE OverloadedStrings #-}

module Text.Numeral.Language.PT (pt) where

import Data.String
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Pelletier (shortScalePlural)

-- Sources:
--   http://www.sonia-portuguese.com/text/numerals.htm
--   http://www.smartphrase.com/Portuguese/po_numbers_voc.shtml

ptOne :: (IsString s, Joinable s) => (Integer, s) -> s
ptOne (x, x') | x <= 1000 = x'
              | otherwise = "um" <+> x'

-- TODO: When to use "e" is still unclear.
ptAdd :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
ptAdd (x, x') (_, y') | x == 10   = y' <> x'
                      | otherwise = x' <+> "e" <+> y'

ptMul :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
ptMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

ptTable :: (IsString s, Joinable s) => [NumSymbol s]
ptTable = [ term 0         $ const "zero"
          , term 1         $ tenForms "um"     "on"    "um"
          , term 2         $ tenForms "dois"   "do"    "dois"
          , term 3         $ tenForms "três"   "tre"   "trin"
          , term 4         $ tenForms "quatro" "cator" "quar"
          , term 5         $ tenForms "cinco"  "quin"  "cinqü"
          , term 6         $ tenForms "seis"   "seis"  "sess"
          , term 7         $ tenForms "sete"   "sete"  "set"
          , term 8         $ tenForms "oito"   "oito"  "oit"
          , term 9         $ tenForms "nove"   "nove"  "nov"
          , mul  10        $ \ctx -> case ctx of
                                        LA _ _ -> "ze"
                                        RM 3 _ -> "ta"
                                        RM _ _ -> "enta"
                                        _      -> "dez"
          , term 16        $ const "dezesseis"
          , term 17        $ const "dezessete"
          , term 18        $ const "dezoito"
          , term 19        $ const "dezenove"
          , add  20    10  $ const "vinte"
          , mul  100       $ \ctx -> case ctx of
                                        RM _ _ -> "centos"
                                        LA _ _ -> "cento"
                                        _      -> "cem"
          , add  200   100 $ const "duzentos"
          , add  300   100 $ const "trezentos"
          , add  500   100 $ const "quinhentos"
          , mul  1000      $ const "mil"
          ] ++ shortScalePlural "ilhão" "ilhões"

pt :: (IsString s, Joinable s) => NumConfig s
pt = NumConfig { ncNeg      = error "ptNeg: undefined"
               , ncOne      = ptOne
               , ncAdd      = ptAdd
               , ncMul      = ptMul
               , ncCardinal = findSym ptTable
               }
