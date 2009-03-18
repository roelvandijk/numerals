-- -*- coding: utf-8 -*-

{-# LANGUAGE OverloadedStrings #-}

module Text.Numeral.Language.NO (no) where

import Data.String
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Misc (d, withSnd)


-- Sources:
--   http://en.wikibooks.org/wiki/Norwegian_Numbers

no :: (IsString s, Joinable s) => NumConfig s
no = NumConfig { ncNeg      = error "noNeg: undefined"
               , ncOne      = noOne
               , ncAdd      = noAdd
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym noTable
               }

noOne :: (IsString s, Joinable s) => (Integer, s) -> s
noOne (v, vs) | v >= (d 6) = "én" <+> vs
              | otherwise  = vs

-- TODO: What are the rules for conjunction in Norse? When do you put
-- "og" between numbers?
noAdd :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
noAdd (x, x') (_, y') | x < 20    = y' <> x'
                      | otherwise = x' <> y'

noTable :: (IsString s, Joinable s) => [NumSymbol s]
noTable = [ term 0        $ const "null"
          , term 1        $ const "én"
          , term 2        $ const "to"
          , term 3        $ tenForms "tre"  "tret" "tret"
          , term 4        $ tenForms "fire" "fjor" "før"
          , term 5        $ const "fem"
          , term 6        $ const "seks"
          , term 7        $ tenForms "sju"  "syt" "syt"
          , term 8        $ tenForms "åtte" "at"  "åt"
          , term 9        $ tenForms "ni"   "nit" "nit"
          , mul  10       $ \ctx -> case ctx of
                                       LA {} -> "ten"
                                       _     -> "ti"
          , term 11       $ const "elleve"
          , term 12       $ const "tolv"
          , add  20    10 $ const "tjue"
          , mul  100      $ const "hundre"
          , mul  (d 3)    $ const "tusen"
          , mul  (d 6)    $ mulForms "million"  "millioner"
          , mul  (d 9)    $ mulForms "milliard" "milliarder"
          ]
