-- -*- coding: utf-8 -*-

{-# LANGUAGE OverloadedStrings #-}

module Text.Numeral.Language.NL (nl) where

import Data.String
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Pelletier (longScale)

nl :: (IsString s, Joinable s) => NumConfig s
nl = NumConfig { ncNeg      = ("min" <+>)
               , ncOne      = snd
               , ncAdd      = nlAdd
               , ncMul      = nlMul
               , ncCardinal = findSym nlTable
               }

nlAdd :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
nlAdd (x, x') (y, y') | x < 20    = y' <> x'
                      | x < 100   = y' <> (if y == 2 || y == 3
                                           then "ën"
                                           else "en")
                                       <> x'
                      | otherwise = x' <+> y'

nlMul :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
nlMul (_, x') (y, y') | y <= 10   = x' <> y'
                      | otherwise = x' <+> y'

nlTable :: (IsString s, Joinable s) => [NumSymbol s]
nlTable = [ term 0    $ const "nul"
          , term 1    $ const "één"
          , term 2    $ tenForms "twee" "twin" "twin"
          , term 3    $ tenForms "drie" "der"  "der"
          , term 4    $ tenForms "vier" "veer" "veer"
          , term 5    $ const "vijf"
          , term 6    $ const "zes"
          , term 7    $ const "zeven"
          , term 8    $ tenForms "acht" "tach" "acht"
          , term 9    $ const "negen"
          , mul  10   $ \ctx -> case ctx of
                                   RM {} -> "tig"
                                   _     -> "tien"
          , term 11   $ const "elf"
          , term 12   $ const "twaalf"
          , mul  100  $ const "honderd"
          , mul  1000 $ const "duizend"
          ] ++ (longScale "iljoen" "iljard")
