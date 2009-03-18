{-# LANGUAGE OverloadedStrings #-}

module Text.Numeral.Language.EN (enShort, enLong) where

import Data.String
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Pelletier (shortScale, longScale)


enShort :: (IsString s, Joinable s) => NumConfig s
enShort = NumConfig { ncNeg      = enNeg
                    , ncOne      = enOne
                    , ncAdd      = enAdd
                    , ncMul      = enMul
                    , ncCardinal = findSym $ enTable ++ shortScale "illion"
                    }

enLong :: (IsString s, Joinable s) => NumConfig s
enLong = enShort { ncCardinal = findSym $ enTable ++ longScale "illion" "illiard"}

enNeg :: (IsString s, Joinable s) => s -> s
enNeg = ("minus" <+>)

enOne :: (IsString s, Joinable s) => (Integer, s) -> s
enOne (v,  vs) | v >= 100  = "one" <+> vs
               | otherwise = vs

enAdd :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
enAdd (x, x') (_, y') | x < 20    = y' <> x'
                      | x < 100   = x' <-> y'
                      | otherwise = x' <+> y'

enMul :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
enMul (_, x') (y, y') | y == 10   = x' <> y'
                      | otherwise = x' <+> y'

enTable :: (IsString s, Joinable s) => [NumSymbol s]
enTable = [ term 0    $ const "zero"
          , term 1    $ const "one"
          , term 2    $ tenForms "two"   "twen" "twen"
          , term 3    $ tenForms "three" "thir" "thir"
          , term 4    $ tenForms "four"  "four" "for"
          , term 5    $ tenForms "five"  "fif"  "fif"
          , term 6    $ const "six"
          , term 7    $ const "seven"
          , term 8    $ tenForms "eight" "eigh" "eigh"
          , term 9    $ const "nine"
          , mul  10   $ \ctx -> case ctx of
                                   LA {} -> "teen"
                                   RM {} -> "ty"
                                   _     -> "ten"
          , term 11   $ const "eleven"
          , term 12   $ const "twelve"
          , mul  100  $ const "hundred"
          , mul  1000 $ const "thousand"
          ]
