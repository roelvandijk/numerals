-- -*- coding: utf-8 -*-

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Text.Numeral.Language.DE (de) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- base
import Data.String

-- base-unicode-symbols
import Data.Eq.Unicode  ( (≡) )
import Data.Ord.Unicode ( (≥) )

-- numerals
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Misc ( d, withSnd )

#ifdef DO_SPECIALISE
import qualified Data.ByteString as B
import qualified Data.DString    as DS

{-# SPECIALISE de ∷ NumConfig String       #-}
{-# SPECIALISE de ∷ NumConfig B.ByteString #-}
{-# SPECIALISE de ∷ NumConfig DS.DString   #-}
#endif

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

de ∷ (IsString s, Joinable s) ⇒ NumConfig s
de = NumConfig { ncNeg      = ("minus" <+>)
               , ncOne      = deOne
               , ncAdd      = deAdd
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym deTable
               }

deOne ∷ (IsString s, Joinable s) ⇒ (Integer, s) → s
deOne (v, vs) | v ≥ (d 6) = "eine" <+> vs
              | v ≥ 100   = "ein"  <>  vs
              | otherwise = vs

deAdd ∷ (IsString s, Joinable s) ⇒ (Integer, s) → (Integer, s) → s
deAdd (x, x') (y, y') | x < 20    = y' <> x'
                      | x < 100   = (if y ≡ 1
                                     then "ein"
                                     else y') <> "und" <> x'
                      | otherwise = x' <> y'

deTable ∷ (IsString s, Joinable s) ⇒ [NumSymbol s]
deTable = [ term 0        $ const "null"
          , term 1        $ const "eins"
          , term 2        $ tenForms "zwei" "zwei" "zwan"
          , term 3        $ const "drei"
          , term 4        $ const "vier"
          , term 5        $ const "fünf"
          , term 6        $ const "sechs"
          , term 7        $ tenForms "sieben" "sieb" "sieb"
          , term 8        $ const "acht"
          , term 9        $ const "neun"
          , mul  10       $ mulForms "zehn" "zig"
          , term 11       $ const "elf"
          , term 12       $ const "zwölf"
          , add  30    10 $ const "dreißig"
          , mul  100      $ const "hundert"
          , mul  1000     $ const "tausend"
          , mul  (d 6)    $ const "million"
          , mul  (d 9)    $ const "milliarde"
          ]
