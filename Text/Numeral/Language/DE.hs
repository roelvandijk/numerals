{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.DE (de) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( otherwise )
import Data.Function ( const, ($) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integer )

-- from base-unicode-symbols:
import Data.Eq.Unicode  ( (≡) )
import Data.Ord.Unicode ( (≥) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( d, withSnd )

-- from string-combinators:
import Data.String.Combinators ( (<>), (<+>) )


-------------------------------------------------------------------------------
-- DE
-------------------------------------------------------------------------------

de ∷ (Monoid s, IsString s) ⇒ NumConfig s
de = NumConfig { ncNeg      = ("minus" <+>)
               , ncOne      = deOne
               , ncAdd      = deAdd
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym deTable
               }

deOne ∷ (Monoid s, IsString s) ⇒ (Integer, s) → s
deOne (v, vs) | v ≥ (d 6) = "eine" <+> vs
              | v ≥ 100   = "ein"  <>  vs
              | otherwise = vs

deAdd ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
deAdd (x, x') (y, y') | x < 20    = y' <> x'
                      | x < 100   = (if y ≡ 1
                                     then "ein"
                                     else y') <> "und" <> x'
                      | otherwise = x' <> y'

deTable ∷ (Monoid s, IsString s) ⇒ [NumSymbol s]
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
