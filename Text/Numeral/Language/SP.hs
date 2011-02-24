{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.SP (sp) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- import base:
import Data.Bool     ( otherwise )
import Data.Function ( const, ($) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Data.Tuple    ( snd )
import Prelude       ( Integer, error )

-- from base-unicode-symbols:
import Data.Bool.Unicode ( (∧) )
import Data.Eq.Unicode   ( (≡) )
import Data.Ord.Unicode  ( (≤) )

-- import numerals:
import Text.Numeral
import Text.Numeral.Misc (d)

-- from string-combinators:
import Data.String.Combinators ( (<>), (<+>) )


-------------------------------------------------------------------------------
-- SP
-------------------------------------------------------------------------------

-- Sources:
--   http://spanish.about.com/cs/forbeginners/a/cardinalnum_beg.htm
--   http://www.learn-spanish-help.com/count-in-spanish.html
--   http://www.donquijote.org/spanishlanguage/numbers/numbers1.asp

sp ∷ (Monoid s, IsString s) ⇒ NumConfig s
sp = NumConfig { ncNeg      = error "spNeg: undefined"
               , ncOne      = snd
               , ncAdd      = spAdd
               , ncMul      = spMul
               , ncCardinal = findSym spTable
               }

spAdd ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
spAdd (x, x') (y, y') | x ≡ 10 ∧ y < 6 = y' <> x'
                      | x ≡ 10     = x' <> y'
                      | x < 30     = x' <>  "i" <>  y'
                      | x < 100    = x' <+> "y" <+> y'
                      | otherwise  = x' <+> y'

spMul ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
spMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

spTable ∷ (Monoid s, IsString s) ⇒ [NumSymbol s]
spTable = [ term  0         $ const "cero"
          , termG 1         $ tenFormsG (genderN "uno" "un" "una") (const "on") (const "uno")
          , term  2         $ \ctx → case ctx of
                                       AddR 10 _ → "do"
                                       AddR 20 _ → "dós"
                                       _       → "dos"
          , term  3         $ \ctx → case ctx of
                                       AddR n _ | n ≡ 10 → "tre"
                                              | n < 100 → "trés"
                                       MulL 10 _ → "trein"
                                       _       → "tres"
          , term  4         $ tenForms "cuatro" "cator" "cuaren"
          , term  5         $ tenForms "cinco"  "quin"  "cincuen"
          , term  6         $ \ctx → case ctx of
                                       AddR n _ | n ≤ 20 → "séis"
                                       MulL 10 _ → "sesen"
                                       _       → "seis"
          , term  7         $ tenForms' "siete" "siete" "seten" "sete"
          , term  8         $ tenForms  "ocho"  "ocho"  "ochen"
          , term  9         $ tenForms' "nueve" "nueve" "noven" "novo"
          , mul   10        $ \ctx → case ctx of
                                       AddL n _ | n < 6     → "ce"
                                              | otherwise → "dieci"
                                       MulR _ _ → "ta"
                                       _      → "diez"
          , add   20    10  $ const "veint"
          , mul   100       $ \ctx → case ctx of
                                       MulR _ _ → "cientos"
                                       AddL _ _ → "ciento"
                                       _      → "cien"
          , add   500   100 $ const "quinientos"
          , mul   1000      $ const "mil"
          , mul   (d 6)     $ mulForms "millón" "millones"
          , mul   (d 12)    $ mulForms "billón" "billones"
          ]
