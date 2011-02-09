{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.PT (pt) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( otherwise )
import Data.Function ( const, ($) )
import Data.List     ( (++) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integer, error )

-- from base-unicode-symbols:
import Data.Eq.Unicode  ( (≡) )
import Data.Ord.Unicode ( (≤) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier (shortScalePlural)

-- from string-combinators:
import Data.String.Combinators ( (<>), (<+>) )


-------------------------------------------------------------------------------
-- PT
-------------------------------------------------------------------------------

-- Sources:
--   http://www.sonia-portuguese.com/text/numerals.htm
--   http://www.smartphrase.com/Portuguese/po_numbers_voc.shtml

pt ∷ (Monoid s, IsString s) ⇒ NumConfig s
pt = NumConfig { ncNeg      = error "ptNeg: undefined"
               , ncOne      = ptOne
               , ncAdd      = ptAdd
               , ncMul      = ptMul
               , ncCardinal = findSym ptTable
               }

ptOne ∷ (Monoid s, IsString s) ⇒ (Integer, s) → s
ptOne (x, x') | x ≤ 1000  = x'
              | otherwise = "um" <+> x'

-- TODO: When to use "e" is still unclear.
ptAdd ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
ptAdd (x, x') (_, y') | x ≡ 10    = y' <> x'
                      | otherwise = x' <+> "e" <+> y'

ptMul ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
ptMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

ptTable ∷ (Monoid s, IsString s) ⇒ [NumSymbol s]
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
          , mul  10        $ \ctx → case ctx of
                                      LA _ _ → "ze"
                                      RM 3 _ → "ta"
                                      RM _ _ → "enta"
                                      _      → "dez"
          , term 16        $ const "dezesseis"
          , term 17        $ const "dezessete"
          , term 18        $ const "dezoito"
          , term 19        $ const "dezenove"
          , add  20    10  $ const "vinte"
          , mul  100       $ \ctx → case ctx of
                                      RM _ _ → "centos"
                                      LA _ _ → "cento"
                                      _      → "cem"
          , add  200   100 $ const "duzentos"
          , add  300   100 $ const "trezentos"
          , add  500   100 $ const "quinhentos"
          , mul  1000      $ const "mil"
          ] ++ shortScalePlural "ilhão" "ilhões"
