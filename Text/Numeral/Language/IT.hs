{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.IT (it) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( otherwise )
import Data.Function ( const, ($) )
import Data.List     ( (++) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Data.Tuple    ( snd )
import Prelude       ( Integer, error )

-- from base-unicode-symbols:
import Data.Eq.Unicode   ( (≡) )
import Data.Bool.Unicode ( (∧) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc      ( d )
import Text.Numeral.Pelletier ( longScalePlural )

-- from string-combinators:
import Data.String.Combinators ( (<>), (<+>) )


--------------------------------------------------------------------------------
-- IT
--------------------------------------------------------------------------------

-- Sources:
--   http://italian.about.com/library/weekly/aa042600a.htm


it ∷ (Monoid s, IsString s) ⇒ NumConfig s
it = NumConfig { ncNeg      = error "itNeg: undefined"
               , ncOne      = snd
               , ncAdd      = itAdd
               , ncMul      = itMul
               , ncCardinal = findSym itTable
               }

itAdd ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
itAdd (x, x') (y, y') | x ≡ 10 ∧ y < 7 = y' <> x'
                      | y ≡ 3    = x' <> "tré"
                      | otherwise = x' <> y'

itMul ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
itMul (_, x') (y, y') | y < d 6   = x' <> y'
                      | otherwise = x' <+> y'

itTable ∷ (Monoid s, IsString s) ⇒ [NumSymbol s]
itTable = [ term  0       $ const "zero"
          , termG 1       $ tenFormsG (gender "uno" "una") (const "un") (const "uno")
          , term  2       $ tenForms "due"     "do"      "due"
          , term  3       $ tenForms "tre"     "tre"     "ten"
          , term  4       $ tenForms "quattro" "quattor" "quar"
          , term  5       $ tenForms "cinque"  "quin"    "cinqu"
          , term  6       $ tenForms "sei"     "se"      "sess"
          , term  7       $ tenForms "sette"   "assette" "sett"
          , term  8       $ tenForms "otto"    "otto"    "ott"
          , term  9       $ tenForms "nove"    "annove"  "nove"
          , mul   10      $ \ctx → case ctx of
                                       LA _ _ → "dici"
                                       RM 3 _ → "ta"
                                       RM _ _ → "anta"
                                       _      → "dieci"
          , add   20   10 $ const "venti"
          , term  21      $ const "ventuno"
          , term  28      $ const "ventotto"
          , term  31      $ const "trentuno"
          , term  38      $ const "trentotto"
          , term  41      $ const "quarantuno"
          , term  48      $ const "quarantotto"
          , term  51      $ const "cinquantuno"
          , term  58      $ const "cinquantotto"
          , term  61      $ const "sessantuno"
          , term  68      $ const "sessantotto"
          , term  71      $ const "settantuno"
          , term  78      $ const "settantotto"
          , term  81      $ const "ottantuno"
          , term  88      $ const "ottantotto"
          , term  91      $ const "novantuno"
          , term  98      $ const "novantotto"
          , mul   100     $ const "cento"
          , mul   1000    $ mulForms "mille" "mila"
          ] ++ longScalePlural "ilione" "ilioni" "iliardo" "iliardi"
