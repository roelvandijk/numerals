{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.LA (la) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( otherwise )
import Data.Function ( const, ($) )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Data.Tuple    ( snd )
import Prelude       ( Integer, error )

-- from base-unicode-symbols:
import Data.Eq.Unicode   ( (≡) )
import Data.List.Unicode ( (∈) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Misc (d)


--------------------------------------------------------------------------------
-- LA
--------------------------------------------------------------------------------

la ∷ (IsString s, Joinable s) ⇒ NumConfig s
la = NumConfig { ncNeg      = error "laNeg: undefined"
               , ncOne      = snd
               , ncAdd      = laAdd
               , ncMul      = laMul
               , ncCardinal = findSym laTable
               }

laAdd ∷ (IsString s, Joinable s) ⇒ (Integer, s) → (Integer, s) → s
laAdd (x, x') (_, y') | x ≡ 10   = y' <> x'
                      | otherwise = x' <+> y'

laMul ∷ (IsString s, Joinable s) ⇒ (Integer, s) → (Integer, s) → s
laMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

laTable ∷ (IsString s, Joinable s) ⇒ [NumSymbol s]
laTable = [ term 0     $ const "nulla"
          , term 1     $ tenForms  "unus"     "un"       "unus"
          , term 2     $ tenForms' "duo"      "duo"      "vi"      "du"
          , term 3     $ tenForms' "tres"     "tre"      "tri"     "tre"
          , term 4     $ tenForms' "quattuor" "quattuor" "quadra"  "quadri"
          , term 5     $ tenForms' "quinque"  "quin"     "quinqua" "quin"
          , term 6     $ tenForms' "sex"      "se"       "sexa"    "ses"
          , term 7     $ tenForms' "septem"   "septen"   "septua"  "septin"
          , term 8     $ const "octo"
          , term 9     $ tenForms' "novem"    "novem"    "nona"    "non"
          , mul  10    $ \ctx → case ctx of
                                    LA _ _ → "decim"
                                    RM 2 _ → "ginti"
                                    RM _ _ → "ginta"
                                    _      → "decem"
          , term 18    $ const "duodeviginti"
          , term 19    $ const "undeviginti"
          , term 28    $ const "duodetriginta"
          , term 29    $ const "undetriginta"
          , term 38    $ const "duodequadraginta"
          , term 39    $ const "undequadraginta"
          , term 48    $ const "duodequinquaginta"
          , term 49    $ const "undequinquaginta"
          , term 58    $ const "duodesexaginta"
          , term 59    $ const "undesexaginta"
          , term 68    $ const "duodeseptuaginta"
          , term 69    $ const "undeseptuaginta"
          , term 78    $ const "duodeoctoginta"
          , term 79    $ const "undeoctoginta"
          , term 88    $ const "duodenonaginta"
          , term 89    $ const "undenonaginta"
          , term 98    $ const "duodecentum"
          , term 99    $ const "undecentum"
          , mul  100   $ \ctx → case ctx of
                                    RM n _ | n ∈ [2, 3, 6] → "centi"
                                           | otherwise     → "genti"
                                    _                      → "centum"
          , mul  1000  $ mulForms "mille" "millia"
          , term (d 6) $ const "decies centena milia"
          ]
