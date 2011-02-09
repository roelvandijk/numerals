{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.JA (ja) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( otherwise )
import Data.Function ( const, ($) )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integer )

-- from base-unicode-symbols:
import Data.Ord.Unicode  ( (≥) )
import Data.Bool.Unicode ( (∧), (∨) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Misc (d, withSnd)


--------------------------------------------------------------------------------
-- JA
--------------------------------------------------------------------------------

ja ∷ (IsString s, Joinable s) ⇒ NumConfig s
ja = NumConfig { ncNeg      = ("mainasu" <+>)
               , ncOne      = jaOne
               , ncAdd      = withSnd (<+>)
               , ncMul      = withSnd (<->)
               , ncCardinal = findSym jaTable
               }

jaOne ∷ (IsString s, Joinable s) ⇒ (Integer, s) → s
jaOne (v, vs) | v < 100 ∨ (300 ≥ v ∧ v < 400) = vs
              | otherwise = "ichi" <-> vs

jaTable ∷ (IsString s, Joinable s) ⇒ [NumSymbol s]
jaTable = [ term 0         $ const "rei"
          , term 1         $ const "ichi"
          , term 2         $ const "ni"
          , term 3         $ const "san"
          , term 4         $ const "yon"
          , term 5         $ const "go"
          , term 6         $ const "roku"
          , term 7         $ const "nana"
          , term 8         $ const "hachi"
          , term 9         $ const "kyū"
          , mul 10         $ const "jū"
          , mul 100        $ const "hyaku"
          , add 300    100 $ const "san-byaku" -- rendaku
          , mul 1000       $ const "sen"
          , mul (d 4)      $ const "man"
          , mul (d 8)      $ const "oku"
          , mul (d 12)     $ const "chō"
          , mul (d 16)     $ const "kei"
          , mul (d 20)     $ const "gai"
          , mul (d 24)     $ const "jo"
          , mul (d 28)     $ const "jō"
          , mul (d 32)     $ const "kō"
          , mul (d 36)     $ const "kan"
          , mul (d 40)     $ const "sei"
          , mul (d 44)     $ const "sai"
          , mul (d 48)     $ const "goku"
          , mul (d 52)     $ const "gōgasha"
          , mul (d 56)     $ const "asōgi"
          , mul (d 60)     $ const "nayuta"
          , mul (d 64)     $ const "fukashigi"
          , mul (d 68)     $ const "muryōtaisū"
          ]
