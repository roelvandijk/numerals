{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.EO (eo) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( const, ($) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Data.Tuple    ( snd )
import Prelude       ( error )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc (d, withSnd)

-- from string-combinators:
import Data.String.Combinators ( (<>), (<+>) )


--------------------------------------------------------------------------------
-- EO
--------------------------------------------------------------------------------

eo ∷ (Monoid s, IsString s) ⇒ NumConfig s
eo = NumConfig { ncNeg      = error "eoNeg: undefined"
               , ncOne      = snd
               , ncAdd      = withSnd (<+>)
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym eoTable
               }

eoTable ∷ (Monoid s, IsString s) ⇒ [NumSymbol s]
eoTable = [ term 0    $ const "nulo"
          , term 1    $ const "unu"
          , term 2    $ const "du"
          , term 3    $ const "tri"
          , term 4    $ const "kvar"
          , term 5    $ const "kvin"
          , term 6    $ const "ses"
          , term 7    $ const "sep"
          , term 8    $ const "ok"
          , term 9    $ const "naŭ"
          , mul 10    $ const "dek"
          , mul 100   $ const "cent"
          , mul 1000  $ const "mil"
          , mul (d 6) $ const "miliono"
          ]
