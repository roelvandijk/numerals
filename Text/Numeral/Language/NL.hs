{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.NL (nl) where

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
import Prelude       ( Integer )

-- from base-unicode-symbols:
import Data.Bool.Unicode   ( (∨) )
import Data.Eq.Unicode     ( (≡) )
import Data.Ord.Unicode    ( (≤) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Pelletier ( longScale )

-- from string-combinators:
import Data.String.Combinators ( (<>), (<+>) )


--------------------------------------------------------------------------------
-- NL
--------------------------------------------------------------------------------

nl ∷ (Monoid s, IsString s) ⇒ NumConfig s
nl = NumConfig { ncNeg      = ("min" <+>)
               , ncOne      = snd
               , ncAdd      = nlAdd
               , ncMul      = nlMul
               , ncCardinal = findSym nlTable
               }

nlAdd ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
nlAdd (x, x') (y, y') | x < 20    = y' <> x'
                      | x < 100   = y' <> (if y ≡ 2 ∨ y ≡ 3
                                           then "ën"
                                           else "en")
                                       <> x'
                      | otherwise = x' <+> y'

nlMul ∷ (Monoid s, IsString s) ⇒ (Integer, s) → (Integer, s) → s
nlMul (_, x') (y, y') | y ≤ 10    = x' <> y'
                      | otherwise = x' <+> y'

nlTable ∷ (Monoid s, IsString s) ⇒ [NumSymbol s]
nlTable = [ term 0    $ const "nul"
          , term 1    $ const "één"
          , term 2    $ tenForms "twee" "twin" "twin"
          , term 3    $ tenForms "drie" "der"  "der"
          , term 4    $ tenForms "vier" "veer" "veer"
          , term 5    $ const "vijf"
          , term 6    $ const "zes"
          , term 7    $ const "zeven"
          , term 8    $ tenForms "acht" "acht" "tach"
          , term 9    $ const "negen"
          , mul  10   $ \ctx → case ctx of
                                 RM {} → "tig"
                                 _     → "tien"
          , term 11   $ const "elf"
          , term 12   $ const "twaalf"
          , mul  100  $ const "honderd"
          , mul  1000 $ const "duizend"
          ] ++ (longScale "iljoen" "iljard")
