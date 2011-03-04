{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , PatternGuards
           , RecordWildCards
           , NamedFieldPuns
           , ScopedTypeVariables
           , OverloadedStrings
  #-}

module Text.Numeral
    ( module Text.Numeral.Exp
    , module Text.Numeral.Repr
    , module Text.Numeral.Rules
    , mkCardinal
    )
    where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>>=) )
import Data.Function ( fix )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral )

-- from numerals:
import Text.Numeral.Exp
import Text.Numeral.Repr
import Text.Numeral.Rules


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

mkCardinal ∷ (Monoid s, IsString s, Integral α)
           ⇒ Rule α Exp → Repr s → α → Maybe s
mkCardinal rule repr = \n → positive (fix rule) n >>= textify repr
