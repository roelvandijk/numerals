{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        to

[@ISO639-2@]        ton

[@ISO639-3@]        ton

[@Native name@]     lea faka-Tonga

[@English name@]    Tongan
-}

module Text.Numeral.Language.NLD
    ( ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Monad ( (>=>) )
import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integer )
import "positional-numerals" Text.Numeral.Positional ( toPositional )

--------------------------------------------------------------------------------
-- TO
--------------------------------------------------------------------------------

cardinal :: (Monoid s, IsString s) => Integer -> Maybe s
cardinal n = toPositional f 10 n
    where
      f :: Integer -> s
      f 0 = "noa"
      f 1 = "taha"
      f 2 = "ua"
      f 3 = "tolu"
      f 4 = "fā"
      f 5 = "nima"
      f 6 = "ono"
      f 7 = "fitu"
      f 8 = "valu"
      f 9 = "hiva"
      f _ = "?"
