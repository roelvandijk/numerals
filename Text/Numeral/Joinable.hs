{-# LANGUAGE OverloadedStrings
           , NoImplicitPrelude
           , TypeSynonymInstances
           , UnicodeSyntax
  #-}

module Text.Numeral.Joinable where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Char   ( String )
import Data.String ( IsString, fromString )
import Text.Show   ( ShowS, showString )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )

-- from bytestring:
import qualified Data.ByteString.Char8 as B

-- from dstring:
import qualified Data.DString as DS

-- from pretty:
import qualified Text.PrettyPrint as PP

-- from text:
import qualified Data.Text as T


--------------------------------------------------------------------------------
-- Joinable class
--------------------------------------------------------------------------------

-- | Class of string-like types which can be joined.
class Joinable s where
    (<>)  ∷ s → s → s
    (<+>) ∷ s → s → s

infixr 5 <>, <+>, <->

(<->) ∷ (Joinable s, IsString s) ⇒ s → s → s
x <-> y = x <> "-" <> y

space ∷ (Joinable s, IsString s) ⇒ s → s → s
space x y = x <> " " <> y


-------------------------------------------------------------------------------
-- Joinable instances
-------------------------------------------------------------------------------

instance Joinable String where
    (<>)  = (⊕)
    (<+>) = space

instance Joinable B.ByteString where
    (<>)  = (⊕)
    (<+>) = space

instance Joinable T.Text where
    (<>)  = (⊕)
    (<+>) = space

instance Joinable ShowS where
    (<>)  = (⊕)
    (<+>) = space

instance Joinable DS.DString where
    (<>)  = (⊕)
    (<+>) = space

instance Joinable PP.Doc where
    (<>)  = (PP.<>)
    (<+>) = (PP.<+>)


-------------------------------------------------------------------------------
-- Miscellaneous IsString instances
-------------------------------------------------------------------------------

instance IsString ShowS where
    fromString = showString

instance IsString PP.Doc where
    fromString = PP.text
