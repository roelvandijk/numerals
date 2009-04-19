{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Text.Numeral.Joinable where

import Data.Monoid
import Data.String

import qualified Data.DString          as DS
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Text.PrettyPrint      as PP


-- | Class of string-like types which can be joined.
class Joinable s where
    (<>)  :: s -> s -> s
    (<+>) :: s -> s -> s

infixr 5 <>, <+>, <->

(<->) :: (Joinable s, IsString s) => s -> s -> s
x <-> y = x <> "-" <> y

space :: (Joinable s, IsString s) => s -> s -> s
space x y = x <> " " <> y

instance Joinable String where
    (<>)  = mappend
    (<+>) = space

instance Joinable B.ByteString where
    (<>)  = mappend
    (<+>) = space

instance Joinable T.Text where
    (<>)  = mappend
    (<+>) = space

instance Joinable ShowS where
    (<>)  = mappend
    (<+>) = space

instance Joinable DS.DString where
    (<>)  = mappend
    (<+>) = space

instance Joinable PP.Doc where
    (<>)  = (PP.<>)
    (<+>) = (PP.<+>)

-------------------------------------------------------------------------------

instance IsString ShowS where
    fromString = showString

instance IsString PP.Doc where
    fromString = PP.text
