module Text.Numeral.Entry
    ( Conversion(..)
    , Entry(..)
    , emptyEntry
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "this" Text.Numeral.Exp ( Exp )
import "this" Text.Numeral.Grammar ( Inflection )
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- Language entry
--------------------------------------------------------------------------------

data Conversion a = Conversion
    { toNumeral   :: Inflection -> a -> Maybe Text
    , toStructure :: a -> Exp
    }

data Entry = Entry
    { entIso639_1       :: Maybe Text
    , entIso639_2       :: [Text]
    , entIso639_3       :: Maybe Text
    , entNativeNames    :: [Text]
    , entEnglishName    :: Maybe Text
    , entVariant        :: Maybe Text
    , entCardinal       :: Maybe (Conversion Integer)
    , entOrdinal        :: Maybe (Conversion Integer)
    , entPartitive      :: Maybe (Conversion (Integer, Integer))
    , entMultiplicative :: Maybe (Conversion Integer)
    }

emptyEntry :: Entry
emptyEntry = Entry
    { entIso639_1       = Nothing
    , entIso639_2       = []
    , entIso639_3       = Nothing
    , entNativeNames    = []
    , entEnglishName    = Nothing
    , entVariant        = Nothing
    , entCardinal       = Nothing
    , entOrdinal        = Nothing
    , entPartitive      = Nothing
    , entMultiplicative = Nothing
    }
