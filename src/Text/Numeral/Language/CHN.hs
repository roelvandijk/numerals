{-|
[@ISO639-1@]        -

[@ISO639-2B@]       chn

[@ISO639-3@]        chn

[@Native name@]     chinuk wawa

[@English name@]    Chinook Jargon
-}

module Text.Numeral.Language.CHN
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import qualified "containers" Data.Map as M ( fromList, lookup )
import "this" Text.Numeral
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- CHN
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_2    = ["chn"]
    , entIso639_3    = Just "chn"
    , entNativeNames = ["chinuk wawa"]
    , entEnglishName = Just "Chinook Jargon"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

struct :: (Integral a) => a -> Exp
struct = checkPos
       $ fix
       $ findRule (  0, lit           )
                [ ( 11, step 10 10 R L)
                , (100, lit           )
                ]
                   100

bounds :: (Integral a) => (a, a)
bounds = (0, 100)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprAdd   = Just $ \_ _ _ -> " pe "
               , reprMul   = Just $ \_ _ _ -> " "
               }
    where
      syms =
          M.fromList
          [ (1, const "ikt")
          , (2, const "mokst")
          , (3, const "klone")
          , (4, const "lakit")
          , (5, const "kwinnum")
          , (6, const "taghum")
          , (7, const "sinamokst")
          , (8, const "stotekin")
          , (9, const "kwaist")
          , (10, const "tahtlum")
          , (100, const "tukamonuk")
          ]
