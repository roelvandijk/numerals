{-|
[@ISO639-1@]        eo

[@ISO639-2B@]       epo

[@ISO639-3@]        epo

[@Native name@]     Esperanto

[@English name@]    Esperanto
-}

module Text.Numeral.Language.EPO
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
-- EPO
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "eo"
    , entIso639_2    = ["epo"]
    , entIso639_3    = Just "epo"
    , entNativeNames = ["Esperanto"]
    , entEnglishName = Just "Esperanto"
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
       $ findRule (  0, lit            )
                [ ( 11, step 10  10 R L)
                , (100, step 100 10 R L)
                ]
                  1000

bounds :: (Integral a) => (a, a)
bounds = (0, 1000)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprAdd   = Just $ \_ _ _ -> " "
               , reprMul   = Just $ \_ _ _ -> ""
               }
    where
      syms =
          M.fromList
          [ (0, const "nul")
          , (1, const "unu")
          , (2, const "du")
          , (3, const "tri")
          , (4, const "kvar")
          , (5, const "kvin")
          , (6, const "ses")
          , (7, const "sep")
          , (8, const "ok")
          , (9, const "naŭ")
          , (10, const "dek")
          , (100, const "cent")
          , (1000, const "mil")
          ]
