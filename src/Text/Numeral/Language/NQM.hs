{-|
[@ISO639-1@]        -

[@ISO639-2B@]       -

[@ISO639-3@]        nqm

[@Native name@]     -

[@English name@]    Ndom
-}

module Text.Numeral.Language.NQM
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
-- NQM
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_3    = Just "nqm"
    , entEnglishName = Just "Ndom"
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
       $ findRule (  1, lit       )
                [ (  7, add  6 R  )
                , ( 12, mul  6 R R)
                , ( 18, lit       )
                , ( 19, add 18 R  )
                , ( 36, lit       )
                , ( 37, add 36 R  )
                , ( 72, mul 36 R R)
                ]
                   107

bounds :: (Integral a) => (a, a)
bounds = (1, 107)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprAdd   = Just $ \_ _ _ -> " abo "
               , reprMul   = Just (⊡)
               }
    where
      (Lit 36 ⊡ _) _ = " "
      (_      ⊡ _) _ = " an "

      syms =
          M.fromList
          [ ( 1, const "sas")
          , ( 2, const "thef")
          , ( 3, const "ithin")
          , ( 4, const "thonith")
          , ( 5, const "meregh")
          , ( 6, const "mer")
          , (18, const "tondor")
          , (36, const "nif")
          ]
