{-|
[@ISO639-1@]        -

[@ISO639-2@]        sco

[@ISO639-3@]        sco

[@Native name@]     Scots

[@English name@]    Scots
-}

module Text.Numeral.Language.SCO
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import qualified "containers" Data.Map as M ( fromList, lookup )
import "this" Text.Numeral
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


-------------------------------------------------------------------------------
-- SCO
-------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_2    = ["sco"]
    , entIso639_3    = Just "sco"
    , entNativeNames = ["Scots"]
    , entEnglishName = Just "Scots"
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
                [ ( 13, add 10 L  )
                , ( 20, mul 10 R L)
                , (100, lit       )
                ]
                   100

bounds :: (Integral a) => (a, a)
bounds = (1, 100)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just $ \_ _ _ -> ""
               }
    where
      ((_ `Mul` _) ⊞ _) _ = " "
      (_           ⊞ _) _ = ""

      syms =
          M.fromList
          [ (1, const    "ane"                     )
          , (2, tenForms "twa"    "twa"    "twin"  )
          , (3, tenForms "three"  "ther"   "ther"  )
          , (4, const    "fower"                   )
          , (5, tenForms "five"   "feif"   "fuf"   )
          , (6, const    "sax"                     )
          , (7, tenForms "seeven" "seiven" "seeven")
          , (8, tenForms "echt"   "ech"    "ech"   )
          , (9, tenForms "nine"   "nin"    "nin"   )
          , (10, \c -> case c of
                        CtxAdd {} -> "teen"
                        CtxMul {} -> "tie"
                        _         -> "ten"
            )
          , (11, const "aleeven")
          , (12, const "twal")
          , (100, const "hunner")
          , (1000, const "thousant")
          ]

      tenForms o a m = \c -> case c of
                              CtxAdd L _ _ -> a
                              CtxMul {}    -> m
                              _            -> o
