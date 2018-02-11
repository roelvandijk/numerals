{-|
[@ISO639-1@]        ig

[@ISO639-2@]        ibo

[@ISO639-3@]        ibo

[@Native name@]     Asụsụ Igbo

[@English name@]    Igbo
-}
module Text.Numeral.Language.IBO
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
    , ordinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import "base" Data.Monoid ( (<>) )
import qualified "containers" Data.Map as M
import "this" Text.Numeral
import "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


-------------------------------------------------------------------------------
-- IBO
-------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "ig"
    , entIso639_2    = ["ibo"]
    , entIso639_3    = Just "ibo"
    , entNativeNames = ["Asụsụ Igbo"]
    , entEnglishName = Just "Igbo"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    , entOrdinal     = Just Conversion
                       { toNumeral   = ordinal
                       , toStructure = struct
                       }
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

ordinal :: (Integral a) => Inflection -> a -> Maybe Text
ordinal inf = ordinalRepr inf . struct

struct :: (Integral a) => a -> Exp
struct = pos
       $ fix
       $ findRule (    0, lit        )
                [ (   11, add  10 R  )
                , (   20, mul  10 R R)
                , (  100, step  100      10 R R)
                , ( 1000, step 1000    1000 R R)
                , (dec 6, step (dec 6) 1000 R R)
                , (dec 9, step (dec 9) 1000 R R)
                ]
                  (dec 12 - 1)

bounds :: (Integral a) => (a, a)
bounds = (0, dec 12 - 1)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprAdd = Just (⊞)
               , reprMul = Just (⊡)
               }
    where
      (_ ⊞ _) _ = " na "
      (_ ⊡ _) _ = " "

      syms =
          M.fromList
          [ (0, const "adigi")
          , (1, const "otu")
          , (2, const "abụọ")
          , (3, const "atọ")
          , (4, const "anọ")
          , (5, const "ise")
          , (6, const "isii")
          , (7, const "asaa")
          , (8, const "asato")
          , (9, const "itoolu")
          , (10, const "iri")
          , (100, const "nnari")
          , (1000, const "puku")
          , (dec 6, const "nde")
          , (dec 9, const "ijeri")
          ]

ordinalRepr :: Inflection -> Exp -> Maybe Text
ordinalRepr inf e =
    ("nke " <>) <$>
    render defaultRepr
           { reprValue = \_ n -> M.lookup n syms
           , reprAdd = Just (⊞)
           , reprMul = Just (⊡)
           }
           inf
           e
    where
      (_ ⊞ _) _ = " na "
      (_ ⊡ _) _ = " "

      syms =
          M.fromList
          [ (0, const "adigi")
          , (1, \c -> case c of
                          CtxEmpty -> "mbụ"
                          _        -> "otu"
            )
          , (2, const "abụọ")
          , (3, const "atọ")
          , (4, const "anọ")
          , (5, const "ise")
          , (6, const "isii")
          , (7, const "asaa")
          , (8, const "asato")
          , (9, const "itoolu")
          , (10, const "iri")
          , (100, const "nnari")
          , (1000, const "puku")
          , (dec 6, const "nde")
          , (dec 9, const "ijeri")
          ]
