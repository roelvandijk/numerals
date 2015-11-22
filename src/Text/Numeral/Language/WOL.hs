{-|
[@ISO639-1@]        wo

[@ISO639-2@]        wo1

[@ISO639-3@]        wo1

[@Native name@]     Wolof

[@English name@]    Wolof
-}

module Text.Numeral.Language.WOL
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
import "base" Data.Monoid ( (<>) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import "this" Text.Numeral
import "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- WOL
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "wo"
    , entIso639_2    = ["wol"]
    , entIso639_3    = Just "wol"
    , entNativeNames = ["Wolof"]
    , entEnglishName = Just "Wolof"
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
       $ findRule (  0, lit        )
                [ (  6, add 5 R    )
                , ( 10, lit        )
                , ( 11, add 10 R   )
                , ( 20, mul 10 R L )
                , (100,  step 100 10 R L)
                , (1000, step 1000 1000 R L)
                , (dec 6, lit)
                ]
                  (dec 6)

bounds :: (Integral a) => (a, a)
bounds = (0, dec 6)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               }
    where
      (Lit 5 ⊞ _) _ = "-"
      (_     ⊞ _) _ = " ak "

      (_ ⊡ Lit 10) _ = "-"
      (_ ⊡ _     ) _ = " "

      syms =
          M.fromList
          [ (0, const "tus")
          , (1, i "benn")
          , (2, i "ñaar")
          , (3, i "ñett")
          , (4, i "ñeent")
          , (5, i "juróom")
          , (10, i "fukk")
          , (100, i "téeméer")
          , (1000, const "junni")
          , (dec 6, const "tamndareet")
          ]

      i s = \c -> s <> case c of
                         CtxMul _ (Lit n) _ | n >= 100 -> "i"
                         CtxAdd R _ (CtxMul _ (Lit n) _) | n >= 100 -> "i"
                         _ -> ""
