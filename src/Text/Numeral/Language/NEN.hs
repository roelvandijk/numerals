{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        nen

[@Native name@]     -

[@English name@]    Nengone
-}
module Text.Numeral.Language.NEN
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
-- NEN
-------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_3    = Just "nen"
    , entEnglishName = Just "Nengone"
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
       $ findRule ( 0, lit     )
                [ ( 6, add  5 R)
                , (10, lit     )
                , (11, add 10 R)
                , (15, lit     )
                , (16, add 15 R)
                , (20, lit     )
                , (21, add 20 R)
                ]
                   30

bounds :: (Integral a) => (a, a)
bounds = (0, 30)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprAdd   = Just (⊞)
               }
    where
      (Lit 5 ⊞ _) (CtxAdd _ (Lit 20) _) = ""
      (_ ⊞ _) _ = " ne "

      syms =
          M.fromList
          [ (1, const "sa")
          , (2, forms "rewe" "rew")
          , (3, forms "tini" "tin")
          , (4, forms "ece"  "ec")
          , (5, \c -> case c of
                       CtxAdd _ _ (CtxAdd _ (Lit 20) _) -> "sedo"
                       _ -> "sedong"
            )
          , (10, const "ruenin")
          , (15, const "adenin")
          , (20, const "sarengom")
          ]
        where
          forms :: s -> s -> Ctx Exp -> s
          forms _ a (CtxAdd _ _ _) = a
          forms n _ _              = n
