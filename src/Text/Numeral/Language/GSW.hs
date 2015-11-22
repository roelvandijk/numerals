{-|
[@ISO639-1@]        -

[@ISO639-2@]        gse

[@ISO639-3@]        gse

[@Native name@]     Schwyzerdütsch

[@English name@]    Swiss German
-}

module Text.Numeral.Language.GSW
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

import qualified "containers" Data.Map as M ( fromList, lookup )
import "this" Text.Numeral
import "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Language.DEU ( struct )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- GSW
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_2    = ["gse"]
    , entIso639_3    = Just "gse"
    , entNativeNames = ["Schwyzerdütsch"]
    , entEnglishName = Just "Swiss German"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

bounds :: (Integral a) => (a, a)
bounds = let x = dec 6 - 1 in (negate x, x)

genericRepr :: Repr
genericRepr = defaultRepr
              { reprAdd   = Just (⊞)
              , reprMul   = Just $ \_ _ _ -> ""
              }
    where
      (_       ⊞ (_ `Mul` Lit 10)) _ = "e"
      (Lit 100 ⊞ Lit 1)            _ = "und"
      (_       ⊞ _ )               _ = ""

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render genericRepr
               { reprValue = \_ n -> M.lookup n syms }
    where
      syms =
          M.fromList
          [ (1, \c -> case c of
                       CtxAdd _ (Lit 100) _ -> "äis"
                       CtxAdd {}            -> "ein"
                       _                    -> "eis"
            )
          , (2, \c -> case c of
                       CtxMul _ (Lit 10) _ -> "zwän"
                       _                   -> "zwöi"
            )
          , (3, \c -> case c of
                       CtxAdd _ (Lit 10) _ -> "dry"
                       CtxMul _ (Lit 10) _ -> "drys"
                       _                   -> "drü"
            )
          , (4, const "vier")
          , (5, const "füf")
          , (6, \c -> case c of
                       CtxMul _ (Lit 10) _ -> "sëch"
                       _                   -> "sächs"
            )
          , (7, \c -> case c of
                       CtxAdd _ (_ `Mul` Lit 10) _ -> "siben"
                       _                           -> "sibe"
            )
          , (8, \c -> case c of
                       CtxMul _ (Lit 10) _ -> "ach"
                       _                   -> "acht"
            )
          , (9, \c -> case c of
                       CtxAdd _ (_ `Mul` Lit 10) _ -> "nün"
                       _                           -> "nüün"
            )
          , (10, \c -> case c of
                        CtxMul _ (Lit 3) _ -> "sg"
                        CtxMul {}          -> "zg"
                        _                  -> "zäh"
            )
          , (11, const "euf")
          , (12, const "zwüof")
          , (100, const "hundert")
          ]
