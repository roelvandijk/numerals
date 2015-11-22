{-|
[@ISO639-1@]        -

[@ISO639-2@]        fur

[@ISO639-3@]        fur

[@Native name@]     Furlan

[@English name@]    Friulan
-}

module Text.Numeral.Language.FUR
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
import qualified "containers" Data.Map as M
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum  as BN
import           "this" Text.Numeral.Misc ( dec )
import           "this" Text.Numeral.Entry
import qualified "this" Text.Numeral.Language.ITA as ITA ( rule )
import           "this" Text.Numeral.Render.Utils ( addCtx, mulCtx )
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- FUR
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_2    = ["fur"]
    , entIso639_3    = Just "fur"
    , entNativeNames = ["Furlan", "marilenghe"]
    , entEnglishName = Just "Friulan"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

struct :: (Integral a) => a -> Exp
struct = fix $ ITA.rule `combine` pelletierScale1 R L BN.rule

bounds :: (Integral a) => (a, a)
bounds = let x = dec 12 - 1 in (negate x, x)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \inf n -> M.lookup n (syms inf)
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprScale = BN.pelletierRepr
                               (BN.quantityName "ilion"  "ilions")
                               (BN.quantityName "iliart" "iliarts")
                               []
               }
    where
      (         Lit   20  ⊞ _                        ) _ = "e"
      (         Lit  100  ⊞ _                        ) _ = " e "
      (         Lit 1000  ⊞ (        Lit 100 `Add` _)) _ = " "
      (         Lit 1000  ⊞ (_ `Mul` Lit 100 `Add` _)) _ = " "
      (         Lit 1000  ⊞ _                        ) _ = " e "
      ((_ `Mul` Lit  100) ⊞ _                        ) _ = " e "
      ((_ `Mul` Lit 1000) ⊞ (        Lit 100 `Add` _)) _ = " "
      ((_ `Mul` Lit 1000) ⊞ (_ `Mul` Lit 100 `Add` _)) _ = " "
      ((_ `Mul` Lit 1000) ⊞ _                        ) _ = " e "
      (_                  ⊞ _                        ) _ = ""

      (_ ⊡ Lit 1000) _ = " "
      (_ ⊡ Scale {}) _ = " "
      (_ ⊡ _       ) _ = ""

      syms inf =
          M.fromList
          [ (0, const "zero")
          , (1, \c -> case c of
                       _ | isFeminine  inf -> "une"
                         | otherwise       -> "un"
            )
          , (2, addCtx 10 "do" $ mulCtx 100 "dus"
                $ \c -> case c of
                         _ | isFeminine inf -> "dôs"
                           | otherwise      -> "doi"
            )
          , (3, addCtx 10  "tre"  $ mulCtx 10 "tr"
              $ mulCtx 100 "tres" $ const     "trê"
            )
          , (4, addCtx 10 "cutuar" $ mulCtx 10 "cuar"  $ const "cuatri")
          , (5, addCtx 10 "cuin"   $ mulCtx 10 "cincu" $ const "cinc")
          , (6, addCtx 10 "se"     $ mulCtx 10 "sess"  $ const "sîs")
          , (7,                      mulCtx 10 "set"   $ const "siet")
          , (8,                      mulCtx 10 "ot"    $ const "vot")
          , (9,                      mulCtx 10 "nov"   $ const "nûf")
          , (10, \c -> case c of
                        CtxAdd R (Lit n) _
                            | n <= 7       -> "dis"
                        CtxAdd L _       _ -> "dise"
                        CtxMul _ (Lit 3) _ -> "ente"
                        CtxMul _ (Lit n) _
                            | n <= 9       -> "ante"
                        _                  -> "dîs"
            )
          , (20, const "vincj")
          , (100, \c -> case c of
                         CtxMul _ (Lit n) _
                             | n <= 3    -> "inte"
                             | otherwise -> "cent"
                         _               -> "cent"
            )
          , (1000, const "mil")
          ]
