{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        da

[@ISO639-2@]        dan

[@ISO639-3@]        dan

[@Native name@]     dansk

[@English name@]    Danish
-}

module Text.Numeral.Language.DA
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

import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Function.Unicode ( (.) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum as BN
import qualified "this" Text.Numeral.Exp    as E
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry


-------------------------------------------------------------------------------
-- DA
-------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "da"
    , entIso639_2    = ["dan"]
    , entIso639_3    = Just "dan"
    , entNativeNames = ["Dansk"]
    , entEnglishName = Just "Danish"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal :: (Integral a, E.Scale a, Monoid s, IsString s)
         => Inflection -> a -> Maybe s
cardinal inf = cardinalRepr inf . struct

struct :: ( Integral a, E.Scale a
         , E.Unknown b, E.Lit b, E.Neg b, E.Add b, E.Mul b, E.Scale b
         )
       => a -> b
struct = pos
       $ fix
       $ findRule (   0, lit               )
                [ (  13, add    10      L  )
                , (  20, lit               )
                , (  21, add    20      L  )
                , (  30, mul    10      L L)
                -- , (  50, 2.5 ⋅ 20)
                , (  60, mul    20      L L)
                -- , (  70, 3.5 ⋅ 20)
                , (  80, mul    20      L L)
                -- , (  90, 4.5 ⋅ 20)
                , ( 100, lit               )
                -- , ( 100, step  100   10 R L)
                ]
                  (dec 6 - 1)
         `combine` pelletierScale R L BN.rule

bounds :: (Integral a) => (a, a)
bounds = let x = 100 in (negate x, x)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprScale = BN.scaleRepr (BN.quantityName "illion" "illioner") []
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _   -> "minus "
               }
    where
      (_ ⊞ Lit 20) _ = "og"
      (_ ⊞ (_ `Mul` Lit 10)) _ = "og"
      (_ ⊞ (_ `Mul` Lit 20)) _ = "og"
      (_ ⊞ _) _ = ""

      (_ ⊡ Lit 20) _ = "s" -- or "-sinds-"
      (_ ⊡ _) _ = ""

      syms =
          M.fromList
          [ (0,  const "nul")
          , (1,  \c -> case c of
                        CtxAdd {} -> "én"
                        _ -> "en"
            )
          , (2,  \c -> case c of
                        _ -> "to"
            )
          , (3,  \c -> case c of
                        CtxAdd _ (Lit 10) _ -> "tret"
                        _ -> "tre"
            )
          , (4,  \c -> case c of
                        CtxAdd _ (Lit 10) _ -> "fjor"
                        CtxMul _ (Lit 10) _ -> "fyrre"
                        CtxMul _ (Lit 20) _ -> "fir"
                        _ -> "fire"
            )
          , (5,  \c -> case c of
                        CtxAdd _ (Lit 10) _ -> "fem"
                        _ -> "fem"
            )
          , (6,  \c -> case c of
                        _ -> "seks"
            )
          , (7,  \c -> case c of
                        CtxAdd _ (Lit 10) _ -> "syt"
                        _ -> "syv"
            )
          , (8,  \c -> case c of
                        CtxAdd _ (Lit 10) _ -> "at"
                        _ -> "otte"
            )
          , (9,  \c -> case c of
                        CtxAdd _ (Lit 10) _ -> "nit"
                        _ -> "ni"
            )
          , (10, \c -> case c of
                        CtxAdd R _ _ -> "ten"
                        CtxMul _ (Lit 4) _ -> "" -- or "tyve"
                        CtxMul {} -> "dive"
                        _ -> "ti"
            )
          , (11, const "elleve")
          , (12, const "tolv")
          , (20, \c -> case c of
                        CtxMul {} -> "" -- or "tyve" but only when prefix with "-sinds-"
                        _ -> "tyve"
            )
          , (100, const "hundrede")
          ]
