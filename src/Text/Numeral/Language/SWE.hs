{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        sv

[@ISO639-2B@]       swe

[@ISO639-3@]        swe

[@Native name@]     svenska

[@English name@]    Swedish
-}

module Text.Numeral.Language.SWE
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

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Prelude       ( Num, Integral, (-), div, negate, even )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum as BN
import qualified "this" Text.Numeral.Exp    as E
import qualified "this" Text.Numeral.Grammar as G
import           "this" Text.Numeral.Misc ( dec, intLog )
import "this" Text.Numeral.Entry
import "this" Text.Numeral.Render.Utils ( addCtx, mulCtx, outsideCtx )
import "text" Data.Text ( Text )


-------------------------------------------------------------------------------
-- SWE
-------------------------------------------------------------------------------

entry ∷ Entry
entry = emptyEntry
    { entIso639_1    = Just "sv"
    , entIso639_2    = ["swe"]
    , entIso639_3    = Just "swe"
    , entNativeNames = ["svenska"]
    , entEnglishName = Just "Swedish"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    , entOrdinal     = Just Conversion
                       { toNumeral   = ordinal
                       , toStructure = struct
                       }
    }

cardinal ∷ (G.Common i, G.Neuter i, Integral α, E.Scale α)
         ⇒ i → α → Maybe Text
cardinal inf = cardinalRepr inf ∘ struct

ordinal ∷ (G.Common i, G.Neuter i, Integral α, E.Scale α)
        ⇒ i → α → Maybe Text
ordinal inf = ordinalRepr inf ∘ struct

struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
         , E.Inflection β, G.Common (E.Inf β)
         )
       ⇒ α → β
struct = pos $ fix $ rule `combine` pelletierScale1_sv
    where
      rule = findRule (   0, lit       )
                    [ (  13, add 10 L  )
                    , (  20, lit       )
                    , (  21, add 20 R  )
                    , (  30, mul 10 R L)
                    , ( 100, step1 100   10 R L)
                    , (1000, step1 1000 1000 R L)
                    ]
                      (dec 6 - 1)

-- | Like 'pelletierScale1' with the difference that all scale
-- elements are of the common gender.
pelletierScale1_sv ∷ ( Integral α, E.Scale α
                     , E.Unknown β, E.Lit β, E.Add β, E.Mul β, E.Scale β
                     , E.Inflection β, G.Common (E.Inf β)
                     )
                   ⇒ Rule α β
pelletierScale1_sv =
    conditional (\n → even $ intLog n `div` 3)
                (mulScale1_sv 6 0 R L BN.rule)
                (mulScale1_sv 6 3 R L BN.rule)
  where
    mulScale1_sv = mulScale_ $ \f m s _ → commonMul (f m) s
    commonMul m s = E.inflection (G.common) $ E.mul m s

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 60000 - 1 in (negate x, x)

cardinalRepr ∷ (G.Common i, G.Neuter i) ⇒ i → Exp i → Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \ctx n → M.lookup n (syms ctx)
               , reprScale = BN.pelletierRepr
                               (BN.quantityName "iljon"  "iljoner")
                               (BN.quantityName "iljard" "iljarder")
                               bigNumSyms
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _   → "minus "
               }
    where
      (Inflection _ _ ⊞ _) _ = " "
      (_ ⊞ _) _ = ""

      (_ ⊡ Lit  100) CtxEmpty = " "
      (_ ⊡ Lit 1000) CtxEmpty = " "
      (_ ⊡ Scale{})  _ = " "
      (_ ⊡ _) _ = ""

      syms ctx =
          M.fromList
          [ (0, const "noll")
          , (1, \c → case c of
                       CtxMul _ (Lit 1000) CtxEmpty → "ett"
                       CtxMul _ (Lit 1000) _ → "et"
                       _ | G.isCommon ctx → "en"
                         | G.isNeuter ctx → "ett"
                         | otherwise      → "?"
            )
          , (2, const "två")
          , (3, addCtx 10 "tret" $ mulCtx 10 "tret" $ const "tre")
          , (4, addCtx 10 "fjor" $ mulCtx 10 "fyr"  $ const "fyra")
          , (5, const "fem")
          , (6, const "sex")
          , (7, addCtx 10 "sjut" $ mulCtx 10 "sjut" $ const "sju")
          , (8, addCtx 10 "ar"   $ mulCtx 10 "åt"   $ const "åtta")
          , (9, addCtx 10 "nit"  $ mulCtx 10 "nit"  $ const "nio")
          , (10, \c → case c of
                        CtxAdd {} → "ton"
                        _         → "tio"
            )
          , (11, const "elva")
          , (12, const "tolv")
          , (20, const "tjugo")
          , (100, const "hundra")
          , (1000, const "tusen")
          ]

ordinalRepr ∷ (G.Common i, G.Neuter i) ⇒ i → Exp i → Maybe Text
ordinalRepr = render defaultRepr
              { reprValue = \ctx n → M.lookup n (syms ctx)
              , reprScale = BN.pelletierRepr
                               (BN.ordQuantityName "iljon" "iljonte" "iljoner" "iljonte")
                               (BN.quantityName "iljard" "iljarder")
                               bigNumSyms
              , reprAdd   = Just (⊞)
              , reprMul   = Just (⊡)
              , reprNeg   = Just $ \_ _   → "minus "
              }
    where
      (Inflection _ _ ⊞ _) _ = " "
      (_ ⊞ _) _ = ""

      (_ ⊡ Lit  100) CtxEmpty = " "
      (_ ⊡ Lit 1000) CtxEmpty = " "
      (_ ⊡ Scale{})  _ = " "
      (_ ⊡ _) _ = ""

      syms ctx =
          M.fromList
          [ (0, outsideCtx R "nollte" $ const "noll")
          , (1, outsideCtx R "första"
                $ \c → case c of
                         CtxMul _ (Lit 1000) CtxEmpty → "ett"
                         CtxMul _ (Lit 1000) _ → "et"
                         _ | G.isCommon ctx → "en"
                           | G.isNeuter ctx → "ett"
                           | otherwise      → "?"
            )
          , (2, outsideCtx R "andra"   $ const "två")
          , (3, outsideCtx R "tredje"  $ addCtx 10 "tret" $ mulCtx 10 "tret" $ const "tre")
          , (4, outsideCtx R "fjärde"  $ addCtx 10 "fjor" $ mulCtx 10 "fyr"  $ const "fyra")
          , (5, outsideCtx R "femte"   $ const "fem")
          , (6, outsideCtx R "sjätte"  $ const "sex")
          , (7, outsideCtx R "sjunde"  $ addCtx 10 "sjut" $ mulCtx 10 "sjut" $ const "sju")
          , (8, outsideCtx R "åttonde" $ addCtx 10 "ar"   $ mulCtx 10 "åt"   $ const "åtta")
          , (9, outsideCtx R "nionde"  $ addCtx 10 "nit"  $ mulCtx 10 "nit"  $ const "nio")
          , (10, \c → case c of
                        CtxAdd {}
                          | isOutside R c → "tonde"
                          | otherwise     → "ton"
                        _ | isOutside R c → "tionde"
                          | otherwise     → "tio"
            )
          , (11, outsideCtx R "elfte"    $ const "elva")
          , (12, outsideCtx R "tolfte"   $ const "tolv")
          , (20, outsideCtx R "tjugonde" $ const "tjugo")
          , (100, outsideCtx R "hundrade" $ const "hundra")
          , (1000, outsideCtx R "tusende" $ const "tusen")
          ]

bigNumSyms ∷ (Num α) ⇒ [(α, Ctx (Exp i) → Text)]
bigNumSyms =
    [ (4, BN.forms "kvadr" "kvattuor" "kvattuor" "kvadra"  "kvadri")
    , (5, BN.forms "kvint" "kvin"     "kvinkva"  "kvinkva" "kvin")
    , (8, BN.forms "okt"   "okto"     "okto"     "okto"    "oktin")
    ]
