{-# LANGUAGE FlexibleContexts
           , NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        sv

[@ISO639-2B@]       swe

[@ISO639-3@]        swe

[@Native name@]     svenska

[@English name@]    Swedish
-}

module Text.Numeral.Language.SV
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

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-), div, negate, even )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Prelude.Unicode       ( ℤ )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.BigNum as BN
import qualified "numerals-base" Text.Numeral.Exp    as E
import qualified "numerals-base" Text.Numeral.Grammar as G
import           "numerals-base" Text.Numeral.Misc ( dec, intLog )
import "this" Text.Numeral.Entry


-------------------------------------------------------------------------------
-- SV
-------------------------------------------------------------------------------

entry ∷ (Monoid s, IsString s) ⇒ Entry s
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
    }

cardinal ∷ ( G.Common i, G.Neuter i
           , Integral α, E.Scale α
           , Monoid s, IsString s
           )
         ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

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

cardinalRepr ∷ (G.Common i, G.Neuter i, Monoid s, IsString s)
             ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \ctx n → M.lookup n (syms ctx)
               , reprScale = pelletierRepr
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
          , (3, ten   "tre"  "tret" "tret")
          , (4, ten   "fyra" "fjor" "fyr")
          , (5, const "fem")
          , (6, const "sex")
          , (7, ten   "sju"  "sjut" "sjut")
          , (8, ten   "åtta" "ar"   "åt")
          , (9, ten   "nio"  "nit"  "nit")
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

      ten n a m = \c → case c of
                         CtxAdd _ (Lit 10) _ → a
                         CtxMul _ (Lit 10) _ → m
                         _                   → n

pelletierRepr ∷ (IsString s, Monoid s)
              ⇒ i → ℤ → ℤ → (Exp i) → Ctx (Exp i) → Maybe s
pelletierRepr =
    BN.pelletierRepr
      (BN.quantityName "iljon"  "iljoner")
      (BN.quantityName "iljard" "iljarder")
      [ (4, BN.forms "kvadr" "kvattuor" "kvattuor" "kvadra"  "kvadri")
      , (5, BN.forms "kvint" "kvin"     "kvinkva"  "kvinkva" "kvin")
      , (8, BN.forms "okt"   "okto"     "okto"     "okto"    "oktin")
      ]
