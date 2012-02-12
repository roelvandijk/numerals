{-# LANGUAGE NoImplicitPrelude
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
    ( -- * Conversions
      cardinal
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
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Prelude.Unicode       ( ℤ )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.BigNum as BN
import qualified "numerals-base" Text.Numeral.Exp    as E
import           "numerals-base" Text.Numeral.Grammar ( Inflection )
import           "numerals-base" Text.Numeral.Misc ( dec )


-------------------------------------------------------------------------------
-- SV
-------------------------------------------------------------------------------

cardinal ∷ (Inflection i, Integral α, E.Scale α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
         )
       ⇒ α → β
struct = pos $ fix $ rule `combine` pelletierScale R L BN.rule
    where
      rule = findRule (   0, lit       )
                    [ (  13, add 10 L  )
                    , (  20, lit       )
                    , (  21, add 20 R  )
                    , (  30, mul 10 R L)
                    , ( 100, step  100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                      (dec 6 - 1)

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 60000 - 1 in (negate x, x)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprScale = pelletierRepr
               , reprAdd   = Just $ \_ _ _ → ""
               , reprMul   = Just $ \_ _ _ → ""
               , reprNeg   = Just $ \_ _   → "minus "
               }
    where
      syms =
          M.fromList
          [ (0,  const "noll")
          , (1,  const "ett")
          , (2,  const "två")
          , (3,  ten   "tre"  "tret" "tret")
          , (4,  ten   "fyra" "fjor" "fyr")
          , (5,  const "fem")
          , (6,  const "sex")
          , (7,  ten   "sju"  "sjut" "sjut")
          , (8,  ten   "åtta" "ar"   "åt")
          , (9,  ten   "nio"  "nit"  "nit")
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
      (\_ _ → "iljon")
      (\_ _ → "iljard")
      [ (4, BN.forms "kvadr" "kvattuor" "kvattuor" "kvadra"  "kvadri")
      , (5, BN.forms "kvint" "kvin"     "kvinkva"  "kvinkva" "kvin")
      , (8, BN.forms "okt"   "okto"     "okto"     "okto"    "oktin")
      ]
