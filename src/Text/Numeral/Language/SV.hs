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
    ( cardinal
    , struct
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-), Integer )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.BigNum      as BN
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- SV
-------------------------------------------------------------------------------

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
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

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
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
              ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
pelletierRepr =
    BN.pelletierRepr
      "iljon"  "iljon"
      "iljard" "iljard"
      [ (4, BN.forms "kvadr" "kvattuor" "kvattuor" "kvadra"  "kvadri")
      , (5, BN.forms "kvint" "kvin"     "kvinkva"  "kvinkva" "kvin")
      , (8, BN.forms "okt"   "okto"     "okto"     "okto"    "oktin")
      ]
