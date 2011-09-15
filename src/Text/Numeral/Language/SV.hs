{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

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

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, (-), Integer )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.BigNum      as BN
import qualified Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- SV
-------------------------------------------------------------------------------

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ ( Integral α, C.Scale α
         , C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → Maybe β
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
