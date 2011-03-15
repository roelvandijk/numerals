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
    , cardinalRepr
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
import Prelude       ( Integral )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import qualified Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- SV
-------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Neg β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = pos
       $ fix
       $ findRule (  0, lit         )
                [ ( 13, add   10 L  )
                , ( 20, lit         )
                , ( 21, add   20 R  )
                , ( 30, mul   10 R L)
                , (100, lit1        )
                , (101, add  100 R  )
                , (200, mul1 100 R L)
                ]
                  1000

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = \_ _ → Just ""
               , reprMul   = \_ _ → Just ""
               , reprNeg   = \_   → Just "minus "
               }
    where
      symMap = M.fromList
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
                             CtxAddR {} → "ton"
                             _          → "tio"
                 )
               , (11, const "elva")
               , (12, const "tolv")
               , (20, const "tjugo")
               , (100, const "hundra")
               , (1000, const "tusen")
               ]

      ten n a m = \c → case c of
                         CtxAddL (Lit 10) _ → a
                         CtxMulL (Lit 10) _ → m
                         _                  → n
