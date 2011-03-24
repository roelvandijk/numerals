{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        nb

[@ISO639-2B@]       nob

[@ISO639-3@]        nob

[@Native name@]     Bokmål

[@English name@]    Norwegian Bokmål
-}

module Text.Numeral.Language.NO
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
import Prelude       ( Integral )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import qualified Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- NO
-------------------------------------------------------------------------------

-- Sources:
--   http://en.wikibooks.org/wiki/Norwegian_Numbers
--   http://www.sf.airnet.ne.jp/~ts/language/number/norwegian.html

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Neg β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = pos
       $ fix
       $ findRule (  0, lit            )
                [ ( 13, add   10    L  )
                , ( 20, lit            )
                , ( 21, add   20    R  )
                , ( 30, mul   10    R L)
                , (100, step 100 10 R L)
                ]
                  1000

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = \_ _ → Just ""
               , reprMul   = \_ _ → Just ""
               , reprNeg   = \_ → Just "minus "
               }
    where
      syms =
          M.fromList
          [ (0,  const "null")
          , (1,  const "en")
          , (2,  const "to")
          , (3,  ten   "tre"  "tret" "tret")
          , (4,  ten   "fire" "fjor" "før")
          , (5,  const "fem")
          , (6,  const "seks")
          , (7,  ten   "syv"  "syt"  "syt")
          , (8,  ten   "åtte" "at"   "åt")
          , (9,  ten   "ni"   "nit"  "nit")
          , (10, \c → case c of
                        CtxAdd {} → "ten"
                        _         → "ti"
            )
          , (11, const "elleve")
          , (12, const "tolv")
          , (20, const "tjue")
          , (100, const "hundre")
          , (1000, const "tusen")
          ]

      ten n a m = \c → case c of
                         CtxAdd _ (Lit 10) _ → a
                         CtxMul {}           → m
                         _                   → n
