{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

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

import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.BigNum as BN
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- NO
-------------------------------------------------------------------------------

-- Sources:
--   http://en.wikibooks.org/wiki/Norwegian_Numbers
--   http://www.sf.airnet.ne.jp/~ts/language/number/norwegian.html

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = pos
       $ fix
       $ findRule (   0, lit               )
                [ (  13, add    10      L  )
                , (  20, lit               )
                , (  21, add    20      R  )
                , (  30, mul    10      R L)
                , ( 100, step  100   10 R L)
                , (1000, step 1000 1000 R L)
                ]
                  (dec 6 - 1)
         `combine` pelletierScale R L BN.rule

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = BN.scaleRepr "illion" "illioner" []
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _   → "minus "
               }
    where
      (Lit 100  ⊞ _) _ = " "
      (Lit 1000 ⊞ _) _ = " "
      (_        ⊞ _) _ = ""

      (_ ⊡ Lit 10  ) _ = ""
      (_ ⊡ Lit 1000) _ = ""
      (_ ⊡ _       ) _ = " "

      syms =
          M.fromList
          [ (0,  const "null")
          , (1,  const "en")
          , (2,  const "to")
          , (3,  tenForms   "tre"  "tret" "tret")
          , (4,  tenForms   "fire" "fjor" "før")
          , (5,  const "fem")
          , (6,  const "seks")
          , (7,  tenForms   "syv"  "syt"  "syt")
          , (8,  tenForms   "åtte" "at"   "åt")
          , (9,  tenForms   "ni"   "nit"  "nit")
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

      tenForms n a m = \c → case c of
                              CtxAdd _ (Lit 10  ) _ → a
                              CtxMul _ (Lit 100 ) _ → n
                              CtxMul _ (Lit 1000) _ → n
                              CtxMul {}             → m
                              _                     → n
