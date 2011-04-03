{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        oj

[@ISO639-2@]        oji

[@ISO639-3@]        oji

[@Native name@]     ᐊᓂᔑᓈᐯᒧᐎᓐ (Anishinaabemowin)

[@English name@]    Ojibwe
-}

module Text.Numeral.Language.OJ
    ( cardinal
    , struct
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- OJ
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/ojibwa.html
  http://www.languagesandnumbers.com/how-to-count-in-ojibwa/en/oji/
-}

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Add β, C.Mul β)
       ⇒ α → Maybe β
struct = checkPos $ fix $ rule

rule ∷ (Integral α, C.Lit β, C.Add β, C.Mul β) ⇒ Rule α β
rule = findRule (   1, lit               )
              [ (  11, step   10 10 R L)
              , ( 100, step  100 10 R L)
              , (1000, step 1000  2 R L)
              ]
                 1999

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = Just $ \_ _ _ → " shaa "
               , reprMul   = Just $ \_ _ _ → ""
               }
    where
      syms =
          M.fromList
          [ (0, const "kaagego"                              )
          , (1, const "bezhik"                               )
          , (2, const "niizh"                                )
          , (3, forms "nswi"        "nsim"        "ns"       )
          , (4, forms "niiwin"      "niim"        "nii"      )
          , (5, forms "naanan"      "naanmi"      "naan"     )
          , (6, forms "ngodwaaswi"  "ngodwaasmi"  "ngodwaas" )
          , (7, forms "niizhwaaswi" "niizhwaasmi" "niizhwaas")
          , (8, forms "nshwaaswi"   "nshwaasmi"   "nshwaas"  )
          , (9, forms "zhaangswi"   "zhaangsmi"   "zhaangs"  )
          , (10, \c → case c of
                        CtxMul {} → "taana"
                        _         → "mdaaswi"
            )
          , (100, \c → case c of
                         CtxMul {} → "waak"
                         _         → "ngodwaak"
            )
          , (1000, const "mdaaswaak")
          ]

      forms ∷ s → s → s → Ctx Exp → s
      forms o t h = \c → case c of
                           CtxMul _ (Lit 10)  _ → t
                           CtxMul _ (Lit 100) _ → h
                           _                    → o
