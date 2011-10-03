{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        sco

[@ISO639-3@]        sco

[@Native name@]     Scots

[@English name@]    Scots
-}

module Text.Numeral.Language.SCO
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
import "base" Prelude       ( Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.Exp as E


-------------------------------------------------------------------------------
-- SCO
-------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/scots.html
-}

cardinal ∷ (Integral α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β) ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (  1, lit       )
                [ ( 13, add 10 L  )
                , ( 20, mul 10 R L)
                , (100, lit       )
                ]
                   100

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (1, 100)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just $ \_ _ _ → ""
               }
    where
      ((_ `Mul` _) ⊞ _) _ = " "
      (_           ⊞ _) _ = ""

      syms =
          M.fromList
          [ (1, const    "ane"                     )
          , (2, tenForms "twa"    "twa"    "twin"  )
          , (3, tenForms "three"  "ther"   "ther"  )
          , (4, const    "fower"                   )
          , (5, tenForms "five"   "feif"   "fuf"   )
          , (6, const    "sax"                     )
          , (7, tenForms "seeven" "seiven" "seeven")
          , (8, tenForms "echt"   "ech"    "ech"   )
          , (9, tenForms "nine"   "nin"    "nin"   )
          , (10, \c → case c of
                        CtxAdd {} → "teen"
                        CtxMul {} → "tie"
                        _         → "ten"
            )
          , (11, const "aleeven")
          , (12, const "twal")
          , (100, const "hunner")
          , (1000, const "thousant")
          ]

      tenForms o a m = \c → case c of
                              CtxAdd L _ _ → a
                              CtxMul {}    → m
                              _            → o
