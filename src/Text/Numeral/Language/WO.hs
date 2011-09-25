{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        wo

[@ISO639-2@]        wo1

[@ISO639-3@]        wo1

[@Native name@]     Wolof

[@English name@]    Wolof
-}

module Text.Numeral.Language.WO
    ( -- * Conversions
      cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

{-
Sources:
  http://en.wikipedia.org/wiki/Wolof_language#Numerals
  http://www.sf.airnet.ne.jp/~ts/language/number/wolof.html
-}

import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Monoid.Unicode   ( (⊕) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≥) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- WO
--------------------------------------------------------------------------------

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β) ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (  0, lit        )
                [ (  6, add 5 R    )
                , ( 10, lit        )
                , ( 11, add 10 R   )
                , ( 20, mul 10 R L )
                , (100,  step 100 10 R L)
                , (1000, step 1000 1000 R L)
                , (dec 6, lit)
                ]
                  (dec 6)

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (0, dec 6)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               }
    where
      (Lit 5 ⊞ _) _ = "-"
      (_     ⊞ _) _ = " ak "

      (_ ⊡ Lit 10) _ = "-"
      (_ ⊡ _     ) _ = " "

      syms =
          M.fromList
          [ (0, const "tus")
          , (1, i "benn")
          , (2, i "ñaar")
          , (3, i "ñett")
          , (4, i "ñeent")
          , (5, i "juróom")
          , (10, i "fukk")
          , (100, i "téeméer")
          , (1000, const "junni")
          , (dec 6, const "tamndareet")
          ]

      i s = \c → s ⊕ case c of
                       CtxMul _ (Lit n) _ | n ≥ 100 → "i"
                       CtxAdd R _ (CtxMul _ (Lit n) _) | n ≥ 100 → "i"
                       _ → ""
