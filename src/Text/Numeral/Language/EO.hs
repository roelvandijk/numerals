{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        eo

[@ISO639-2B@]       epo

[@ISO639-3@]        epo

[@Native name@]     Esperanto

[@English name@]    Esperanto
-}

module Text.Numeral.Language.EO
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

import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.Exp as E


--------------------------------------------------------------------------------
-- EO
--------------------------------------------------------------------------------

cardinal ∷ (Integral α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β) ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (  0, lit            )
                [ ( 11, step 10  10 R L)
                , (100, step 100 10 R L)
                ]
                  1000

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (0, 1000)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprAdd   = Just $ \_ _ _ → " "
               , reprMul   = Just $ \_ _ _ → ""
               }
    where
      syms =
          M.fromList
          [ (0, const "nul")
          , (1, const "unu")
          , (2, const "du")
          , (3, const "tri")
          , (4, const "kvar")
          , (5, const "kvin")
          , (6, const "ses")
          , (7, const "sep")
          , (8, const "ok")
          , (9, const "naŭ")
          , (10, const "dek")
          , (100, const "cent")
          , (1000, const "mil")
          ]
