{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2B@]       -

[@ISO639-3@]        nqm

[@Native name@]     -

[@English name@]    Ndom
-}

module Text.Numeral.Language.NQM
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
-- NQM
--------------------------------------------------------------------------------

cardinal ∷ (Integral α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β) ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (  1, lit       )
                [ (  7, add  6 R  )
                , ( 12, mul  6 R R)
                , ( 18, lit       )
                , ( 19, add 18 R  )
                , ( 36, lit       )
                , ( 37, add 36 R  )
                , ( 72, mul 36 R R)
                ]
                   107

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (1, 107)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprAdd   = Just $ \_ _ _ → " abo "
               , reprMul   = Just (⊡)
               }
    where
      (Lit 36 ⊡ _) _ = " "
      (_      ⊡ _) _ = " an "

      syms =
          M.fromList
          [ ( 1, const "sas")
          , ( 2, const "thef")
          , ( 3, const "ithin")
          , ( 4, const "thonith")
          , ( 5, const "meregh")
          , ( 6, const "mer")
          , (18, const "tondor")
          , (36, const "nif")
          ]
