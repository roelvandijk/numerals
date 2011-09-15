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
    ( cardinal
    , struct
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
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- NQM
--------------------------------------------------------------------------------

cardinal ∷ (Integral α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β) ⇒ α → β
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

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
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
