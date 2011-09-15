{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        amp

[@Native name@]     -

[@English name@]    Alamblak
-}

module Text.Numeral.Language.AMP
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
import "base" Prelude       ( Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- AMP
-------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/alamblak.html
-}

cardinal ∷ (Integral α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β) ⇒ α → β
struct = checkPos
       $ fix
       $ findRule ( 1, lit       )
                [ ( 3, add  2 R  )
                , ( 5, lit       )
                , ( 6, add  5 R  )
                , (10, mul  5 R R)
                , (20, lit       )
                , (21, add 20 R  )
                , (40, mul 20 R R)
                ]
                  399

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just $ \_ _ _ → " "
               }
    where
      (Lit 2 ⊞ Lit _) _ = "i"
      (_     ⊞ _    ) _ = "i "

      syms =
          M.fromList
          [ (1,  const "rpat")
          , (2,  const "hosf")
          , (5,  \c → case c of
                        CtxMul L _ _ → "tir"
                        _            → "tir yohtt"
            )
          , (20, \c → case c of
                        CtxMul L _ _ → "yima"
                        _            → "yima yohtt"
            )
          ]
