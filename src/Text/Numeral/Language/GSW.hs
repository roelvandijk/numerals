{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        gse

[@ISO639-3@]        gse

[@Native name@]     Schwyzerdütsch

[@English name@]    Swiss German
-}

module Text.Numeral.Language.GSW
    ( cardinal
    , struct
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( ($), const )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.Exp.Classes as C
import qualified "this"          Text.Numeral.Language.DE as DE


--------------------------------------------------------------------------------
-- GSW
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-swiss-german/en/gsw-che/
-}

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = DE.struct

genericRepr ∷ (Monoid s, IsString s) ⇒ Repr s
genericRepr = defaultRepr
              { reprAdd   = Just (⊞)
              , reprMul   = Just $ \_ _ _ → ""
              }
    where
      (_       ⊞ (_ `Mul` Lit 10)) _ = "e"
      (Lit 100 ⊞ Lit 1)            _ = "und"
      (_       ⊞ _ )               _ = ""

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render genericRepr
               { reprValue = \n → M.lookup n syms }
    where
      syms =
          M.fromList
          [ (1, \c → case c of
                       CtxAdd _ (Lit 100) _ → "äis"
                       CtxAdd {}            → "ein"
                       _                    → "eis"
            )
          , (2, \c → case c of
                       CtxMul _ (Lit 10) _ → "zwän"
                       _                   → "zwöi"
            )
          , (3, \c → case c of
                       CtxAdd _ (Lit 10) _ → "dry"
                       CtxMul _ (Lit 10) _ → "drys"
                       _                   → "drü"
            )
          , (4, const "vier")
          , (5, const "füf")
          , (6, \c → case c of
                       CtxMul _ (Lit 10) _ → "sëch"
                       _                   → "sächs"
            )
          , (7, \c → case c of
                       CtxAdd _ (_ `Mul` Lit 10) _ → "siben"
                       _                           → "sibe"
            )
          , (8, \c → case c of
                       CtxMul _ (Lit 10) _ → "ach"
                       _                   → "acht"
            )
          , (9, \c → case c of
                       CtxAdd _ (_ `Mul` Lit 10) _ → "nün"
                       _                           → "nüün"
            )
          , (10, \c → case c of
                        CtxMul _ (Lit 3) _ → "sg"
                        CtxMul {}          → "zg"
                        _                  → "zäh"
            )
          , (11, const "euf")
          , (12, const "zwüof")
          , (100, const "hundert")
          ]
