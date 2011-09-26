{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        cs

[@ISO639-2B@]       cze

[@ISO639-2T@]       ces

[@ISO639-3@]        ces

[@Native name@]     Čeština

[@English name@]    Czech
-}

module Text.Numeral.Language.CS
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

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( const )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.BigNum as BN
import qualified "numerals-base" Text.Numeral.Exp.Classes as C
import qualified "this" Text.Numeral.Language.PL as PL


-------------------------------------------------------------------------------
-- CS
-------------------------------------------------------------------------------

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = PL.struct

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 60000 - 1 in (negate x, x)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = BN.pelletierRepr (quantityRepr "ilión"   "ilióny"  "iliónů")
                                              (quantityRepr "iliarda" "iliarda" "iliarda")
                                              []
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               }
    where
      (Lit n ⊞ Lit 10) _ | n ≤ 9 = ""
      (_     ⊞ _     ) _         = " "

      (_ ⊡ Lit 10 ) _ = ""
      (_ ⊡ Lit 100) _ = " "
      (_ ⊡ _      ) _ = " "

      quantityRepr s p1 p2 ctx =
          case ctx of
            CtxMul _ (Lit 1) _ → s
            CtxMul _ (Lit n) _ | n ≤ 4 → p1
            CtxMul {}          → p2
            _                  → s

      syms =
          M.fromList
          [ (0,  const                         "nula")
          , (1,  \c → case c of
                        CtxAdd _ (Lit 10)  _ → "jede"
                        CtxAdd {}            → "jedna"
                        _                    → "jeden"
            )
          , (2,  \c → case c of
                        CtxMul _ (Lit 100) _ → "dvě"
                        _                    → "dva"
            )
          , (3, const                          "tři")
          , (4,  \c → case c of
                        CtxAdd _ (Lit 10)  _ → "ctr"
                        _                    → "čtyři"
            )
          , (5,  \c → case c of
                        CtxAdd _ (Lit 10)  _ → "pat"
                        CtxMul _ (Lit 10)  _ → "pa"
                        _                    → "pět"
            )
          , (6,  \c → case c of
                        CtxMul _ (Lit 10)  _ → "še"
                        _                    → "šest"
            )
          , (7,  const                         "sedm")
          , (8,  const                         "osm")
          , (9,  \c → case c of
                        CtxAdd _ (Lit 10)  _ → "devate"
                        CtxMul _ (Lit 10)  _ → "devá"
                        _                    → "devět"
            )
          , (10,  \c → case c of
                         CtxAdd R (Lit n) _
                             | n ≤ 9     → "náct"
                         CtxMul R (Lit n) _
                             | n ≤ 4     → "cet"
                             | otherwise → "desát"
                         _               → "deset"
            )
          , (100, \c → case c of
                         CtxMul _ (Lit n) _
                             | n ≡ 2 → "stě"
                             | n ≤ 4 → "sta"
                             | n ≤ 9 → "set"
                         _           → "sto"
            )
          , (1000, \c → case c of
                          CtxMul _ (Lit n) _ | n ≤ 4 → "tisíce"
                          _                          → "tisíc"
            )
          ]
