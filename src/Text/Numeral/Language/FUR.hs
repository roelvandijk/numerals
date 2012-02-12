{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        fur

[@ISO639-3@]        fur

[@Native name@]     Furlan

[@English name@]    Friulan
-}

module Text.Numeral.Language.FUR
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

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.BigNum  as BN
import qualified "numerals-base" Text.Numeral.Exp     as E
import qualified "numerals-base" Text.Numeral.Grammar as G
import           "numerals-base" Text.Numeral.Misc ( dec )


--------------------------------------------------------------------------------
-- FUR
--------------------------------------------------------------------------------

cardinal ∷ ( G.Masculine i, G.Feminine i
           , Integral α, E.Scale α
           , Monoid s, IsString s
           )
         ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Add β, E.Mul β, E.Scale β
         )
       ⇒ α → β
struct = fix $ rule `combine` pelletierScale1 R L BN.rule
    where
      rule = findRule (   0, lit         )
                    [ (  11, add   10 L  )
                    , (  17, add   10 R  )
                    , (  20, lit         )
                    , (  21, add   20 R  )
                    , (  30, mul   10 R L)
                    , ( 100, step  100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                      (dec 6 - 1)

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 12 - 1 in (negate x, x)

cardinalRepr ∷ (G.Feminine i, G.Masculine i, Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \inf n → M.lookup n (syms inf)
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprScale = BN.pelletierRepr
                               (BN.quantityName "ilion"  "ilions")
                               (BN.quantityName "iliart" "iliarts")
                               []
               }
    where
      (Lit 20          ⊞ _) _ = "e"
      (Lit 100         ⊞ _) _ = " e "
      (Mul _ (Lit 100) ⊞ _) _ = " e "
      (_               ⊞ _) _ = ""

      (_ ⊡ Lit 1000) _ = " "
      (_ ⊡ Scale {}) _ = " "
      (_ ⊡ _       ) _ = ""

      syms inf =
          M.fromList
          [ (0, const "zero")
          , (1, \c → case c of
                       _ | G.isFeminine  inf → "une"
                         | otherwise         → "un"
            )
          , (2, \c → case c of
                       CtxAdd _ (Lit  10) _ → "do"
                       CtxMul _ (Lit 100) _ → "dus"
                       _ | G.isFeminine inf → "dôs"
                         | otherwise        → "doi"
            )
          , (3, \c → case c of
                       CtxAdd _ (Lit  10) _ → "tre"
                       CtxMul _ (Lit  10) _ → "tr"
                       CtxMul _ (Lit 100) _ → "tres"
                       _                    → "trê"
            )
          , (4, \c → case c of
                       CtxAdd _ (Lit 10) _ → "cutuar"
                       CtxMul _ (Lit 10) _ → "cuar"
                       _                   → "cuatri"
            )
          , (5, \c → case c of
                       CtxAdd _ (Lit 10) _ → "cuin"
                       CtxMul _ (Lit 10) _ → "cincu"
                       _                   → "cinc"
            )
          , (6, \c → case c of
                       CtxAdd _ (Lit 10) _ → "se"
                       CtxMul _ (Lit 10) _ → "sess"
                       _                   → "sîs"
            )
          , (7, \c → case c of
                       CtxMul _ (Lit 10) _ → "set"
                       _                   → "siet"
            )
          , (8, \c → case c of
                       CtxMul _ (Lit 10) _ → "ot"
                       _                   → "vot"
            )
          , (9, \c → case c of
                       CtxMul _ (Lit 10) _ → "nov"
                       _                   → "nûf"
            )
          , (10, \c → case c of
                        CtxAdd R _       _ → "dis"
                        CtxAdd L _       _ → "dise"
                        CtxMul _ (Lit 3) _ → "ente"
                        CtxMul _ (Lit _) _ → "ante"
                        _                  → "dîs"
            )
          , (20, \c → case c of
                        _ → "vincj"
            )
          , (100, \c → case c of
                         CtxMul _ (Lit n) _
                             | n ≤ 3     → "inte"
                             | otherwise → "cent"
                         _               → "cent"
            )
          , (1000, \c → case c of
                          _ → "mil"
            )
          ]
