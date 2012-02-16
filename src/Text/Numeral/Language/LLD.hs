{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        lld

[@Native name@]     Ladin

[@English name@]    Ladin
-}

module Text.Numeral.Language.LLD
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
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
import "base-unicode-symbols" Data.Bool.Unicode     ( (∧), (∨) )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡), (≢) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.BigNum  as BN
import qualified "numerals-base" Text.Numeral.Exp     as E
import qualified "numerals-base" Text.Numeral.Grammar as G
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "this" Text.Numeral.Language.IT as IT ( rule )
import           "this" Text.Numeral.Entry


--------------------------------------------------------------------------------
-- FUR
--------------------------------------------------------------------------------

entry ∷ (Monoid s, IsString s) ⇒ Entry s
entry = emptyEntry
    { entIso639_3    = Just "lld"
    , entNativeNames = ["Ladin"]
    , entEnglishName = Just "Ladin"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

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
struct = fix $ IT.rule `combine` pelletierScale1 R L BN.rule

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 12 - 1 in (negate x, x)

cardinalRepr ∷ (G.Feminine i, G.Masculine i, Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \inf n → M.lookup n (syms inf)
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprScale = BN.pelletierRepr
                               (BN.quantityName "ilion"  "ilion")
                               (BN.quantityName "iliard" "iliard")
                               []
               }
    where
      (             Lit 20  ⊞ Lit n) _ | n ≢ 1 ∧          n ≢ 8  = "e"
      ((Lit t `Mul` Lit 10) ⊞ Lit n) _ | n ≢ 1 ∧ (t ≡ 3 ∨ n ≢ 8) = "e"
      (         Lit 100  ⊞ _) _ = "e"
      ((_ `Mul` Lit 100) ⊞ _) _ = "e"
      ((_ `Mul` Scale{}) ⊞ _) _ = "e"
      (_                 ⊞ _) _ = ""

      (_ ⊡ Scale {}) _ = " "
      (_ ⊡ _       ) _ = ""

      syms inf =
          M.fromList
          [ (0, const "zero")
          , (1, add10 "un"
                $ \c → case c of
                         _ | G.isFeminine inf → "una"
                           | otherwise        → "un"
            )
          , (2, add10 "do"
                $ \c → case c of
                         _ | G.isFeminine inf → "does"
                           | otherwise        → "doi"
            )
          , (3, add10 "tre"
                $ mul10 "tr"
                $ \c → case c of
                         _ → "trei"
            )
          , (4, add10 "cator"
                $ mul10 "car"
                $ \c → case c of
                         _ → "cater"
            )
          , (5, add10 "chin"
                $ mul10 "cinc"
                $ \c → case c of
                         _ → "cinch"
            )
          , (6, add10 "sei"
                $ mul10 "sess"
                $ \c → case c of
                         _ → "sies"
            )
          , (7, \c → case c of
                       _ → "set"
            )
          , (8, add10 "dot"
                $ \c → case c of
                         _ → "ot"
            )
          , (9, mul10 "non"
                $ \c → case c of
                         _ → "nuef"
            )
          , (10, \c → case c of
                        CtxAdd _ (Lit n) _
                            | n ≤ 6 → "desc"
                            | n ≡ 7 → "dejes"
                            | n ≤ 9 → "deje"
                        CtxMul _ (Lit 3) (CtxAdd {}) → "ent"
                        CtxMul _ (Lit 3) _ → "enta"
                        CtxMul _ (Lit _) (CtxAdd {}) → "ant"
                        CtxMul _ (Lit _) _ → "anta"
                        _ → "diesc"
            )
          , (20, \c → case c of
                        _ → "vint"
            )
          , (100, \c → case c of
                         CtxMul _ (Lit 6) _ → "çent"
                         _ → "cent"
            )
          , (1000, \c → case c of
                          _ → "mile"
            )
          ]

      add10 ∷ s → (Ctx (Exp i) → s) → Ctx (Exp i) → s
      add10 a o = \c → case c of
                         CtxAdd _ (Lit 10) _ → a
                         _ → o c

      mul10 ∷ s → (Ctx (Exp i) → s) → Ctx (Exp i) → s
      mul10 m o = \c → case c of
                         CtxMul _ (Lit 10) _ → m
                         _ → o c
