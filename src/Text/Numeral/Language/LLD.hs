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
import "base" Data.Function ( ($), const )
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
import "this" Text.Numeral.Entry
import "this" Text.Numeral.Language.FUR ( struct )
import "this" Text.Numeral.Render.Utils ( addCtx, mulCtx )


--------------------------------------------------------------------------------
-- LLD
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
          , (1, addCtx 10 "un"
                $ \c → case c of
                         _ | G.isFeminine inf → "una"
                           | otherwise        → "un"
            )
          , (2, addCtx 10 "do"
                $ \c → case c of
                         _ | G.isFeminine inf → "does"
                           | otherwise        → "doi"
            )
          , (3, addCtx 10 "tre"   $ mulCtx 10 "tr"   $ const "trei")
          , (4, addCtx 10 "cator" $ mulCtx 10 "car"  $ const "cater")
          , (5, addCtx 10 "chin"  $ mulCtx 10 "cinc" $ const "cinch")
          , (6, addCtx 10 "sei"   $ mulCtx 10 "sess" $ const "sies")
          , (7, const "set")
          , (8, addCtx 10 "dot" $ const "ot")
          , (9, mulCtx 10 "non" $ const "nuef")
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
          , (20, const "vint")
          , (100, mulCtx 6 "çent" $ const "cent")
          , (1000, const "mile")
          ]
