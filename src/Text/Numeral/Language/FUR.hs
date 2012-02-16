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
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.BigNum  as BN
import qualified "numerals-base" Text.Numeral.Exp     as E
import qualified "numerals-base" Text.Numeral.Grammar as G
import           "numerals-base" Text.Numeral.Misc ( dec )
import           "this" Text.Numeral.Entry
import qualified "this" Text.Numeral.Language.IT as IT ( rule )
import           "this" Text.Numeral.Render.Utils ( addCtx, mulCtx )


--------------------------------------------------------------------------------
-- FUR
--------------------------------------------------------------------------------

entry ∷ (Monoid s, IsString s) ⇒ Entry s
entry = emptyEntry
    { entIso639_2    = ["fur"]
    , entIso639_3    = Just "fur"
    , entNativeNames = ["Furlan", "marilenghe"]
    , entEnglishName = Just "Friulan"
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
                               (BN.quantityName "ilion"  "ilions")
                               (BN.quantityName "iliart" "iliarts")
                               []
               }
    where
      (         Lit   20  ⊞ _                        ) _ = "e"
      (         Lit  100  ⊞ _                        ) _ = " e "
      (         Lit 1000  ⊞ (        Lit 100 `Add` _)) _ = " "
      (         Lit 1000  ⊞ (_ `Mul` Lit 100 `Add` _)) _ = " "
      (         Lit 1000  ⊞ _                        ) _ = " e "
      ((_ `Mul` Lit  100) ⊞ _                        ) _ = " e "
      ((_ `Mul` Lit 1000) ⊞ (        Lit 100 `Add` _)) _ = " "
      ((_ `Mul` Lit 1000) ⊞ (_ `Mul` Lit 100 `Add` _)) _ = " "
      ((_ `Mul` Lit 1000) ⊞ _                        ) _ = " e "
      (_                  ⊞ _                        ) _ = ""

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
          , (2, addCtx 10 "do" $ mulCtx 100 "dus"
                $ \c → case c of
                         _ | G.isFeminine inf → "dôs"
                           | otherwise        → "doi"
            )
          , (3, addCtx 10  "tre"  $ mulCtx 10 "tr"
              $ mulCtx 100 "tres" $ const     "trê"
            )
          , (4, addCtx 10 "cutuar" $ mulCtx 10 "cuar"  $ const "cuatri")
          , (5, addCtx 10 "cuin"   $ mulCtx 10 "cincu" $ const "cinc")
          , (6, addCtx 10 "se"     $ mulCtx 10 "sess"  $ const "sîs")
          , (7,                      mulCtx 10 "set"   $ const "siet")
          , (8,                      mulCtx 10 "ot"    $ const "vot")
          , (9,                      mulCtx 10 "nov"   $ const "nûf")
          , (10, \c → case c of
                        CtxAdd R (Lit n) _
                            | n ≤ 7        → "dis"
                        CtxAdd L _       _ → "dise"
                        CtxMul _ (Lit 3) _ → "ente"
                        CtxMul _ (Lit n) _
                            | n ≤ 9        → "ante"
                        _                  → "dîs"
            )
          , (20, const "vincj")
          , (100, \c → case c of
                         CtxMul _ (Lit n) _
                             | n ≤ 3     → "inte"
                             | otherwise → "cent"
                         _               → "cent"
            )
          , (1000, const "mil")
          ]
