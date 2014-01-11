{-# LANGUAGE FlexibleContexts
           , NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        bg

[@ISO639-2@]        bul

[@ISO639-3@]        bul

[@Native name@]     Български език

[@English name@]    Bulgarian
-}

module Text.Numeral.Language.BG
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
import "base-unicode-symbols" Prelude.Unicode       ( ℤ )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum  as BN
import qualified "this" Text.Numeral.Exp     as E
import qualified "this" Text.Numeral.Grammar as G
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry


--------------------------------------------------------------------------------
-- BG
--------------------------------------------------------------------------------

entry ∷ (Monoid s, IsString s) ⇒ Entry s
entry = emptyEntry
    { entIso639_1    = Just "bg"
    , entIso639_2    = ["bul"]
    , entIso639_3    = Just "bul"
    , entNativeNames = ["Български език"]
    , entEnglishName = Just "Bulgarian"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal ∷ ( G.Neuter i, G.Feminine i, G.Masculine i
           , Integral α, E.Scale α, Monoid s, IsString s
           )
         ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
         , E.Inflection β, G.Masculine (E.Inf β)
         )
       ⇒ α → β
struct = pos $ fix $ rule `combine` shortScale1_bg
    where
       rule = findRule (   0, lit               )
                     [ (  13, add 10 L          )
                     , (  20, mul 10 R L        )
                     , ( 100, step  100   10 R L)
                     , (1000, step 1000 1000 R L)
                     ]
                     (dec 6 - 1)

-- | Like 'shortScale1' but forces the right-hand-side to have
-- masculine gender.
shortScale1_bg ∷ ( Integral α, E.Scale α
                 , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
                 , E.Inflection β, G.Masculine (E.Inf β)
                 )
               ⇒ Rule α β
shortScale1_bg = mulScale_ bgMul 3 3 R L rule
    where
      bgMul f m scale' _ = masculineMul (f m) scale'
      masculineMul x y = E.inflection G.masculine $ E.mul x y
      rule = findRule (1, lit) [] 14

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 48 - 1 in (negate x, x)

cardinalRepr ∷ ( G.Neuter i, G.Feminine i, G.Masculine i
               , Monoid s, IsString s
               )
             ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \inf n → M.lookup n (syms inf)
               , reprScale = shortScaleRepr
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _ → "минус "
               }
    where
      (Lit 100 ⊞  Lit _) _          = " и "
      (_ ⊞ Add _ (Lit 10)) _        = " и "
      (_ ⊞ Mul _ (Lit 10)) _        = " и "
      (_ ⊞ Lit 10) _                = ""
      ((_ `Mul` Lit _) ⊞ Lit _) _   = " и "
      (Inflection _ _ ⊞ Lit _) _    = " и "
      (_     ⊞ _     ) _            = " "

      (_ ⊡ Lit n) _ | n ≤ 100 = ""
      (_ ⊡ _    ) _           = " "

      syms inf =
          M.fromList
          [ (0, const "нула")
          , (1, const $ if G.isFeminine inf
                        then "една"
                        else if G.isMasculine inf
                             then "един"
                             else "едно"
            )
          , (2, \c → case c of
                       CtxMul _ (Lit   10) _ → "два"
                       CtxMul _ (Lit  100) _ → "две"
                       CtxMul _ (Lit 1000) _ → "две"
                       _ | G.isMasculine inf → "два"
                         | otherwise         → "две"
            )
          , (3, const "три")
          , (4, const "четири")
          , (5, const "пет")
          , (6, const "шест")
          , (7, const "седем")
          , (8, const "осем")
          , (9, const "девет")
          , (10, \c → case c of
                        CtxAdd _ (Lit n) _ | n ≤ 9 → "надесет"
                        _                          → "десет"
            )
          , (11, const "единадесет")
          , (12, const "дванадесет")
          , (100, \c → case c of
                         CtxMul _ (Lit 2) _ → "ста"
                         CtxMul _ (Lit 3) _ → "ста"
                         CtxMul R _ _       → "стотин"
                         _                  → "сто"
            )
          , (1000, \c → case c of
                          CtxMul R _ _ → "хиляди"
                          _            → "хиляда"
            )
          ]

shortScaleRepr ∷ (IsString s, Monoid s)
               ⇒ i → ℤ → ℤ → (Exp i) → Ctx (Exp i) → Maybe s
shortScaleRepr inf b o e
    = case e of
        Lit 2 → BN.scaleRepr (BN.quantityName "илиард" "илиарда") syms inf b o e
        _ → BN.scaleRepr (BN.quantityName "илион" "илиона") syms inf b o e
    where
      syms = [ ( 1, const "м")
             , ( 2, const "м")
             , ( 3, const "тр")
             , ( 4, const "квадр")
             , ( 5, const "квинт")
             , ( 6, const "секст")
             , ( 7, const "септ")
             , ( 8, const "окт")
             , ( 9, const "нон")
             , (10, const "дец")
             , (11, const "индец")
             , (12, const "дуодец")
             , (13, const "тридец")
             , (14, const "куадродец")
             ]
