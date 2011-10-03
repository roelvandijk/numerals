{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        pl

[@ISO639-2@]        pol

[@ISO639-3@]        pol

[@Native name@]     język polski

[@English name@]    Polish
-}

module Text.Numeral.Language.PL
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
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≥), (≤) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.BigNum as BN
import qualified "numerals-base" Text.Numeral.Exp    as E


-------------------------------------------------------------------------------
-- PL
-------------------------------------------------------------------------------

-- Sources:
--   Krzysztof Skrzętnicki
--   http://www.polishforums.com/archives/2009/general-language-17/numbers-polish-language-6722/

-- | liczebniki główne
cardinal ∷ (Integral α, E.Scale α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
         )
       ⇒ α → β
struct = pos
       $ fix
       $ findRule (   0, lit               )
                [ (  11, add    10      L  )
                , (  20, step   10   10 R L)
                , ( 100, step  100   10 R L)
                , (1000, step 1000 1000 R L)
                ]
                  (dec 6 - 1)
         `combine` pelletierScale R L BN.rule

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 60000 - 1 in (negate x, x)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprScale = BN.pelletierRepr (quantityRepr "ilion"  "iliony"  "ilionów")
                                              (quantityRepr "iliard" "iliardy" "iliardów")
                                              []
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               }
    where
      (Lit n ⊞ Lit 10) _ | n ≤ 9 = ""
      (_     ⊞ _     ) _         = " "

      (_ ⊡ Lit 10 ) _ = ""
      (_ ⊡ Lit 100) _ = ""
      (_ ⊡ _      ) _ = " "

      quantityRepr s p1 p2 ctx =
          case ctx of
            CtxMul _ (Lit 1) _ → s
            CtxMul _ (Lit n) _ | n ≤ 4 → p1
            CtxMul {}          → p2
            _                  → s

      syms =
          M.fromList
          [ (0,  const                         "zero")
          , (1,  \c → case c of
                        CtxAdd _ (Lit 10)  _ → "jede"
                        _                    → "jeden"
            )
          , (2,  \c → case c of
                        CtxMul _ (Lit 100) _ → "dwie"
                        _                    → "dwa"
            )
          , (3, const                          "trzy")
          , (4,  \c → case c of
                        CtxAdd _ (Lit 10)  _ → "czter"
                        CtxMul _ (Lit 10)  _ → "czter"
                        _                    → "cztery"
            )
          , (5,  \c → case c of
                        CtxAdd _ (Lit 10)  _ → "pięt"
                        _                    → "pięć"
            )
          , (6,  \c → case c of
                        CtxAdd _ (Lit 10)  _ → "szes"
                        _                    → "sześć"
            )
          , (7,  const                         "siedem")
          , (8,  const                         "osiem")
          , (9,  \c → case c of
                        CtxAdd _ (Lit 10)  _ → "dziewięt"
                        _                    → "dziewięć"
            )
          , (10,  \c → case c of
                         CtxAdd R (Lit n) _ | n ≤ 9 → "naście"
                         CtxMul R (Lit n) _
                             | n ≡ 2     → "dzieścia"
                             | n ≥ 5     → "dziesiąt"
                             | otherwise → "dzieści"
                         _               → "dziesięć"
            )
          , (100, \c → case c of
                         CtxMul _ (Lit n) _
                             | n ≡ 2 → "ście"
                             | n ≤ 4 → "sta"
                             | n ≤ 9 → "set"
                         _           → "sto"
            )
          , (1000, \c → case c of
                          CtxMul _ (Lit n) _ | n ≤ 4 → "tysiące"
                          CtxMul {}                  → "tysięcy"
                          _                          → "tysiąc"
            )
          ]
