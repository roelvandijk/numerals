{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        pt

[@ISO639-2@]        por

[@ISO639-3@]        por

[@Native name@]     Português

[@English name@]    Portuguese
-}

module Text.Numeral.Language.PT
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
    , ordinal
      -- * Structure
    , cardinal_struct
    , ordinal_struct
      -- * Bounds
    , bounds
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Ord      ( (<) )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.List.Unicode     ( (∉) )
import "base-unicode-symbols" Data.Monoid.Unicode   ( (⊕) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import "base-unicode-symbols" Prelude.Unicode       ( ℤ )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum  as BN
import qualified "this" Text.Numeral.Exp     as E
import           "this" Text.Numeral.Grammar as G
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


-------------------------------------------------------------------------------
-- PT
-------------------------------------------------------------------------------

entry ∷ Entry
entry = emptyEntry
    { entIso639_1    = Just "pt"
    , entIso639_2    = ["por"]
    , entIso639_3    = Just "por"
    , entNativeNames = ["Português"]
    , entEnglishName = Just "Portuguese"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = cardinal_struct
                       }
    , entOrdinal     = Just Conversion
                       { toNumeral   = ordinal
                       , toStructure = ordinal_struct
                       }
    }

cardinal ∷ (G.Feminine i, G.Masculine i, Integral α, E.Scale α)
         ⇒ i → α → Maybe Text
cardinal inf = cardinalRepr inf ∘ cardinal_struct

ordinal ∷ (G.Feminine i, G.Masculine i, G.Singular i, Integral α, E.Scale α)
         ⇒ i → α → Maybe Text
ordinal inf = ordinalRepr inf ∘ ordinal_struct

cardinal_struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
         , E.Inflection β, G.Masculine (E.Inf β)
         )
       ⇒ α → β
cardinal_struct = pos $ fix $ rule `combine` shortScale1_pt
    where
      rule = findRule (   0, lit       )
                    [ (  11, add 10 L  )
                    , (  16, add 10 R  )
                    , (  20, mul 10 R L)
                    , ( 100, step  100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                      (dec 6 - 1)

ordinal_struct ∷ ( Integral α, E.Scale α
                 , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
                 , E.Inflection β, G.Masculine (E.Inf β)
                 )
                 ⇒ α → β
ordinal_struct = pos $ fix $ rule `combine` shortScale1_pt
    where
      rule = findRule (   0, lit       )
                    [ (  11, add 10 R  )
                    , (  20, mul 10 R L)
                    , ( 100, step  100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                      (dec 6 - 1)

-- | Like 'shortScale1' with the difference that all scale elements
-- are masculine.
shortScale1_pt ∷ ( Integral α, E.Scale α
                , E.Unknown β, E.Lit β, E.Add β, E.Mul β, E.Scale β
                , E.Inflection β, G.Masculine (E.Inf β)
                )
              ⇒ Rule α β
shortScale1_pt = mulScale1_es 3 3 R L BN.rule
    where
      mulScale1_es = mulScale_ $ \f m s _ → masculineMul (f m) s
      masculineMul x y = E.inflection (G.masculine) $ E.mul x y

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 60000 - 1 in (negate x, x)

cardinalRepr ∷ (G.Feminine i) ⇒ i → Exp i → Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \inf n → M.lookup n (syms inf)
               , reprScale = shortScaleRepr
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _ → "menos "
               }
    where
      (Lit 10 ⊞ Lit n ) _ | n < 8     = "as"
                          | n ≡ 8     = ""
                          | otherwise = "a"
      (Lit _  ⊞ Lit 10) _             = ""
      (_      ⊞ _     ) _             = " e "


      (_ ⊡ Lit 10 ) _ = ""
      (_ ⊡ Lit 100) _ = ""
      (_ ⊡ _      ) _ = " "

      syms inf =
          M.fromList
          [ (0, const "zero")
          , (1, \c → case c of
                       CtxAdd _ (Lit 10) _  → "on"
                       _ | G.isFeminine inf → "uma"
                         | otherwise        → "um"
            )
          , (2, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "do"
                       CtxMul _ (Lit 10)  _ → "vin"
                       CtxMul _ (Lit 100) _ → "duz"
                       _ | G.isFeminine inf → "duas"
                         | otherwise        → "dois"
            )
          , (3, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "tre"
                       CtxMul _ (Lit 10)  _ → "trin"
                       CtxMul _ (Lit 100) _ → "trez"
                       _                    → "três"
            )
          , (4, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "cator"
                       CtxMul _ (Lit 10)  _ → "quaren"
                       _                    → "quatro"
            )
          , (5, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "quin"
                       CtxMul _ (Lit 10)  _ → "cinquen"
                       CtxMul _ (Lit 100) _ → "quin"
                       _                    → "cinco"
            )
          , (6, \c → case c of
                       CtxMul _ (Lit 10) _ → "sessen"
                       _                   → "seis"
            )
          , (7, \c → case c of
                       CtxMul _ (Lit 10) _ → "seten"
                       _                   → "sete"
            )
          , (8, \c → case c of
                       CtxMul _ (Lit 10) _ → "oiten"
                       _                   → "oito"
            )
          , (9, \c → case c of
                       CtxMul _ (Lit 10) _ → "noven"
                       _                   → "nove"
            )
          , (10, \c → case c of
                        CtxAdd R (Lit _) _ → "ze"
                        CtxMul R (Lit 2) _ → "te"
                        CtxMul R (Lit _) _ → "ta"
                        _                  → "dez"
            )
          , (100, \c → case c of
                         CtxAdd {} → "cento"
                         CtxMul _ (Lit n) _
                             | n ≤ 3 → if G.isFeminine inf then "entas"  else "entos"
                             | n ≡ 5 → if G.isFeminine inf then "hentas" else "hentos"
                             | n ≤ 9 → if G.isFeminine inf then "centas" else "centos"
                             | otherwise → "cem"
                         _ → "cem"
            )
          , (1000, const "mil")
          ]

ordinalRepr ∷ (G.Feminine i, G.Singular i) ⇒ i → Exp i → Maybe Text
ordinalRepr = render defaultRepr
              { reprValue = \inf n → M.lookup n (syms inf)
              , reprScale = shortScaleRepr
              , reprAdd   = Just (⊞)
              , reprMul   = Just (⊡)
              , reprNeg   = Just $ \_ _ → "menos "
              }
    where
      (Lit _  ⊞ Lit 10) _ = ""
      (_      ⊞ _     ) _ = " "

      (_ ⊡ Lit 10 ) _ = ""
      (_ ⊡ Lit 100) _ = ""
      (_ ⊡ _      ) _ = " "

      syms inf =
          M.fromList
          [ (0, const "zero")
          , (1, \c → case c of
                       _ → "primeir" ⊕ postFix
            )
          , (2, \c → case c of
                       CtxMul _ (Lit 10)  _ → "vi"
                       CtxMul _ (Lit 100) _ → "du"
                       _                    → "segund" ⊕ postFix
            )
          , (3, \c → case c of
                       CtxMul _ (Lit 10)  _ → "tri"
                       CtxMul _ (Lit 100) _ → "tre"
                       _                    → "terceir" ⊕ postFix
            )
          , (4, \c → case c of
                       CtxMul _ (Lit 10)  _ → "quadra"
                       CtxMul _ (Lit 100) _ → "quadrin"
                       _                    → "quart" ⊕ postFix
            )
          , (5, \c → case c of
                       CtxMul _ (Lit 10)  _ → "qüinqua"
                       CtxMul _ (Lit 100) _ → "qüin"
                       _                    → "quint" ⊕ postFix
            )
          , (6, \c → case c of
                       CtxMul _ (Lit 10)  _ → "sexa"
                       CtxMul _ (Lit 100) _ → "sex"
                       _                    → "sext" ⊕ postFix
            )
          , (7, \c → case c of
                       CtxMul _ (Lit 10)  _ → "septua"
                       CtxMul _ (Lit 100) _ → "setin"
                       _                    → "sétim" ⊕ postFix
            )
          , (8, \c → case c of
                       CtxMul _ (Lit 10)  _ → "octo"
                       CtxMul _ (Lit 100) _ → "octin"
                       _                    → "oitav" ⊕ postFix
            )
          , (9, \c → case c of
                       CtxMul _ (Lit 10)  _ → "nona"
                       CtxMul _ (Lit 100) _ → "non"
                       _                    → "non" ⊕ postFix
            )
          , (10, \c → case c of
                        CtxAdd R (Lit _) _  → "ze"
                        CtxMul R (Lit _) _
                            | isOutside R c → "gésim" ⊕ postFix
                            | otherwise     → "gésimo"
                        _ | isOutside R c   → "décim" ⊕ postFix
                          | otherwise       → "décimo"
            )
          , (100, \c → case c of
                         CtxAdd {}         → "cento"
                         CtxMul _ (Lit n) _
                             | n ∉ [2,3,6] → "gentésim" ⊕ postFix
                         _                 → "centésim" ⊕ postFix
            )
          , (1000, const $ "milésim" ⊕ postFix)
          ]
          where
            postFix = if G.isFeminine inf
                      then if G.isSingular inf
                           then "a"
                           else "as"
                      else if G.isSingular inf
                           then "o"
                           else "os"

shortScaleRepr ∷ i → ℤ → ℤ → Exp i → Ctx (Exp i) → Maybe Text
shortScaleRepr =
    BN.scaleRepr (BN.quantityName "ilhão" "ilhões")
                 [(4, BN.forms "quatr" "quator" "quator" "quatra" "quatri")]
