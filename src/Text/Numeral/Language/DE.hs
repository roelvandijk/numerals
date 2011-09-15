{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        de

[@ISO639-2B@]       ger

[@ISO639-3@]        deu

[@Native name@]     Deutsch

[@English name@]    German
-}

module Text.Numeral.Language.DE
    ( cardinal
    , struct
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-), Integer )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.List.Unicode     ( (∈) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≥) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.BigNum      as BN
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- DE
-------------------------------------------------------------------------------

{-
Sources:
  http://de.wikipedia.org/wiki/Zahlennamen
-}

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = pos $ fix $ rule `combine` pelletierScale R L BN.rule
    where
      rule = findRule (   0, lit       )
                    [ (  13, add 10 L  )
                    , (  20, mul 10 L L)
                    , ( 100, step1 100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                      (dec 6 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = pelletierRepr
               , reprAdd   = Just (⊞)
               , reprMul   = Just $ \_ _ _ → ""
               , reprNeg   = Just $ \_ _   → "minus "
               }
    where
      (_ ⊞ (_ `Mul` Lit 10)) _ = "und"
      (_ ⊞ _               ) _ = ""

      syms =
          M.fromList
          [ (0, const "null")
          , (1, \c → case c of
                       CtxAdd {}        → "ein"
                       CtxMul _ (Lit n) _
                           | n ≥ dec 6  → "eine"
                           | n ≥ 100    → "ein"
                       _                → "eins"
            )
          , (2, \c → case c of
                       CtxMul _ (Lit 10) _ → "zwan"
                       _                   → "zwei"
            )
          , (3, const "drei")
          , (4, const "vier")
          , (5, const "fünf")
          , (6, \c → case c of
                       CtxAdd _ (Lit 10) _ → "sech"
                       CtxMul _ (Lit 10) _ → "sech"
                       _                   → "sechs"
            )
          , (7, \c → case c of
                       CtxAdd _ (Lit 10) _ → "sieb"
                       CtxMul _ (Lit 10) _ → "sieb"
                       _                   → "sieben"
            )
          , (8, const "acht")
          , (9, const "neun")
          , (10, \c → case c of
                        CtxMul _ (Lit 3) _ → "ßig"
                        CtxMul _ (Lit _) _ → "zig"
                        _                  → "zehn"
            )
          , (11, const "elf")
          , (12, const "zwölf")
          , (100, const "hundert")
          , (1000, const "tausend")
          ]

pelletierRepr ∷ (IsString s, Monoid s)
              ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
pelletierRepr =
    BN.pelletierRepr
      "illion"   "illion"
      "illiarde" "illiarde"
      [ (8, BN.forms "okt" "okto" "okto" "okto" "oktin")
      , (10, \c → case c of
                    CtxAdd _ (Lit 100) _              → "dezi"
                    CtxMul _ _ (CtxAdd _ (Lit 100) _) → "ginta"
                    CtxMul {}                         → "gint"
                    _                                 → "dez"
        )
      , (100, \c → case c of
                     CtxMul _ (Lit n) _
                         | n ∈ [2,3,6] → "zent"
                         | otherwise   → "gent"
                     _                 → "zent"
        )
      ]
