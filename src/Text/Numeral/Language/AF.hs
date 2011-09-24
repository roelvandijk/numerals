{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        af

[@ISO639-2@]        afr

[@ISO639-3@]        afr

[@Native name@]     Afrikaans

[@English name@]    Afrikaans
-}

module Text.Numeral.Language.AF
    ( cardinal
    , ordinal
    , struct
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.Ord      ( (<) )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.Exp.Classes as C
import qualified "numerals-base" Text.Numeral.BigNum as BN


--------------------------------------------------------------------------------
-- AF
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-afrikaans/en/afr/
  http://mylanguages.org/afrikaans_numbers.php
-}

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

ordinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
ordinal = ordinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = pos
       $ fix
       $ findRule (   0, lit                )
                [ (  13, add     10      L  )
                , (  20, mul     10      L L)
                , ( 100, step1  100   10 R L)
                , (1000, step1 1000 1000 R L)
                ]
                  (dec 6 - 1)
         `combine` pelletierScale1 R L BN.rule

genericRepr ∷ (Monoid s, IsString s) ⇒ Repr s
genericRepr = defaultRepr
               { reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _   → "min "
               }
    where
      (_     ⊞ Lit 10) _         = ""
      (Lit n ⊞ _) _ | n < 10     = "-en-"
                    | otherwise  = ""
      (_     ⊞ _) _              = " "

      (_ ⊡ Lit _) _ = ""
      (_ ⊡ _    ) _ = " "

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render genericRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = BN.pelletierRepr (const "iljoen")
                                              (const "iljard")
                                              []
               }
    where
      syms =
          M.fromList
          [ (0, const "nul")
          , (1, const "een")
          , (2, forms "twee" "twin")
          , (3, forms "drie" "der")
          , (4, forms "vier" "veer")
          , (5, const "vyf")
          , (6, const "ses")
          , (7, forms "sewe" "sewen")
          , (8, \c → case c of
                       CtxMul _ (Lit 10) _ → "tag"
                       _                   → "ag"
            )
          , (9, \c → case c of
                       CtxAdd _ (Lit 10) _ → "negen"
                       CtxMul _ (Lit 10) _ → "neën"
                       _                   → "nege"
            )
          , (10, \c → case c of
                        CtxMul R _ _ → "tig"
                        _            → "tien"
            )
          , (11, const "elf")
          , (12, const "twaalf")
          , (100, const "honderd")
          , (1000, const "duisend")
          ]

      forms ∷ s → s → Ctx Exp → s
      forms n t ctx = case ctx of
                        CtxMul _ (Lit 10) _ → t
                        CtxAdd _ (Lit 10) _ → t
                        CtxAdd _ (Lit _)  _ → n
                        _                   → n

ordinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
ordinalRepr = render genericRepr
              { reprValue = \n → M.lookup n syms
              , reprScale = BN.pelletierRepr (const "iljoen")
                                             (const "iljard")
                                             []
              }
    where
      syms =
          M.fromList
          [ (0, const "nul")
          , (1, \c → case c of
                       CtxEmpty          → "eerste"
                       _ | isOutside R c → "eende"
                         | otherwise     → "een"
            )
          , (2, forms "tweede" "twee" "twin")
          , (3, forms "derde"  "drie" "der")
          , (4, forms "vierde" "vier" "veer")
          , (5, \c → if isOutside R c then "vyfde" else "vyf")
          , (6, \c → if isOutside R c then "sesde" else "ses")
          , (7, forms "sewende" "sewe" "sewen")
          , (8, \c → case c of
                       _ | isOutside R c   → "agtste"
                       CtxMul _ (Lit 10) _ → "tag"
                       _                   → "ag"
            )
          , (9, \c → case c of
                       _ | isOutside R c   → "negende"
                       CtxAdd _ (Lit 10) _ → "negen"
                       CtxMul _ (Lit 10) _ → "neën"
                       _                   → "nege"
            )
          , (10, \c → case c of
                        CtxMul R _ _ | isOutside R c → "tigste"
                                     | otherwise     → "tig"
                        _            | isOutside R c → "tiende"
                                     | otherwise     → "tien"
            )
          , (11, \c → if isOutside R c then "elfde" else "elf")
          , (12, \c → if isOutside R c then "twaalfde" else "twaalf")
          , (100, \c → if isOutside R c then "honderste" else "honderd")
          , (1000, \c → if isOutside R c then "duisendste" else "duisend")
          ]

      forms ∷ s → s → s → Ctx Exp → s
      forms o c t ctx = case ctx of
                          _ | isOutside R ctx → o
                          CtxMul _ (Lit 10) _ → t
                          CtxAdd _ (Lit 10) _ → t
                          CtxAdd _ (Lit _)  _ → c
                          _                   → c
