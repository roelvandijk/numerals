{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        nl

[@ISO639-2B@]       dut

[@ISO639-3@]        nld

[@Native name@]     Nederlands

[@English name@]    Dutch
-}

module Text.Numeral.Language.NL
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
import "base-unicode-symbols" Data.List.Unicode     ( (∈) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.Exp.Classes as C
import qualified "numerals-base" Text.Numeral.BigNum as BN


--------------------------------------------------------------------------------
-- NL
--------------------------------------------------------------------------------

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

ordinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
ordinal = ordinalRepr ∘ struct


struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = pos $ fix $ rule `combine` pelletierScale R L BN.rule

rule ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β) ⇒ Rule α β
rule = findRule (   0, lit               )
              [ (  13, add    10      L  )
              , (  20, mul    10      L L)
              , ( 100, step  100   10 R L)
              , (1000, step 1000 1000 R L)
              ]
                (dec 6 - 1)

genericRepr ∷ (Monoid s, IsString s) ⇒ Repr s
genericRepr = defaultRepr
               { reprAdd   = Just (⊞)
               , reprMul   = Just $ \_ _ _ → ""
               , reprNeg   = Just $ \_ _   → "min "
               }
    where
      (_     ⊞ Lit 10) _         = ""
      (Lit n ⊞ _) _ | n ∈ [2,3]  = "ën"
                    | n < 10     = "en"
                    | otherwise  = ""
      (_     ⊞ _) _              = ""

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
          , (2, tenForms "twee" "twin")
          , (3, tenForms "drie" "der")
          , (4, tenForms "vier" "veer")
          , (5, const "vijf")
          , (6, const "zes")
          , (7, const "zeven")
          , (8, \c → case c of
                       CtxMul _ (Lit 10) _ → "tach"
                       CtxAdd _ (Lit _)  _ → "ach"
                       _                   → "acht"
            )
          , (9, const "negen")
          , (10, \c → case c of
                        CtxMul R _ _ → "tig"
                        _            → "tien"
            )
          , (11, const "elf")
          , (12, const "twaalf")
          , (100, const "honderd")
          , (1000, const "duizend")
          ]

      tenForms ∷ s → s → Ctx Exp → s
      tenForms n t ctx = case ctx of
                           CtxMul _ (Lit 10) _ → t
                           CtxAdd _ (Lit 10) _ → t
                           CtxAdd _ (Lit _)  _ → n
                           _                   → n

ordinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
ordinalRepr = render genericRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = BN.pelletierRepr ( BN.ordQuantityName "iljoen" "iljoenste"
                                                                   "iljoen" "iljoenste"
                                              )
                                              ( BN.ordQuantityName "iljard" "iljardste"
                                                                   "iljard" "iljardste"
                                              )
                                              []
               }
    where
      syms =
          M.fromList
          [ (0, const "nulde")
          , (1, \c → case c of
                       CtxEmpty → "eerste"
                       _ | isOutside R c → "eende"
                         | otherwise    → "een"
            )
          , (2, tenForms "tweede"  "twee"  "twin")
          , (3, tenForms "derde"   "drie"  "der")
          , (4, tenForms "vierde"  "vier"  "veer")
          , (5, tenForms "vijfde"  "vijf"  "vijf")
          , (6, tenForms "zesde"   "zes"   "zes")
          , (7, tenForms "zevende" "zeven" "zeven")
          , (8, \c → case c of
                       _ | isOutside R c → "achtste"
                       CtxMul _ (Lit 10) _ → "tach"
                       CtxAdd _ (Lit _)  _ → "ach"
                       _                   → "acht"
            )
          , (9, tenForms "negende" "negen" "negen")
          , (10, \c → case c of
                        CtxMul R _ _ | isOutside R c → "tigste"
                                     | otherwise     → "tig"
                        _            | isOutside R c → "tiende"
                                     | otherwise     → "tien"
            )
          , (11, tenForms "elfde"    "elf"    "elf")
          , (12, tenForms "twaalfde" "twaalf" "twaalf")
          , (100,  \c → if isOutside R c then "honderdste" else "honderd")
          , (1000, \c → if isOutside R c then "duizendste" else "duizend")
          ]

      tenForms ∷ s -- ^ Ordinal form.
               → s -- ^ Cardinal form.
               → s -- ^ Added to, or multiplied with, ten.
               → Ctx Exp
               → s
      tenForms o c t ctx = case ctx of
                             _ | isOutside R ctx → o
                             CtxMul _ (Lit 10) _ → t
                             CtxAdd _ (Lit _)  _ → t
                             _                   → c
