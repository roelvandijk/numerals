{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        pdc

[@Native name@]     Pennsilfaanisch Deitsch

[@English name@]    Pennsylvania German
-}

module Text.Numeral.Language.PDC
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
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
import "base" Data.Maybe    ( Maybe(Nothing, Just) )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum as BN
import qualified "this" Text.Numeral.Exp    as E
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


-------------------------------------------------------------------------------
-- PDC
-------------------------------------------------------------------------------

entry ∷ Entry
entry = emptyEntry
    { entIso639_1    = Nothing
    , entIso639_2    = []
    , entIso639_3    = Just "pdc"
    , entNativeNames = ["Pennsilfaanisch Deitsch"]
    , entEnglishName = Just "Pennsylvania German"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    , entOrdinal     = Nothing
    }

cardinal ∷ (Integral α, E.Scale α) ⇒ i → α → Maybe Text
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
         )
       ⇒ α → β
struct = pos
       $ fix
       $ findRule (   0, lit       )
                [ (  13, add 10 L  )
                , (  20, mul 10 L L)
                , ( 100, step1 100    10 R L)
                , (1000, step1 1000 1000 R L)
                ]
                  (dec 6 - 1)
         `combine` pelletierScale1 R L BN.rule

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (0, dec 9 - 1)

genericRepr ∷ Repr i
genericRepr = defaultRepr
              { reprAdd   = Just (⊞)
              , reprMul   = Just (⊡)
              , reprNeg   = Just $ \_ _ → "minus "
              }
    where
      (Lit n ⊞ (_ `Mul` Lit 10)) _ | n ≤ 9 = "un"
      ((Lit _ `Mul` Lit 100) ⊞ Lit _ ) _ = " un "
      (_ ⊞ Lit 10) _ = ""
      (_ ⊞ _     ) _ = " "

      (_ ⊡ Lit 10) _ = ""
      (_ ⊡ _)      _ = " "


cardinalRepr ∷ i → Exp i → Maybe Text
cardinalRepr = render genericRepr
               { reprValue = \_ n → M.lookup n syms
               , reprScale = BN.pelletierRepr (BN.quantityName "illyon"  "illyon")
                                              (BN.quantityName "illyard" "illyard")
                                              []
               }
    where
      syms =
          M.fromList
          [ (0, const "null")
          , (1, \c → case c of
                       CtxAdd L _ _  → "een"
                       CtxMul _ _ _  → "en"
                       _             → "eens"
            )
          , (2, \c → case c of
                       CtxMul _ (Lit 10) _ → "zwan"
                       _                   → "zwee"
            )
          , (3, const "drei")
          , (4, \c → case c of
                       CtxAdd _ (Lit 10) _ → "va"
                       CtxMul _ (Lit 10) _ → "va"
                       _                   → "vier"
            )
          , (5, \c → case c of
                       CtxAdd _ (Lit 10) _ → "fuff"
                       CtxMul _ (Lit 10) _ → "fuff"
                       _                   → "fimf"
            )
          , (6, \c → case c of
                       CtxAdd _ (Lit 10) _ → "sech"
                       CtxMul _ (Lit 10) _ → "sech"
                       _                   → "sex"
            )
          , (7, const "siwwe")
          , (8, const "acht")
          , (9, const "nein")
          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ → "zeh"
                        CtxMul _ (Lit 3) _ → "ssich"
                        CtxMul R (Lit n) _ | n ≤ 3     → "sich"
                                           | otherwise → "zich"
                        _                  → "zehe"
            )
          , (11, const "elf")
          , (12, const "zwelf")
          , (100, const "hunnert")
          , (1000, const "dausend")
          ]

