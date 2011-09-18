{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        he

[@ISO639-B@]        heb

[@ISO639-3@]        heb

[@Native name@]     עִבְרִית

[@English name@]    Modern Hebrew
-}

module Text.Numeral.Language.HE
    ( fem_cardinal
    -- , masc_cardinal
    , struct
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( ($), fix )
import "base" Data.List     ( concat )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (+), div )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Monoid.Unicode   ( (⊕) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- HE
--------------------------------------------------------------------------------

{-
Sources:
  E-mail by Yitzchak Gale.
  http://www.hebrew4christians.com/Grammar/Unit_Eight/Cardinal_Numbers/cardinal_numbers.html
-}

fem_cardinal ∷ (Integral α, Monoid s, IsString s) ⇒ α → Maybe s
fem_cardinal = fem_cardinalRepr ∘ struct

-- masc_cardinal ∷ (Integral α, Monoid s, IsString s) ⇒ α → Maybe s
-- masc_cardinal = masc_cardinalRepr ∘ struct

struct ∷ ( Integral α
         , C.Unknown β, C.Lit β, C.Add β, C.Mul β, C.Dual β, C.Plural β
         )
       ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (  0, lit)
                  ( [ (11, add 10 L)
                    , (20, dual ∘ mapRule (`div` 10) lit)
                    , (21, add 20 R)
                    ]
                  ⊕ concat [ [ (n, plural ∘ mapRule (`div` 10) lit)
                             , (n+1, add n R)
                             ]
                           | n ← [20,30..90]
                           ]
                  ⊕ [ (100, step 100 10 R L)
                    , (200, dual ∘ mapRule (`div` 2) lit)
                    , (201, add 200 R)
                    , (300, step 100 10 R L)
                    ]
                  )
                  1000

fem_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
fem_cardinalRepr = render defaultRepr
                   { reprValue = \n → M.lookup n syms
                   , reprAdd   = Just (⊞)
                   , reprMul   = Just (⊡)
                   , reprNeg   = Just $ \_ _ → "מינוס "
                   , reprAddCombine = Just addCombine
                   }
    where
      (_        ⊞ Lit 10) _ = "ה"
      (Dual   _ ⊞ _     ) _ = " ו"
      (Plural _ ⊞ _     ) _ = " ו"
      (_        ⊞ _     ) _ = " "

      (_ ⊡ Lit 10) _ = ""
      (_ ⊡ _     ) _ = " "

      addCombine a x _ y (Lit 10) = x ⊕ " " ⊕ y ⊕ a
      addCombine a x _ y _        = x ⊕ a ⊕ y

      syms =
          M.fromList
          [ (0, \c → case c of
                       _ → "אפס"
            )
          , (1, \c → case c of
                       _ → "אחת"
            )
          , (2, \c → case c of
                       CtxAdd _ (Lit 10) _ → "שתים"
                       CtxDual {}          → "עשרים"
                       _                   → "שתיים"
            )
          , (3, \c → case c of
                       CtxPlural {} → "שלושים"
                       _            → "שלוש"
            )
          , (4, \c → case c of
                       CtxPlural {} → "ארבעים"
                       _            → "ארבע"
            )
          , (5, \c → case c of
                       CtxPlural {} → "חמישים"
                       _            → "חמש"
            )
          , (6, \c → case c of
                       CtxPlural {} → "ששים"
                       _            → "שש"
            )
          , (7, \c → case c of
                       CtxPlural {} → "שבעים"
                       _            → "שבע"
            )
          , (8, \c → case c of
                       CtxPlural {} → "שמונים"
                       _            → "שמונה"
            )
          , (9, \c → case c of
                       CtxPlural {} → "תשעים"
                       _            → "תשע"
            )
          , (10, \c → case c of
                        CtxMul _ (Lit n) _ | n ≤ 9 → ""
                        _ → "עשר"
            )
          , (100, \c → case c of
                         CtxDual   {} → "מאתיים"
                         CtxMul    {} → "מאות"
                         _            → "מאה"
            )
          , (1000, \c → case c of
                          _ → "אלף"
            )
          ]
