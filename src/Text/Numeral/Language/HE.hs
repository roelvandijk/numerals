{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        he

[@ISO639-2@]        heb

[@ISO639-3@]        heb

[@Native name@]     עִבְרִית

[@English name@]    Modern Hebrew
-}

module Text.Numeral.Language.HE
    ( -- * Conversions
      cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( ($), fix )
import "base" Data.List     ( concat )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (+), div, negate )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Monoid.Unicode   ( (⊕) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.Exp as E
import           "numerals-base" Text.Numeral.Grammar ( Inflection )


--------------------------------------------------------------------------------
-- HE
--------------------------------------------------------------------------------

{-
Sources:
  E-mail by Yitzchak Gale.
  http://www.hebrew4christians.com/Grammar/Unit_Eight/Cardinal_Numbers/cardinal_numbers.html
-}

cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ ( Integral α
         , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Dual β, E.Plural β
         )
       ⇒ α → β
struct = pos
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

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = 1000 in (negate x, x)

-- TODO: Handle inflection, mainly masculine and feminine
-- gender. Maybe get rid of Dual and Plural in the expression language
-- and use grammatical number for that.
cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
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
