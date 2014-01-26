{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        nl

[@ISO639-2B@]       dut

[@ISO639-3@]        nld

[@Native name@]     Nederlands

[@English name@]    Dutch
-}

module Text.Numeral.Language.NLD
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
    , ordinal
    , partitive
    , multiplicative
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Monad ( return )
import "base" Data.Bool     ( Bool, otherwise )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Functor  ( fmap )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Ord      ( (<) )
import "base" Prelude       ( Integral, (-), negate )
import "base-unicode-symbols" Data.Bool.Unicode     ( (∧), (∨) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.List.Unicode     ( (∈) )
import "base-unicode-symbols" Data.Monoid.Unicode   ( (⊕) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum  as BN
import qualified "this" Text.Numeral.Exp     as E
import qualified "this" Text.Numeral.Grammar as G
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- NLD
--------------------------------------------------------------------------------

entry ∷ Entry
entry = emptyEntry
    { entIso639_1       = Just "nl"
    , entIso639_2       = ["dut"]
    , entIso639_3       = Just "nld"
    , entNativeNames    = ["Nederlands"]
    , entEnglishName    = Just "Dutch"
    , entCardinal       = Just Conversion
                          { toNumeral   = cardinal
                          , toStructure = struct
                          }
    , entOrdinal        = Just Conversion
                          { toNumeral   = ordinal
                          , toStructure = struct
                          }
    , entPartitive      = Just Conversion
                          { toNumeral   = partitive
                          , toStructure = \(n, d) → E.frac (struct n) (struct d)
                          }
    , entMultiplicative = Just Conversion
                          { toNumeral   = multiplicative
                          , toStructure = struct
                          }
    }

cardinal ∷ (G.Plural i, G.Dative i, Integral α, E.Scale α) ⇒ i → α → Maybe Text
cardinal inf = cardinalRepr inf ∘ struct

ordinal ∷ (Integral α, E.Scale α) ⇒ i → α → Maybe Text
ordinal inf = ordinalRepr "eerste" inf ∘ struct

partitive ∷ ( G.Singular i, G.Plural i, G.NoCase i, G.Dative i
            , Integral α, E.Scale α
            )
          ⇒ i → (α, α) → Maybe Text
partitive inf (n, d) = do
  n' ← cardinal (G.noCase $ G.singular inf) n
  d' ← ordinalRepr "éénde" inf $ struct d
  return $ n' ⊕ " " ⊕ d'

multiplicative ∷ ( G.Singular i, G.Plural i, G.NoCase i, G.Dative i
                 , Integral α, E.Scale α
                 )
               ⇒ i → α → Maybe Text
multiplicative inf = fmap (⊕ "maal") ∘ cardinal (G.noCase $ G.singular inf)

struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β, E.Scale β
         )
       ⇒ α → β
struct = pos
       $ fix
       $ findRule (   0, lit               )
                [ (  13, add    10      L  )
                , (  20, mul    10      L L)
                , ( 100, step  100   10 R L)
                , (1000, step 1000 1000 R L)
                ]
                  (dec 6 - 1)
         `combine` pelletierScale R L BN.rule

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 60000 - 1 in (negate x, x)

genericRepr ∷ Repr i
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

cardinalRepr ∷ ∀ i. (G.Plural i, G.Dative i) ⇒ i → Exp i → Maybe Text
cardinalRepr = render genericRepr
               { reprValue = \inf n → M.lookup n (syms inf)
               , reprScale = BN.pelletierRepr (\i c → if doPlural i c then "iljoenen" else "iljoen")
                                              (\i c → if doPlural i c then "iljarden" else "iljard")
                                              []
               }
    where
      doPlural ∷ i → Ctx (Exp i) → Bool
      doPlural inf ctx = (G.isPlural inf ∨ G.isDative inf) ∧ isOutside R ctx

      syms inf =
          M.fromList
          [ (0, const "nul")
          , (1, pluralDative "één" "éénen" "éénen")
          , (2, forms "twee" "twin" "tweeën" "tweeën")
          , (3, forms "drie" "der"  "drieën" "drieën")
          , (4, forms "vier" "veer" "vieren" "vieren")
          , (5, pluralDative "vijf"  "vijven" "vijven")
          , (6, pluralDative "zes"   "zessen" "zessen")
          , (7, pluralDative "zeven" "zevens" "zevenen")
          , (8, \c → case c of
                       CtxMul _ (Lit 10) _ → "tach"
                       CtxAdd _ (Lit 10) _ → "ach"
                       _ | dativeForm c
                         ∨ pluralForm c    → "achten"
                         | otherwise       → "acht"
            )
          , (9, pluralDative "negen" "negens" "negenen")
          , (10, \c → case c of
                        CtxAdd R _ _
                          | dativeForm c → "tienen"
                          | pluralForm c → "tiens"
                        CtxMul R _ _
                          | dativeForm c → "tigen"
                          | pluralForm c → "tigs"
                          | otherwise    → "tig"
                        _ | dativeForm c
                          ∨ pluralForm c → "tienen"
                          | otherwise    → "tien"
            )
          , (  11, pluralDative "elf"     "elven"     "elven")
          , (  12, pluralDative "twaalf"  "twaalven"  "twaalven")
          , ( 100, pluralDative "honderd" "honderden" "honderden")
          , (1000, pluralDative "duizend" "duizenden" "duizenden")
          ]
          where
            pluralDative ∷ s → s → s → Ctx (Exp i) → s
            pluralDative  o p d ctx
                | pluralForm ctx = p
                | dativeForm ctx = d
                | otherwise      = o

            dativeForm ∷ Ctx (Exp i) → Bool
            dativeForm ctx = G.isDative inf ∧ isOutside R ctx

            pluralForm ∷ Ctx (Exp i) → Bool
            pluralForm ctx = G.isPlural inf ∧ isOutside R ctx

            forms ∷ s -- ^ Normal form.
                  → s -- ^ Added to, or multiplied with ten.
                  → s -- ^ Plural form.
                  → s -- ^ Dative form.
                  → Ctx (Exp i)
                  → s
            forms n t p d ctx =
                case ctx of
                  CtxMul _ (Lit 10) _ → t
                  CtxAdd _ (Lit 10) _ → t
                  _ | dativeForm ctx  → d
                    | pluralForm ctx  → p
                    | otherwise       → n

ordinalRepr ∷ Text → i → Exp i → Maybe Text
ordinalRepr one =
    render genericRepr
           { reprValue = \_ n → M.lookup n syms
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
                       CtxEmpty          → one
                       _ | isOutside R c → "éénde"
                         | otherwise     → "één"
            )
          , (2, forms "tweede"  "twee"  "twin")
          , (3, forms "derde"   "drie"  "der")
          , (4, forms "vierde"  "vier"  "veer")
          , (5, forms "vijfde"  "vijf"  "vijf")
          , (6, forms "zesde"   "zes"   "zes")
          , (7, forms "zevende" "zeven" "zeven")
          , (8, \c → case c of
                       _ | isOutside R c → "achtste"
                       CtxMul _ (Lit 10) _ → "tach"
                       CtxAdd _ (Lit _)  _ → "ach"
                       _                   → "acht"
            )
          , (9, forms "negende" "negen" "negen")
          , (10, \c → case c of
                        CtxMul R _ _ | isOutside R c → "tigste"
                                     | otherwise     → "tig"
                        _            | isOutside R c → "tiende"
                                     | otherwise     → "tien"
            )
          , (11, \c → if isOutside R c then "elfde"    else "elf")
          , (12, \c → if isOutside R c then "twaalfde" else "twaalf")
          , (100,  \c → if isOutside R c then "honderdste" else "honderd")
          , (1000, \c → if isOutside R c then "duizendste" else "duizend")
          ]

      forms ∷ s -- ^ Ordinal form.
            → s -- ^ Cardinal form.
            → s -- ^ Added to, or multiplied with, ten.
            → Ctx (Exp i)
            → s
      forms o c t ctx = case ctx of
                          _ | isOutside R ctx → o
                          CtxMul _ (Lit 10) _ → t
                          CtxAdd _ (Lit _)  _ → t
                          _                   → c
