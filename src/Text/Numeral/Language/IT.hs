{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        it

[@ISO639-2B@]       ita

[@ISO639-3@]        ita

[@Native name@]     Italiano

[@English name@]    Italian
-}

module Text.Numeral.Language.IT
    ( cardinal
    , struct
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- IT
--------------------------------------------------------------------------------

-- Sources:
--   http://www.sf.airnet.ne.jp/~ts/language/number/italian.html
--   http://www.orbilat.com/Languages/Italian/Grammar/Italian-Numerals.html
--   http://italian.about.com/library/weekly/aa042600a.htm
--   http://www.suite101.com/content/how-to-count-in-italian-a146487

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ ( Integral α, C.Scale α
         , C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → β
struct = pos $ fix $ rule `combine` pelletierScale R L BN.rule
    where
      rule = findRule (   0, lit         )
                    [ (  11, add   10 L  )
                    , (  17, add   10 R  )
                    , (  20, lit         )
                    , (  21, add   20 R  )
                    , (  30, mul   10 R L)
                    , ( 100, step  100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                      (dec 6 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = pelletierRepr
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _ → "meno "
               }
    where
      (Lit 10                ⊞ Lit 7) _ = "as"
      (Lit 10                ⊞ Lit 9) _ = "an"
      ((_ `Mul` Scale _ _ _) ⊞ _    ) _ = " "
      (_                     ⊞ _    ) _ = ""


      (Lit n ⊡ Lit 10) _ | n ≥ 4 = "an"
      (_     ⊡ Scale _ _ _) _ = " "
      (_     ⊡ _          ) _ = ""

      syms =
          M.fromList
          [ (0, const "zero")
          , (1, \c → case c of
                       CtxAdd _ (Lit 10) _ → "un"
                       _                   → "uno"
            )
          , (2, \c → case c of
                       CtxAdd _ (Lit 10) _ → "do"
                       _                   → "due"
            )
          , (3, \c → case c of
                       CtxAdd R _        _ → "tré"
                       CtxMul _ (Lit 10) _ → "tren"
                       _                   → "tre"
            )
          , (4, \c → case c of
                       CtxAdd _ (Lit 10) _ → "quattor"
                       CtxMul _ (Lit 10) _ → "quar"
                       _                   → "quattro"
            )
          , (5, \c → case c of
                       CtxAdd _ (Lit 10) _ → "quin"
                       CtxMul _ (Lit 10) _ → "cinqu"
                       _                   → "cinque"
            )
          , (6, \c → case c of
                       CtxAdd _ (Lit 10) _ → "se"
                       CtxMul _ (Lit 10) _ → "sess"
                       _                   → "sei"
            )
          , (7, \c → case c of
                       CtxMul _ (Lit 10) _ → "sett"
                       _                   → "sette"
            )
          , (8, \c → case c of
                       CtxMul _ (Lit 10) _ → "ott"
                       _                   → "otto"
            )
          , (9, \c → case c of
                       CtxMul _ (Lit 10) _ → "nov"
                       _                   → "nove"
            )
          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ → "dici"
                        -- Last vowel removed because of a
                        -- phonetic rule:
                        CtxMul _ (Lit _) (CtxAdd _ (Lit n) _)
                            | n ∈ [1,8]    → "t"
                        CtxMul R (Lit _) _ → "ta"
                        _                  → "dieci"
            )
          , (20, \c → case c of
                        CtxAdd _ (Lit n) _
                            | n ∈ [1,8]   → "vent"
                        _                 → "venti"
            )
          , ( 100
            , let f c = case c of
                          CtxAdd _ (Lit 8)                      _  → "cent"
                          CtxAdd _ (Lit 8 `Mul` Lit 10)         _  → "cent"
                          CtxAdd _ (Lit 8 `Mul` Lit 10 `Add` _) _  → "cent"
                          CtxMul _ (Lit _) c2 → f c2
                          _ → "cento"
              in f
            )
          , (1000, \c → case c of
                          CtxMul {} → "mila"
                          _         → "mille"
            )
          ]

pelletierRepr ∷ (IsString s, Monoid s)
              ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
pelletierRepr = BN.pelletierRepr
                  (BN.quantityName "ilione"  "ilioni")
                  (BN.quantityName "iliardo" "iliardi")
                  [ (6, BN.forms "sest" "sex"    "ses"    "sexa"   "ses")
                  , (7, BN.forms "sett" "septen" "septem" "septua" "septin")
                  , (8, BN.forms "ott"  "otto"   "otto"   "otto"   "ottin")
                  ]
