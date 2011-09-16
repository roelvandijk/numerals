{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        tr

[@ISO639-2@]        tur

[@ISO639-3@]        tur

[@Native name@]     Türkçe

[@English name@]    Turkish
-}

module Text.Numeral.Language.TR
    ( cardinal
    , struct
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Bool           ( otherwise )
import "base" Data.Function       ( ($), const, fix )
import "base" Data.Maybe          ( Maybe(Just) )
import "base" Data.Monoid         ( Monoid )
import "base" Data.String         ( IsString )
import "base" Prelude             ( Integral, (-), divMod, Integer )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.List.Unicode     ( (∉) )
import "base-unicode-symbols" Prelude.Unicode       ( (⋅) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.Exp.Classes as C
import qualified "numerals-base" Text.Numeral.BigNum as BN ( rule, scaleRepr, forms )


--------------------------------------------------------------------------------
-- TR
--------------------------------------------------------------------------------

{-

Sources:
  http://www.languagesandnumbers.com/how-to-count-in-turkish/en/tur/
  http://www.sf.airnet.ne.jp/~ts/language/number/turkish.html
  http://www.turkishlanguage.co.uk/seasons.htm#article_15
  http://www.turkeytravelplanner.com/details/LanguageGuide/100words_lessons/100Words_10.html
  http://en.wikibooks.org/wiki/Turkish/Numbers
  http://tr.wikipedia.org/wiki/B%C3%BCy%C3%BCk_say%C4%B1lar%C4%B1n_adlar%C4%B1
-}

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ (Integral α, C.Scale α, C.Unknown β, C.Lit β, C.Add β, C.Mul β, C.Scale β)
       ⇒ α → β
struct = checkPos $ fix $ rule `combine` shortScale1 R L BN.rule

rule ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β) ⇒ Rule α β
rule = findRule (   0, lit               )
              [ (  11, addToTens         )
              , ( 100, step  100   10 R L)
              , (1000, step 1000 1000 R L)
              ]
                (dec 6 - 1)

addToTens ∷ (Integral α, C.Lit β, C.Add β) ⇒ Rule α β
addToTens f n = let (m, r) = n `divMod` 10
                    tens   = m ⋅ 10
                in if r ≡ 0
                   then lit f tens
                   else f tens `C.add` f r

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = scaleRepr
               , reprAdd   = Just (⊞)
               , reprMul   = Just $ \_ _ _ → " "
               }
    where
      (Lit 10 ⊞ _) (CtxMul {}) = ""
      (_      ⊞ _) _           = " "

      syms =
          M.fromList
          [ (0, const "sıfır")
          , (1, const "bir")
          , (2, const "iki")
          , (3, const "üç")
          , (4, const "dört")
          , (5, const "beş")
          , (6, const "altı")
          , (7, const "yedi")
          , (8, const "sekiz")
          , (9, const "dokuz")
          , (10, const "on")
          , (20, const "yirmi")
          , (30, const "otuz")
          , (40, const "kırk")
          , (50, const "elli")
          , (60, const "altmış")
          , (70, const "yetmiş")
          , (80, const "seksen")
          , (90, const "doksan")
          , (100, const "yüz")
          , (1000, const "bin")
          ]

scaleRepr ∷ (IsString s, Monoid s)
              ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
scaleRepr = BN.scaleRepr
              "ilyon" "ilyon"
              [ (1, BN.forms "m"     "an"     "an"     ""       "")
              , (2, BN.forms "b"     "do"     "do"     "vi"     "du")
              , (3, \c → case c of
                           CtxAdd _ (Lit 10)  _ → "tre"
                           CtxAdd _ (Lit 100) _ → "tre"
                           CtxAdd {}            → "tres"
                           CtxMul _ (Lit 100) _ → "tre"
                           CtxMul {}            → "tri"
                           _                    → "tr"
                )
              , (4, BN.forms "katr"  "kator"  "kator"  "katra"  "katrin")
              , (5, BN.forms "kent"  "ken"    "kenka"  "kenka"  "ken")
              , (6, BN.forms "sekst" "seks"   "ses"    "seksa"  "se")
              , (7, BN.forms "sept"  "septen" "septem" "septe"  "septin")
              , (8, BN.forms "okt"   "okto"   "okto"   "okto"   "oktin")
              , (10, \c → case c of
                            CtxAdd _ (Lit 100) _              → "desi"
                            CtxAdd _ (Lit 1) (CtxAdd {})      → "desi"
                            CtxMul _ (Lit n) (CtxAdd L (Lit 100) _)
                                       | n ≡ 2     → "ginti"
                                       | otherwise → "ginta"
                            CtxMul _ (Lit _) (CtxAdd _ _ CtxEmpty) → "gint"
                            CtxMul _ (Lit _) CtxEmpty         → "gint"
                            CtxMul {}                         → "ginti"
                            _                                 → "des"
                )
              , (100, \c → case c of
                             CtxMul _ (Lit n) _ | n ∉ [2,3,6] → "gent"
                             _                                → "sent"
                )
              ]
