{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        ru

[@ISO639-2@]        rus

[@ISO639-3@]        rus

[@Native name@]     Русский язык

[@English name@]    Russian
-}

module Text.Numeral.Language.RU
    ( cardinal
    , struct
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.Ord      ( (<), (>) )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral, (-) )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤), (≥) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import           "numerals-base" Text.Numeral.Misc ( dec )
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- RU
--------------------------------------------------------------------------------

{-
Sources:
  http://en.wikibooks.org/wiki/Russian/Numbers
  http://russian.speak7.com/russian_numbers.htm
  http://learningrussian.net/games_verbs_grammar3.php
  http://www.waytorussia.net/WhatIsRussia/Russian/Part1a.html
-}

cardinal ∷ (Integral α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ (Integral α, C.Unknown β, C.Lit β, C.Neg β, C.Add β, C.Mul β) ⇒ α → β
struct = pos 
       $ fix 
       $ findRule (   0, lit               )
                [ (  11, add 10 L          )
                , (  20, mul 10 R L        )
                , (  40, lit               )
                , (  41, add 40 R          )
                , (  50, mul 10 R L        )
                , ( 100, step  100   10 R L)
                , (1000, step 1000 1000 R L)
                ]
                  (dec 6 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _ → "минус "
               }
    where
      (Lit n ⊞ Lit 10) _ | n < 10 = "на"
      (_     ⊞ _     ) _          = " "

      (_ ⊡ Lit n) _ | n ≤ 100 = ""
      (_ ⊡ _    ) _           = " "

      syms =
          M.fromList
          [ (0, const "ноль")
          , (1, \c → case c of
                       CtxAdd _ (Lit 40)    (CtxMul {}) → "одна"
                       CtxAdd _ (_ `Mul` _) (CtxMul {}) → "одна"
                       _                                → "один"
            )
          , (2, \c → case c of
                       CtxAdd _ (Lit 10)    _           → "две"
                       CtxAdd _ (Lit 40)    (CtxMul {}) → "две"
                       CtxAdd _ (_ `Mul` _) (CtxMul {}) → "две"
                       CtxMul _ (Lit n)     _ | n > 10  → "две"
                       _                                → "два"
            )
          , (3, const "три")
          , (4, \c → case c of
                       CtxAdd _ (Lit 10) _ → "четыр"
                       _                   → "четыре"
            )
          , (5, \c → case c of
                       CtxAdd _ (Lit 10) _ → "пят"
                       _                   → "пять"
            )
          , (6, \c → case c of
                       CtxAdd _ (Lit 10) _ → "шест"
                       _                   → "шесть"
            )
          , (7, \c → case c of
                       CtxAdd _ (Lit 10) _ → "сем"
                       _                   → "семь"
            )
          , (8, \c → case c of
                       CtxAdd _ (Lit 10) _ → "восем"
                       _                   → "восемь"
            )
          , (9, \c → case c of
                       CtxAdd _ (Lit 10) _ → "девят"
                       CtxMul _ (Lit 10) _ → "девя"
                       _                   → "девять"
            )
          , (10, \c → case c of
                        CtxAdd _ (Lit n) _ | n ≤ 9 → "дцать"
                        CtxMul _ (Lit n) _ | n < 4 → "дцать"
                                           | n < 9 → "десят"
                                           | n ≡ 9 → "носто"
                        _                          → "десять"
            )
          , (40, const "сорок")
          , (100, \c → case c of
                         CtxMul _ (Lit n) _ | n ≡ 2 → "сти"
                                            | n ≤ 4 → "ста"
                                            | n ≤ 9 → "сот"
                         _                          → "сто"
            )
          , (1000, \c → case c of
                          CtxMul _ (Lit 40    `Add` Lit n) _ | n ≡ 1 → "тысяча"
                                                             | n ≤ 4 → "тысячи"
                                                             | n ≥ 5 → "тысяч"
                          CtxMul _ (_ `Mul` _ `Add` Lit n) _ | n ≡ 1 → "тысяча"
                                                             | n ≤ 4 → "тысячи"
                                                             | n ≥ 5 → "тысяч"
                          CtxMul _ (Lit n) _ | n ≤ 4 → "тысячи"
                          CtxMul {}                  → "тысяч"
                          _                          → "тысяча"
            )
          , (dec 6, const "миллион")
          , (dec 9, const "миллиард")
          ]
