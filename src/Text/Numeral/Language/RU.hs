{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

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

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<), (>) )
import Data.String   ( IsString )
import Prelude       ( Integral, (-) )

-- from base-unicode-symbols:
import Data.Eq.Unicode   ( (≡) )
import Data.List.Unicode ( (∈) )
import Data.Ord.Unicode  ( (≤), (≥) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.Exp.Classes as C


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

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Neg β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = pos $ fix $ rule

rule ∷ (Integral α, C.Lit β, C.Add β, C.Mul β) ⇒ Rule α β
rule = findRule (   0, lit               )
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
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = (⊞)
               , reprMul   = (⊡)
               , reprNeg   = \_   → Just "?"
               }
    where
      Lit n ⊞ Lit 10 | n < 10 = Just "на"
      _     ⊞ _               = Just " "

      _ ⊡ Lit n  | n ≤ 100 = Just ""
      _ ⊡ _                = Just " "

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
