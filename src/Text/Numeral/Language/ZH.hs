{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        zh

[@ISO639-2B@]       chi

[@ISO639-2T@]       zho

[@ISO639-3@]        cmn

[@Native name@]     官話

[@English name@]    Chinese
-}

module Text.Numeral.Language.ZH
    ( -- * Conversions
      trad_cardinal
    , simpl_cardinal
    , finance_trad_cardinal
    , finance_simpl_cardinal
    , pinyin_cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( id, const, fix, flip, ($) )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.Ord      ( (<) )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Num, Integral, fromIntegral, (-), div, divMod, negate )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Monoid.Unicode   ( (⊕) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≥) )
import qualified "containers" Data.Map as M ( Map, fromList, lookup )
import "containers-unicode-symbols" Data.Map.Unicode ( (∪) )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.Exp as E
import           "numerals-base" Text.Numeral.Grammar ( Inflection )
import           "numerals-base" Text.Numeral.Misc ( dec )


--------------------------------------------------------------------------------
-- ZH
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/mandarin.html
-}

flipIfR ∷ Side → (α → α → α) → (α → α → α)
flipIfR L = id
flipIfR R = flip

add0 ∷ (Integral α, E.Lit β, E.Add β) ⇒ α → Rule α β
add0 val f n | n < val `div` 10 = E.lit 0 `E.add` f n
             | otherwise        = f n

mulX ∷ (Integral α, E.Lit β, E.Add β, E.Mul β)
     ⇒ α → Side → Side → Rule α β
mulX val aSide mSide =
    \f n → let (m, a) = n `divMod` val
               mval = if m ≡ 1
                      then E.lit 1 ⊡ E.lit (fromIntegral val)
                      else f m ⊡ E.lit (fromIntegral val)
           in if a ≡ 0
              then mval
              else (flipIfR aSide E.add) (add0 val f a) mval
  where
     (⊡) = flipIfR mSide E.mul

struct ∷ (Integral α, E.Unknown β, E.Lit β, E.Neg β, E.Add β, E.Mul β) ⇒ α → β
struct = pos
       $ fix
       $ findRule (0, lit)
                  ( [(dec 1, step  (dec 1) (dec 1) R L)]
                  ⊕ [(dec n, stepX (dec n) (dec 1) R L) | n ← [2,3]]
                  ⊕ [(dec n, stepX (dec n) (dec 4) R L) | n ← [4,8..44]]
                  )
                  (dec 48 - 1)
    where
      stepX = mkStep lit1 addX mulX

      addX val _ = \f n → E.add (f val) (add0 val f $ n - val)

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 48 - 1 in (negate x, x)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr i s
cardinalRepr = defaultRepr
               { reprAdd = Just $ \_ _ _ → ""
               , reprMul = Just $ \_ _ _ → ""
               }


--------------------------------------------------------------------------------
-- Traditional Characters
--------------------------------------------------------------------------------

trad_cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s)
              ⇒ i → α → Maybe s
trad_cardinal inf = trad_cardinalRepr inf ∘ struct

trad_cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
trad_cardinalRepr =
    render cardinalRepr
           { reprValue = \_ n → M.lookup n trad_syms
           , reprNeg = Just $ \_ _ → "負"
           }

trad_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx (Exp i) → s)
trad_syms =
    M.fromList
    [ (0, \c → case c of
                 CtxEmpty → "零"
                 _        → "〇"
      )
    , (1, const "一")
    , (2, \c → case c of
                 CtxMul _ (Lit n) _ | n ≥ 1000 → "兩"
                 _ → "二"
      )
    , (3, const "三")
    , (4, const "四")
    , (5, const "五")
    , (6, const "六")
    , (7, const "七")
    , (8, const "八")
    , (9, const "九")
    , (10, const "十")
    , (100, const "百")
    , (1000, const "千")
    , (dec 4, const "萬")
    , (dec 8, const "億")
    , (dec 12, const "兆")
    , (dec 16, const "京")
    , (dec 20, const "垓")
    , (dec 24, const "秭")
    , (dec 28, const "穰")
    , (dec 32, const "溝")
    , (dec 36, const "澗")
    , (dec 40, const "正")
    , (dec 44, const "載")
    ]


--------------------------------------------------------------------------------
-- Simplified Characters
--------------------------------------------------------------------------------

simpl_cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s)
               ⇒ i → α → Maybe s
simpl_cardinal inf = simpl_cardinalRepr inf ∘ struct

simpl_cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
simpl_cardinalRepr =
    render cardinalRepr
           { reprValue = \_ n → M.lookup n (simpl_syms ∪ trad_syms)
           , reprNeg = Just $ \_ _ → "负"
           }

simpl_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx (Exp i) → s)
simpl_syms =
    M.fromList
    [ (2, \c → case c of
                 CtxMul _ (Lit n) _ | n ≥ 1000 → "两"
                 _ → "二"
      )
    , (dec 4, const "万")
    , (dec 8, const "亿")
    ]


--------------------------------------------------------------------------------
-- Financial Characters (Traditional)
--------------------------------------------------------------------------------

finance_trad_cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s)
                      ⇒ i → α → Maybe s
finance_trad_cardinal inf = finance_trad_cardinalRepr inf ∘ struct

finance_trad_cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
finance_trad_cardinalRepr =
    render cardinalRepr
           { reprValue = \_ n → M.lookup n (finance_trad_syms ∪ trad_syms)
           , reprNeg = Just $ \_ _ → "負"
           }

finance_trad_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx (Exp i) → s)
finance_trad_syms =
    M.fromList
    [ (0, const "零")
    , (1, const "壹")
    , (2, const "貳")
    , (3, const "参")
    , (4, const "肆")
    , (5, const "伍")
    , (6, const "陸")
    , (7, const "柒")
    , (8, const "捌")
    , (9, const "玖")
    , (10, const "拾")
    , (100, const "伯")
    , (1000, const "仟")
    , (dec 4, const "萬")
    , (dec 8, const "億")
    ]


--------------------------------------------------------------------------------
-- Financial Characters (Simplified)
--------------------------------------------------------------------------------

finance_simpl_cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s)
                       ⇒ i → α → Maybe s
finance_simpl_cardinal inf = finance_simpl_cardinalRepr inf ∘ struct

finance_simpl_cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
finance_simpl_cardinalRepr =
    render cardinalRepr
           { reprValue = \_ n → M.lookup n ( finance_simpl_syms
                                           ∪ finance_trad_syms
                                           ∪ trad_syms
                                           )
           , reprNeg = Just $ \_ _ → "负"
           }
  where
    finance_simpl_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx (Exp i) → s)
    finance_simpl_syms =
        M.fromList
        [ (2, const "贰")
        , (6, const "陆")
        , (dec 4, const "万")
        , (dec 8, const "亿")
        ]


--------------------------------------------------------------------------------
-- Pinyin
--------------------------------------------------------------------------------

pinyin_cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s)
                ⇒ i → α → Maybe s
pinyin_cardinal inf = pinyin_cardinalRepr inf ∘ struct

pinyin_cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
pinyin_cardinalRepr =
    render cardinalRepr
           { reprValue = \_ n → M.lookup n pinyin_syms
           , reprNeg = Just $ \_ _ → "fù"
           , reprAdd = Just (⊞)
           }
  where
    (Lit 10 ⊞ _) _ = ""
    (_      ⊞ _) _ = " "

    pinyin_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx (Exp i) → s)
    pinyin_syms =
        M.fromList
        [ (0, const "líng")
        , (1, const "yī")
        , (2, \c → case c of
                     CtxMul _ (Lit n) _ | n ≥ 1000 → "liǎng"
                     _ → "èr"
          )
        , (3, const "sān")
        , (4, const "sì")
        , (5, const "wǔ")
        , (6, const "liù")
        , (7, const "qī")
        , (8, const "bā")
        , (9, const "jiǔ")
        , (10, const "shí")
        , (100, const "bǎi")
        , (1000, const "qiān")
        , (dec 4, const "wàn")
        , (dec 8, const "yì")
        , (dec 12, const "zhào")
        , (dec 16, const "jīng")
        , (dec 20, const "gāi")
        , (dec 24, const "zǐ")
        , (dec 28, const "ráng")
        , (dec 32, const "gōu")
        , (dec 36, const "jiàn")
        , (dec 40, const "zhēng")
        , (dec 44, const "zài")
        ]
