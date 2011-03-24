{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        zh

[@ISO639-2B@]       chi

[@ISO639-2T@]       zho

[@ISO639-3@]        cmn

[@Native name@]     官話

[@English name@]    Chinese
-}

module Text.Numeral.Language.ZH
    ( struct
    , trad_cardinal
    , simpl_cardinal
    , finance_trad_cardinal
    , finance_simpl_cardinal
    , pinyin_cardinal
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/mandarin.html
-}

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, (-) )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )
import Data.Ord.Unicode    ( (≥) )

-- from containers:
import qualified Data.Map as M ( Map, fromList, lookup )

-- from containers-unicode-symbols:
import Data.Map.Unicode ( (∪) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- ZH
--------------------------------------------------------------------------------

struct ∷ (Integral α, C.Lit β, C.Neg β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = pos
       $ fix
       $ findRule (   0, lit              )
               ([ (  11, add     10    R  )
                , (  20, mul     10    R L)
                , ( 100, step1  100 10 R L)
                , (1000, step1 1000 10 R L)
                ]
                ⊕ [(dec n, step1 (dec n) (dec 4) R L) | n ← [4,8..44]]
               )
                  (dec 48 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
cardinalRepr = defaultRepr
               { reprAdd = \_ _ → Just ""
               , reprMul = \_ _ → Just ""
               }


--------------------------------------------------------------------------------
-- Traditional Characters
--------------------------------------------------------------------------------

trad_cardinal ∷ (Monoid s, IsString s, Integral α)
                     ⇒ α → Maybe s
trad_cardinal = struct >=> trad_cardinalRepr

trad_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
trad_cardinalRepr =
    textify cardinalRepr
    { reprValue = \n → M.lookup n trad_syms
    , reprNeg = \_ → Just "負"
    }

trad_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx Exp → s)
trad_syms =
    M.fromList
    [ (0, const "零")
    , (1, const "一")
    , (2, \c → case c of
                 CtxMul _ (Lit n) _ | n ≥ 100 → "兩"
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

simpl_cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
simpl_cardinal = struct >=> simpl_cardinalRepr

simpl_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
simpl_cardinalRepr =
    textify cardinalRepr
            { reprValue = \n → M.lookup n (simpl_syms ∪ trad_syms)
            , reprNeg = \_ → Just "负"
            }

simpl_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx Exp → s)
simpl_syms =
    M.fromList
    [ (dec 4, const "万")
    , (dec 8, const "亿")
    ]

--------------------------------------------------------------------------------
-- Financial Characters (Traditional)
--------------------------------------------------------------------------------

finance_trad_cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
finance_trad_cardinal = struct >=> finance_trad_cardinalRepr

finance_trad_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
finance_trad_cardinalRepr =
    textify cardinalRepr
    { reprValue = \n → M.lookup n (finance_trad_syms ∪ trad_syms)
    , reprNeg = \_ → Just "負"
    }
  where
    finance_trad_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx Exp → s)
    finance_trad_syms =
        M.fromList
        [ (2, const "貳")
        , (3, const "叄")
        , (6, const "陸")
        ]


--------------------------------------------------------------------------------
-- Financial Characters (Simplified)
--------------------------------------------------------------------------------

finance_simpl_cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
finance_simpl_cardinal = struct >=> finance_simpl_cardinalRepr

finance_simpl_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
finance_simpl_cardinalRepr =
    textify cardinalRepr
    { reprValue = \n → M.lookup n ( finance_simpl_syms
                                  ∪ simpl_syms
                                  ∪ trad_syms
                                  )
    , reprNeg = \_ → Just "负"
    }
  where
    finance_simpl_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx Exp → s)
    finance_simpl_syms =
        M.fromList
        [ (2, const "贰")
        , (3, const "叁")
        , (6, const "陆")
        ]


--------------------------------------------------------------------------------
-- Pinyin
--------------------------------------------------------------------------------

pinyin_cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
pinyin_cardinal = struct >=> pinyin_cardinalRepr

pinyin_cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
pinyin_cardinalRepr =
    textify cardinalRepr
            { reprValue = \n → M.lookup n pinyin_syms
            , reprNeg = \_ → Just "fù"
            , reprAdd = (⊞)
            }
  where
    Lit 10 ⊞ _ = Just ""
    _      ⊞ _ = Just " "

    pinyin_syms ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx Exp → s)
    pinyin_syms =
        M.fromList
        [ (0, const "líng")
        , (1, const "yī")
        , (2, \c → case c of
                     CtxMul _ (Lit n) _ | n ≥ 100 → "liǎng"
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
