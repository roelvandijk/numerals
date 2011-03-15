{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        ja

[@ISO639-2B@]       jpn

[@ISO639-3@]        jpn

[@Native name@]     日本語

[@English name@]    Japanese
-}

module Text.Numeral.Language.JA
    ( struct

    , kanji_cardinal
    , kanji_cardinal_repr

    , daiji_cardinal
    , daiji_cardinal_repr

    , on'yomi_cardinal
    , on'yomi_cardinal_repr

    , preferred_cardinal
    , preferred_cardinal_repr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( ($), const, fix )
import Data.List     ( take )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Num, Integral, (-) )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )
import Prelude.Unicode     ( (⋅) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc  ( dec )
import qualified Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- JA
--------------------------------------------------------------------------------

{-
Sources:
  http://en.wikipedia.org/wiki/Japanese_numerals
  http://www.guidetojapanese.org/numbers.html
-}

struct ∷ (Integral α, C.Lit β, C.Neg β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = pos
       $ fix
       $ findRule (   0, lit         )
              ( [ (  11, add   10 R  )
                , (  20, mul   10 R L)
                , ( 100, lit         )
                , ( 101, add  100 R  )
                , ( 200, mul  100 R L)
                , (1000, lit         )
                , (1001, add 1000 R  )
                , (2000, mul 1000 R L)
                ]
              ⊕ take (3 ⋅ 17) (scale1Rules 4 R L)
              )
             (dec 72 - 1)


--------------------------------------------------------------------------------
-- Kanji
--------------------------------------------------------------------------------

kanji_cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
kanji_cardinal = struct >=> kanji_cardinal_repr

kanji_cardinal_repr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
kanji_cardinal_repr = textify defaultRepr
                      { reprValue = \n → M.lookup n symMap
                      , reprAdd   = \_ _ → Just ""
                      , reprMul   = \_ _ → Just ""
                      , reprNeg   = \_   → Just "マイナス"
                      }
    where
      symMap = M.fromList
               [ (0, const "零") -- alternatives:"ゼロ" or "マル"
               , (1, const "一")
               , (2, const "二")
               , (3, const "三")
               , (4, const "四")
               , (5, const "五")
               , (6, const "六")
               , (7, const "七")
               , (8, const "八")
               , (9, const "九")
               , (10, const "十")
               , (100, const "百")
               , (dec 3, const "千")
               , (dec 4, const "万")
               , (dec 8, const "億")
               , (dec 12, const "兆")
               , (dec 16, const "京")
               , (dec 20, const "垓")
               , (dec 24, const "𥝱") -- or 秭?
               , (dec 28, const "穣")
               , (dec 32, const "溝")
               , (dec 36, const "澗")
               , (dec 40, const "正")
               , (dec 44, const "載")
               , (dec 48, const "極")
               , (dec 52, const "恒河沙")
               , (dec 56, const "阿僧祇")
               , (dec 60, const "那由他/那由多")
               , (dec 64, const "不可思議")
               , (dec 68, const "無量大数")
              ]


--------------------------------------------------------------------------------
-- Daiji
--------------------------------------------------------------------------------

daiji_cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
daiji_cardinal = struct >=> daiji_cardinal_repr

daiji_cardinal_repr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
daiji_cardinal_repr = textify defaultRepr
                      { reprValue = \n → M.lookup n symMap
                      , reprAdd   = \_ _ → Just ""
                      , reprMul   = \_ _ → Just ""
                      , reprNeg   = \_   → Just "マイナス"
                      }
    where
      symMap = M.fromList
               [ (0, const "零") -- alternatives:"ゼロ" or "マル"
               , (1, const "壱")
               , (2, const "弐")
               , (3, const "参")
               , (4, const "四")
               , (5, const "五")
               , (6, const "六")
               , (7, const "七")
               , (8, const "八")
               , (9, const "九")
               , (10, const "拾")
               , (100, const "百")
               , (dec 3, const "千")
               , (dec 4, const "万")
               ]


--------------------------------------------------------------------------------
-- Generic reading
--------------------------------------------------------------------------------

generic_repr ∷ (Monoid s, IsString s) ⇒ s → s → Repr s
generic_repr four seven = defaultRepr
                          { reprValue = \n → M.lookup n symMap
                          , reprAdd   = \_ _ → Just " "
                          , reprMul   = \_ _ → Just ""
                          , reprNeg   = \_   → Just "mainasu "
                          }
    where
      symMap = M.fromList
               [ (0, const "rei")
               , (1, const "ichi")
               , (2, const "ni")
               , (3, const "san")
               , (4, const four)
               , (5, const "go")
               , (6, const "roku")
               , (7, const seven)
               , (8, const "hachi")
               , (9, const "kyū")
               , (10, const "jū")
               , (100, \c → case c of
                              (CtxMulR (Lit 3) _) → "byaku" -- rendaku
                              _                   → "hyaku"
                 )
               , (dec 3, const "sen")
               , (dec 4, const "man")
               , (dec 8, const "oku")
               , (dec 12, const "chō")
               , (dec 16, const "kei")
               , (dec 20, const "gai")
               , (dec 24, const "jo")
               , (dec 28, const "jō")
               , (dec 32, const "kō")
               , (dec 36, const "kan")
               , (dec 40, const "sei")
               , (dec 44, const "sai")
               , (dec 48, const "goku")
               , (dec 52, const "gōgasha")
               , (dec 56, const "asōgi")
               , (dec 60, const "nayuta")
               , (dec 64, const "fukashigi")
               , (dec 68, const "muryōtaisū")
               ]


--------------------------------------------------------------------------------
-- On'yomi
--------------------------------------------------------------------------------

on'yomi_cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
on'yomi_cardinal = struct >=> on'yomi_cardinal_repr

on'yomi_cardinal_repr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
on'yomi_cardinal_repr = textify $ generic_repr "shi" "shichi"


--------------------------------------------------------------------------------
-- Preferred reading
--------------------------------------------------------------------------------

preferred_cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
preferred_cardinal = struct >=> preferred_cardinal_repr

preferred_cardinal_repr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
preferred_cardinal_repr = textify $ generic_repr "yon" "nana"
