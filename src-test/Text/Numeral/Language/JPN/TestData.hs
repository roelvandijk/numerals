{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        ja

[@ISO639-2B@]       jpn

[@ISO639-3@]        jpn

[@Native name@]     日本語

[@English name@]    Japanese
-}

module Text.Numeral.Language.JPN.TestData
    ( preferred_cardinals
    , kanji_cardinals
    , daiji_cardinals
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude     ( Num, Integral )
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- JPN
--------------------------------------------------------------------------------

{-
Sources:
  http://en.wikipedia.org/wiki/Japanese_numerals
  http://www.guidetojapanese.org/numbers.html
-}

preferred_cardinals ∷ (Num i) ⇒ TestData i
preferred_cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "rei")
      , (1, "ichi")
      , (2, "ni")
      , (3, "san")
      , (4, "yon")
      , (5, "go")
      , (6, "roku")
      , (7, "nana")
      , (8, "hachi")
      , (9, "kyū")
      , (10, "jū")
      , (11, "jū ichi")
      , (12, "jū ni")
      , (13, "jū san")
      , (14, "jū yon")
      , (15, "jū go")
      , (16, "jū roku")
      , (17, "jū nana")
      , (18, "jū hachi")
      , (19, "jū kyū")
      , (20, "nijū")
      , (21, "nijū ichi")
      , (22, "nijū ni")
      , (23, "nijū san")
      , (24, "nijū yon")
      , (25, "nijū go")
      , (26, "nijū roku")
      , (27, "nijū nana")
      , (28, "nijū hachi")
      , (29, "nijū kyū")
      , (30, "sanjū")
      , (31, "sanjū ichi")
      , (32, "sanjū ni")
      , (33, "sanjū san")
      , (34, "sanjū yon")
      , (35, "sanjū go")
      , (36, "sanjū roku")
      , (37, "sanjū nana")
      , (38, "sanjū hachi")
      , (39, "sanjū kyū")
      , (40, "yonjū")
      , (41, "yonjū ichi")
      , (42, "yonjū ni")
      , (43, "yonjū san")
      , (44, "yonjū yon")
      , (45, "yonjū go")
      , (46, "yonjū roku")
      , (47, "yonjū nana")
      , (48, "yonjū hachi")
      , (49, "yonjū kyū")
      , (50, "gojū")
      , (51, "gojū ichi")
      , (52, "gojū ni")
      , (53, "gojū san")
      , (54, "gojū yon")
      , (55, "gojū go")
      , (56, "gojū roku")
      , (57, "gojū nana")
      , (58, "gojū hachi")
      , (59, "gojū kyū")
      , (60, "rokujū")
      , (61, "rokujū ichi")
      , (62, "rokujū ni")
      , (63, "rokujū san")
      , (64, "rokujū yon")
      , (65, "rokujū go")
      , (66, "rokujū roku")
      , (67, "rokujū nana")
      , (68, "rokujū hachi")
      , (69, "rokujū kyū")
      , (70, "nanajū")
      , (71, "nanajū ichi")
      , (72, "nanajū ni")
      , (73, "nanajū san")
      , (74, "nanajū yon")
      , (75, "nanajū go")
      , (76, "nanajū roku")
      , (77, "nanajū nana")
      , (78, "nanajū hachi")
      , (79, "nanajū kyū")
      , (80, "hachijū")
      , (81, "hachijū ichi")
      , (82, "hachijū ni")
      , (83, "hachijū san")
      , (84, "hachijū yon")
      , (85, "hachijū go")
      , (86, "hachijū roku")
      , (87, "hachijū nana")
      , (88, "hachijū hachi")
      , (89, "hachijū kyū")
      , (90, "kyūjū")
      , (91, "kyūjū ichi")
      , (92, "kyūjū ni")
      , (93, "kyūjū san")
      , (94, "kyūjū yon")
      , (95, "kyūjū go")
      , (96, "kyūjū roku")
      , (97, "kyūjū nana")
      , (98, "kyūjū hachi")
      , (99, "kyūjū kyū")
      , (100, "hyaku")
      , (208, "nihyaku hachi")
      , (280, "nihyaku hachijū")
      , (2008, "nisen hachi")
      , (2080, "nisen hachijū")
      , (2800, "nisen hachihyaku")
      ]
    )
  ]

kanji_cardinals ∷ (Integral i) ⇒ TestData i
kanji_cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "零")
      , (1, "一")
      , (2, "二")
      , (3, "三")
      , (4, "四")
      , (5, "五")
      , (6, "六")
      , (7, "七")
      , (8, "八")
      , (9, "九")
      , (10, "十")
      , (11, "十一")
      , (12, "十二")
      , (13, "十三")
      , (14, "十四")
      , (21, "二十一")
      , (28, "二十八")
      , (100, "百")
      , (208, "二百八")
      , (280, "二百八十")
      , (321, "三百二十一")
      , (1000, "千")
      , (2008, "二千八")
      , (2080, "二千八十")
      , (2800, "二千八百")
      , (4321, "四千三百二十一")
      , (10000, "一万")
      , (654321, "六十五万四千三百二十一")
      , (87654321, "八千七百六十五万四千三百二十一")
      , (100000000, "一億")
      , (987654321, "九億八千七百六十五万四千三百二十一")
      , (dec 12, "一兆")
      ]
    )
  ]

daiji_cardinals ∷ (Integral i) ⇒ TestData i
daiji_cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "零")
      , (1, "壱")
      , (2, "弐")
      , (3, "参")
      , (4, "四")
      , (5, "五")
      , (6, "六")
      , (7, "七")
      , (8, "八")
      , (9, "九")
      , (10, "拾")
      ]
    )
  ]
