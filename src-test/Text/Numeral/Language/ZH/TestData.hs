{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        zh

[@ISO639-2B@]       chi

[@ISO639-2T@]       zho

[@ISO639-3@]        cmn

[@Native name@]     官話

[@English name@]    Chinese
-}

module Text.Numeral.Language.ZH.TestData
    ( trad_cardinals
    , simpl_cardinals
    , finance_trad_cardinals
    , finance_simpl_cardinals
    , pinyin_cardinals
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude     ( Integral )
import "base-unicode-symbols" Data.Monoid.Unicode ( (⊕) )
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/mandarin.html
-}

-- | Cardinal numbers that are written the same using either
-- traditional or simplified characters.
trad_simpl_cardinals ∷ (Integral i) ⇒ [(i, Text)]
trad_simpl_cardinals =
  [ (0, "零")
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
  , (15, "十五")
  , (16, "十六")
  , (17, "十七")
  , (18, "十八")
  , (19, "十九")
  , (20, "二十")
  , (21, "二十一")
  , (22, "二十二")
  , (23, "二十三")
  , (24, "二十四")
  , (25, "二十五")
  , (26, "二十六")
  , (27, "二十七")
  , (28, "二十八")
  , (29, "二十九")
  , (30, "三十")
  , (31, "三十一")
  , (32, "三十二")
  , (33, "三十三")
  , (34, "三十四")
  , (35, "三十五")
  , (36, "三十六")
  , (37, "三十七")
  , (38, "三十八")
  , (39, "三十九")
  , (40, "四十")
  , (41, "四十一")
  , (42, "四十二")
  , (43, "四十三")
  , (44, "四十四")
  , (45, "四十五")
  , (46, "四十六")
  , (47, "四十七")
  , (48, "四十八")
  , (49, "四十九")
  , (50, "五十")
  , (51, "五十一")
  , (52, "五十二")
  , (53, "五十三")
  , (54, "五十四")
  , (55, "五十五")
  , (56, "五十六")
  , (57, "五十七")
  , (58, "五十八")
  , (59, "五十九")
  , (60, "六十")
  , (61, "六十一")
  , (62, "六十二")
  , (63, "六十三")
  , (64, "六十四")
  , (65, "六十五")
  , (66, "六十六")
  , (67, "六十七")
  , (68, "六十八")
  , (69, "六十九")
  , (100, "一百")
  , (114, "一百十四")
  , (200, "二百")
  , (205, "二百〇五")
  , (208, "二百〇八")
  , (280, "二百八十")
  , (301, "三百〇一")
  , (310, "三百十")
  , (1000, "一千")
  , (dec 12, "一兆")
  , (dec 16, "一京")
  , (dec 20, "一垓")
  , (dec 24, "一秭")
  , (dec 28, "一穰")
  , (dec 32, "一溝")
  , (dec 36, "一澗")
  , (dec 40, "一正")
  , (dec 44, "一載")
  ]

trad_cardinals ∷ (Integral i) ⇒ TestData i
trad_cardinals =
  [ ( "default"
    , defaultInflection
    , trad_simpl_cardinals
      ⊕ [ (2000, "兩千")
        , (2008, "兩千〇八")
        , (2080, "兩千〇八十")
        , (2362, "兩千三百六十二")
        , (2800, "兩千八百")
        , (dec 4, "一萬")
        , (10020, "一萬〇二十")
        , (97375, "九萬七千三百七十五")
        , (100004, "十萬〇四")
        , (8734072, "八百七十三萬四千〇七十二")
        , (10050026, "一千〇五萬〇二十六")
        , (dec 8, "一億")
        , (12345678902345, "十二兆三千四百五十六億七千八百九十萬兩千三百四十五")
        ]
    )
  ]

simpl_cardinals ∷ (Integral i) ⇒ TestData i
simpl_cardinals =
  [ ( "default"
    , defaultInflection
    , trad_simpl_cardinals
      ⊕ [ (2000, "两千")
        , (2008, "两千〇八")
        , (2080, "两千〇八十")
        , (2362, "两千三百六十二")
        , (2800, "两千八百")
        , (dec 4, "一万")
        , (dec 8, "一亿")
        ]
    )
  ]

finance_trad_cardinals ∷ (Integral i) ⇒ TestData i
finance_trad_cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "零")
      , (1, "壹")
      , (2, "貳")
      , (3, "参")
      , (4, "肆")
      , (5, "伍")
      , (6, "陸")
      , (7, "柒")
      , (8, "捌")
      , (9, "玖")
      , (10, "拾")
      , (100, "壹伯")
      , (1000, "壹仟")
      , (dec 4, "壹萬")
      , (dec 8, "壹億")
      ]
    )
  ]

finance_simpl_cardinals ∷ (Integral i) ⇒ TestData i
finance_simpl_cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "零")
      , (1, "壹")
      , (2, "贰")
      , (3, "参")
      , (4, "肆")
      , (5, "伍")
      , (6, "陆")
      , (7, "柒")
      , (8, "捌")
      , (9, "玖")
      , (10, "拾")
      , (100, "壹伯")
      , (1000, "壹仟")
      , (dec 4, "壹万")
      , (dec 8, "壹亿")
      ]
    )
  ]

pinyin_cardinals ∷ (Integral i) ⇒ TestData i
pinyin_cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "líng")
      , (1, "yī")
      , (2, "èr")
      , (3, "sān")
      , (4, "sì")
      , (5, "wǔ")
      , (6, "liù")
      , (7, "qī")
      , (8, "bā")
      , (9, "jiǔ")
      , (10, "shí")
      , (11, "shíyī")
      , (12, "shíèr")
      , (13, "shísān")
      , (14, "shísì")
      , (15, "shíwǔ")
      , (16, "shíliù")
      , (17, "shíqī")
      , (18, "shíbā")
      , (19, "shíjiǔ")
      , (20, "èrshí")
      , (21, "èrshí yī")
      , (22, "èrshí èr")
      , (23, "èrshí sān")
      , (24, "èrshí sì")
      , (25, "èrshí wǔ")
      , (26, "èrshí liù")
      , (27, "èrshí qī")
      , (28, "èrshí bā")
      , (29, "èrshí jiǔ")
      , (30, "sānshí")
      , (31, "sānshí yī")
      , (32, "sānshí èr")
      , (33, "sānshí sān")
      , (34, "sānshí sì")
      , (35, "sānshí wǔ")
      , (36, "sānshí liù")
      , (37, "sānshí qī")
      , (38, "sānshí bā")
      , (39, "sānshí jiǔ")
      , (40, "sìshí")
      , (41, "sìshí yī")
      , (42, "sìshí èr")
      , (43, "sìshí sān")
      , (44, "sìshí sì")
      , (45, "sìshí wǔ")
      , (46, "sìshí liù")
      , (47, "sìshí qī")
      , (48, "sìshí bā")
      , (49, "sìshí jiǔ")
      , (50, "wǔshí")
      , (51, "wǔshí yī")
      , (52, "wǔshí èr")
      , (53, "wǔshí sān")
      , (54, "wǔshí sì")
      , (55, "wǔshí wǔ")
      , (56, "wǔshí liù")
      , (57, "wǔshí qī")
      , (58, "wǔshí bā")
      , (59, "wǔshí jiǔ")
      , (60, "liùshí")
      , (61, "liùshí yī")
      , (62, "liùshí èr")
      , (63, "liùshí sān")
      , (64, "liùshí sì")
      , (65, "liùshí wǔ")
      , (66, "liùshí liù")
      , (67, "liùshí qī")
      , (68, "liùshí bā")
      , (69, "liùshí jiǔ")
      , (70, "qīshí")
      , (71, "qīshí yī")
      , (72, "qīshí èr")
      , (73, "qīshí sān")
      , (74, "qīshí sì")
      , (75, "qīshí wǔ")
      , (76, "qīshí liù")
      , (77, "qīshí qī")
      , (78, "qīshí bā")
      , (79, "qīshí jiǔ")
      , (80, "bāshí")
      , (81, "bāshí yī")
      , (82, "bāshí èr")
      , (83, "bāshí sān")
      , (84, "bāshí sì")
      , (85, "bāshí wǔ")
      , (86, "bāshí liù")
      , (87, "bāshí qī")
      , (88, "bāshí bā")
      , (89, "bāshí jiǔ")
      , (90, "jiǔshí")
      , (91, "jiǔshí yī")
      , (92, "jiǔshí èr")
      , (93, "jiǔshí sān")
      , (94, "jiǔshí sì")
      , (95, "jiǔshí wǔ")
      , (96, "jiǔshí liù")
      , (97, "jiǔshí qī")
      , (98, "jiǔshí bā")
      , (99, "jiǔshí jiǔ")
      , (100, "yībǎi")
      , (208, "èrbǎi líng bā")
      , (280, "èrbǎi bāshí")
      , (1000, "yīqiān")
      , (1002, "yīqiān líng èr")
      , (1020, "yīqiān líng èrshí")
      , (1200, "yīqiān èrbǎi")
      , (1203, "yīqiān èrbǎi líng sān")
      , (1230, "yīqiān èrbǎi sānshí")
      , (1234, "yīqiān èrbǎi sānshí sì")
      , (2008, "liǎngqiān líng bā")
      , (2080, "liǎngqiān líng bāshí")
      , (2800, "liǎngqiān bābǎi")
      , (97375, "jiǔwàn qīqiān sānbǎi qīshí wǔ")
      ]
    )
  ]
