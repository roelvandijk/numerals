{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        one

[@Native name@]     Onʌyotaʔa:ka

[@English name@]    Oneida
-}
module Text.Numeral.Language.ONE.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude ( Num )
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-oneida/en/one/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "úskah")
      , (2, "téken")
      , (3, "áhsʌ")
      , (4, "kayé")
      , (5, "wisk")
      , (6, "yá·yahk")
      , (7, "tsya·ták")
      , (8, "tékluˀ")
      , (9, "wá·tlu")
      , (10, "oye·lí")
      , (11, "úskah yawʌ·lé")
      , (12, "tékni yawʌ·lé")
      , (13, "áhsʌ yawʌ·lé")
      , (14, "kayé yawʌ·lé")
      , (15, "wisk yawʌ·lé")
      , (16, "yá·yahk yawʌ·lé")
      , (17, "tsya·ták yawʌ·lé")
      , (18, "tékluˀ yawʌ·lé")
      , (19, "wá·tlu yawʌ·lé")
      , (20, "tewáshʌ")
      , (21, "tewáshʌ úskah")
      , (22, "tewáshʌ téken")
      , (23, "tewáshʌ áhsʌ")
      , (24, "tewáshʌ kayé")
      , (25, "tewáshʌ wisk")
      , (26, "tewáshʌ yá·yahk")
      , (27, "tewáshʌ tsya·ták")
      , (28, "tewáshʌ tékluˀ")
      , (29, "tewáshʌ wá·tlu")
      , (30, "áhsʌ niwáshʌ")
      , (31, "áhsʌ niwáshʌ úskah")
      , (32, "áhsʌ niwáshʌ téken")
      , (33, "áhsʌ niwáshʌ áhsʌ")
      , (34, "áhsʌ niwáshʌ kayé")
      , (35, "áhsʌ niwáshʌ wisk")
      , (36, "áhsʌ niwáshʌ yá·yahk")
      , (37, "áhsʌ niwáshʌ tsya·ták")
      , (38, "áhsʌ niwáshʌ tékluˀ")
      , (39, "áhsʌ niwáshʌ wá·tlu")
      , (40, "kayé niwáshʌ")
      , (41, "kayé niwáshʌ úskah")
      , (42, "kayé niwáshʌ téken")
      , (43, "kayé niwáshʌ áhsʌ")
      , (44, "kayé niwáshʌ kayé")
      , (45, "kayé niwáshʌ wisk")
      , (46, "kayé niwáshʌ yá·yahk")
      , (47, "kayé niwáshʌ tsya·ták")
      , (48, "kayé niwáshʌ tékluˀ")
      , (49, "kayé niwáshʌ wá·tlu")
      , (50, "wisk niwáshʌ")
      , (51, "wisk niwáshʌ úskah")
      , (52, "wisk niwáshʌ téken")
      , (53, "wisk niwáshʌ áhsʌ")
      , (54, "wisk niwáshʌ kayé")
      , (55, "wisk niwáshʌ wisk")
      , (56, "wisk niwáshʌ yá·yahk")
      , (57, "wisk niwáshʌ tsya·ták")
      , (58, "wisk niwáshʌ tékluˀ")
      , (59, "wisk niwáshʌ wá·tlu")
      , (60, "yá·yahk niwáshʌ")
      , (61, "yá·yahk niwáshʌ úskah")
      , (62, "yá·yahk niwáshʌ téken")
      , (63, "yá·yahk niwáshʌ áhsʌ")
      , (64, "yá·yahk niwáshʌ kayé")
      , (65, "yá·yahk niwáshʌ wisk")
      , (66, "yá·yahk niwáshʌ yá·yahk")
      , (67, "yá·yahk niwáshʌ tsya·ták")
      , (68, "yá·yahk niwáshʌ tékluˀ")
      , (69, "yá·yahk niwáshʌ wá·tlu")
      , (70, "tsya·ták niwáshʌ")
      , (71, "tsya·ták niwáshʌ úskah")
      , (72, "tsya·ták niwáshʌ téken")
      , (73, "tsya·ták niwáshʌ áhsʌ")
      , (74, "tsya·ták niwáshʌ kayé")
      , (75, "tsya·ták niwáshʌ wisk")
      , (76, "tsya·ták niwáshʌ yá·yahk")
      , (77, "tsya·ták niwáshʌ tsya·ták")
      , (78, "tsya·ták niwáshʌ tékluˀ")
      , (79, "tsya·ták niwáshʌ wá·tlu")
      , (80, "tékluˀ niwáshʌ")
      , (81, "tékluˀ niwáshʌ úskah")
      , (82, "tékluˀ niwáshʌ téken")
      , (83, "tékluˀ niwáshʌ áhsʌ")
      , (84, "tékluˀ niwáshʌ kayé")
      , (85, "tékluˀ niwáshʌ wisk")
      , (86, "tékluˀ niwáshʌ yá·yahk")
      , (87, "tékluˀ niwáshʌ tsya·ták")
      , (88, "tékluˀ niwáshʌ tékluˀ")
      , (89, "tékluˀ niwáshʌ wá·tlu")
      , (90, "wá·tlu niwáshʌ")
      , (91, "wá·tlu niwáshʌ úskah")
      , (92, "wá·tlu niwáshʌ téken")
      , (93, "wá·tlu niwáshʌ áhsʌ")
      , (94, "wá·tlu niwáshʌ kayé")
      , (95, "wá·tlu niwáshʌ wisk")
      , (96, "wá·tlu niwáshʌ yá·yahk")
      , (97, "wá·tlu niwáshʌ tsya·ták")
      , (98, "wá·tlu niwáshʌ tékluˀ")
      , (99, "wá·tlu niwáshʌ wá·tlu")
      , (100, "tewʌˀnyáwelu")
      , (101, "tewʌˀnyáwelu ok úskah")
      , (102, "tewʌˀnyáwelu ok téken")
      , (103, "tewʌˀnyáwelu ok áhsʌ")
      , (104, "tewʌˀnyáwelu ok kayé")
      , (105, "tewʌˀnyáwelu ok wisk")
      , (106, "tewʌˀnyáwelu ok yá·yahk")
      , (107, "tewʌˀnyáwelu ok tsya·ták")
      , (108, "tewʌˀnyáwelu ok tékluˀ")
      , (109, "tewʌˀnyáwelu ok wá·tlu")
      , (110, "tewʌˀnyáwelu ok oye·lí")
      , (123, "tewʌˀnyáwelu ok tewáshʌ áhsʌ")
      , (200, "tékni tewʌˀnyáwelu")
      , (300, "áhsʌ tewʌˀnyáwelu")
      , (321, "áhsʌ tewʌˀnyáwelu ok tewáshʌ úskah")
      , (400, "kayé tewʌˀnyáwelu")
      , (500, "wisk tewʌˀnyáwelu")
      , (600, "yá·yahk tewʌˀnyáwelu")
      , (700, "tsya·ták tewʌˀnyáwelu")
      , (800, "tékluˀ tewʌˀnyáwelu")
      , (900, "wá·tlu tewʌˀnyáwelu")
      , (909, "wá·tlu tewʌˀnyáwelu ok wá·tlu")
      , (990, "wá·tlu tewʌˀnyáwelu ok wá·tlu niwáshʌ")
      , (999, "wá·tlu tewʌˀnyáwelu ok wá·tlu niwáshʌ wá·tlu")
      , (1000, "skanutó·tslat")
      ]
    )
  ]
