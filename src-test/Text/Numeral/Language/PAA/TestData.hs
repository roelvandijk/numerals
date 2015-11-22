{-|
[@ISO639-1@]        -

[@ISO639-2B@]       paa

[@ISO639-3@]        hui

[@Native name@]     -

[@English name@]    Huli
-}

module Text.Numeral.Language.PAA.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "mbira")
      , (2, "kira")
      , (3, "tebira")
      , (4, "maria")
      , (5, "duria")
      , (6, "waragaria")
      , (7, "karia")
      , (8, "halira")
      , (9, "dira")
      , (10, "pira")
      , (11, "bearia")
      , (12, "hombearia")
      , (13, "haleria")
      , (14, "deria")
      , (15, "nguira")
      , (16, "nguira-ni mbira")
      , (17, "nguira-ni kira")
      , (18, "nguira-ni tebira")
      , (19, "nguira-ni maria")
      , (20, "nguira-ni duria")
      , (21, "nguira-ni waragaria")
      , (22, "nguira-ni karia")
      , (23, "nguira-ni halira")
      , (24, "nguira-ni dira")
      , (25, "nguira-ni pira")
      , (26, "nguira-ni bearia")
      , (27, "nguira-ni hombearia")
      , (28, "nguira-ni haleria")
      , (29, "nguira-ni deria")
      , (30, "ngui ki")
      , (31, "ngui ki, ngui tebone-gonaga mbira")
      , (32, "ngui ki, ngui tebone-gonaga kira")
      , (33, "ngui ki, ngui tebone-gonaga tebira")
      , (34, "ngui ki, ngui tebone-gonaga maria")
      , (35, "ngui ki, ngui tebone-gonaga duria")
      , (36, "ngui ki, ngui tebone-gonaga waragaria")
      , (37, "ngui ki, ngui tebone-gonaga karia")
      , (38, "ngui ki, ngui tebone-gonaga halira")
      , (39, "ngui ki, ngui tebone-gonaga dira")
      , (40, "ngui ki, ngui tebone-gonaga pira")
      , (41, "ngui ki, ngui tebone-gonaga bearia")
      , (42, "ngui ki, ngui tebone-gonaga hombearia")
      , (43, "ngui ki, ngui tebone-gonaga haleria")
      , (44, "ngui ki, ngui tebone-gonaga deria")
      , (45, "ngui tebo")
      , (46, "ngui tebo, ngui mane-gonaga mbira")
      , (47, "ngui tebo, ngui mane-gonaga kira")
      , (48, "ngui tebo, ngui mane-gonaga tebira")
      , (49, "ngui tebo, ngui mane-gonaga maria")
      , (50, "ngui tebo, ngui mane-gonaga duria")
      , (51, "ngui tebo, ngui mane-gonaga waragaria")
      , (52, "ngui tebo, ngui mane-gonaga karia")
      , (53, "ngui tebo, ngui mane-gonaga halira")
      , (54, "ngui tebo, ngui mane-gonaga dira")
      , (55, "ngui tebo, ngui mane-gonaga pira")
      , (56, "ngui tebo, ngui mane-gonaga bearia")
      , (57, "ngui tebo, ngui mane-gonaga hombearia")
      , (58, "ngui tebo, ngui mane-gonaga haleria")
      , (59, "ngui tebo, ngui mane-gonaga deria")
      , (60, "ngui ma")
      , (61, "ngui ma, ngui dauni-gonaga mbira")
      , (62, "ngui ma, ngui dauni-gonaga kira")
      , (63, "ngui ma, ngui dauni-gonaga tebira")
      , (64, "ngui ma, ngui dauni-gonaga maria")
      , (65, "ngui ma, ngui dauni-gonaga duria")
      , (66, "ngui ma, ngui dauni-gonaga waragaria")
      , (67, "ngui ma, ngui dauni-gonaga karia")
      , (68, "ngui ma, ngui dauni-gonaga halira")
      , (69, "ngui ma, ngui dauni-gonaga dira")
      , (70, "ngui ma, ngui dauni-gonaga pira")
      , (71, "ngui ma, ngui dauni-gonaga bearia")
      , (72, "ngui ma, ngui dauni-gonaga hombearia")
      , (73, "ngui ma, ngui dauni-gonaga haleria")
      , (74, "ngui ma, ngui dauni-gonaga deria")
      , (75, "ngui dau")
      , (76, "ngui dau, ngui waragane-gonaga mbira")
      , (77, "ngui dau, ngui waragane-gonaga kira")
      , (78, "ngui dau, ngui waragane-gonaga tebira")
      , (79, "ngui dau, ngui waragane-gonaga maria")
      , (80, "ngui dau, ngui waragane-gonaga duria")
      , (81, "ngui dau, ngui waragane-gonaga waragaria")
      , (82, "ngui dau, ngui waragane-gonaga karia")
      , (83, "ngui dau, ngui waragane-gonaga halira")
      , (84, "ngui dau, ngui waragane-gonaga dira")
      , (85, "ngui dau, ngui waragane-gonaga pira")
      , (86, "ngui dau, ngui waragane-gonaga bearia")
      , (87, "ngui dau, ngui waragane-gonaga hombearia")
      , (88, "ngui dau, ngui waragane-gonaga haleria")
      , (89, "ngui dau, ngui waragane-gonaga deria")
      , (90, "ngui waraga")
      , (91, "ngui waraga, ngui kane-gonaga mbira")
      , (92, "ngui waraga, ngui kane-gonaga kira")
      , (93, "ngui waraga, ngui kane-gonaga tebira")
      , (94, "ngui waraga, ngui kane-gonaga maria")
      , (95, "ngui waraga, ngui kane-gonaga duria")
      , (96, "ngui waraga, ngui kane-gonaga waragaria")
      , (97, "ngui waraga, ngui kane-gonaga karia")
      , (98, "ngui waraga, ngui kane-gonaga halira")
      , (99, "ngui waraga, ngui kane-gonaga dira")
      , (100, "ngui waraga, ngui kane-gonaga pira")
      ]
    )
  ]
