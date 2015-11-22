{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        wmw

[@Native name@]     -

[@English name@]    Mwani
-}
module Text.Numeral.Language.WMW.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-mwani/en/wmw/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "m’moja")
      , (2, "mbire")
      , (3, "natu")
      , (4, "n’né")
      , (5, "n’tano")
      , (6, "sita")
      , (7, "saba")
      , (8, "nane")
      , (9, "khénta")
      , (10, "kumi")
      , (11, "kumi na m’moja")
      , (12, "kumi na mbire")
      , (13, "kumi na natu")
      , (14, "kumi na n’né")
      , (15, "kumi na n’tano")
      , (16, "kumi na sita")
      , (17, "kumi na saba")
      , (18, "kumi na nane")
      , (19, "kumi na khénta")
      , (20, "shirini")
      , (21, "shirini na m’moja")
      , (22, "shirini na mbire")
      , (23, "shirini na natu")
      , (24, "shirini na n’né")
      , (25, "shirini na n’tano")
      , (26, "shirini na sita")
      , (27, "shirini na saba")
      , (28, "shirini na nane")
      , (29, "shirini na khénta")
      , (30, "talatini")
      , (31, "talatini na m’moja")
      , (32, "talatini na mbire")
      , (33, "talatini na natu")
      , (34, "talatini na n’né")
      , (35, "talatini na n’tano")
      , (36, "talatini na sita")
      , (37, "talatini na saba")
      , (38, "talatini na nane")
      , (39, "talatini na khénta")
      , (40, "arubaine")
      , (41, "arubaine na m’moja")
      , (42, "arubaine na mbire")
      , (43, "arubaine na natu")
      , (44, "arubaine na n’né")
      , (45, "arubaine na n’tano")
      , (46, "arubaine na sita")
      , (47, "arubaine na saba")
      , (48, "arubaine na nane")
      , (49, "arubaine na khénta")
      , (50, "amusine")
      , (51, "amusine na m’moja")
      , (52, "amusine na mbire")
      , (53, "amusine na natu")
      , (54, "amusine na n’né")
      , (55, "amusine na n’tano")
      , (56, "amusine na sita")
      , (57, "amusine na saba")
      , (58, "amusine na nane")
      , (59, "amusine na khénta")
      , (60, "sitine")
      , (61, "sitine na m’moja")
      , (62, "sitine na mbire")
      , (63, "sitine na natu")
      , (64, "sitine na n’né")
      , (65, "sitine na n’tano")
      , (66, "sitine na sita")
      , (67, "sitine na saba")
      , (68, "sitine na nane")
      , (69, "sitine na khénta")
      , (70, "sabine")
      , (71, "sabine na m’moja")
      , (72, "sabine na mbire")
      , (73, "sabine na natu")
      , (74, "sabine na n’né")
      , (75, "sabine na n’tano")
      , (76, "sabine na sita")
      , (77, "sabine na saba")
      , (78, "sabine na nane")
      , (79, "sabine na khénta")
      , (80, "tamanine")
      , (81, "tamanine na m’moja")
      , (82, "tamanine na mbire")
      , (83, "tamanine na natu")
      , (84, "tamanine na n’né")
      , (85, "tamanine na n’tano")
      , (86, "tamanine na sita")
      , (87, "tamanine na saba")
      , (88, "tamanine na nane")
      , (89, "tamanine na khénta")
      , (90, "tusuine")
      , (91, "tusuine na m’moja")
      , (92, "tusuine na mbire")
      , (93, "tusuine na natu")
      , (94, "tusuine na n’né")
      , (95, "tusuine na n’tano")
      , (96, "tusuine na sita")
      , (97, "tusuine na saba")
      , (98, "tusuine na nane")
      , (99, "tusuine na khénta")
      , (100, "mia")
      , (101, "mia na m’moja")
      , (102, "mia na mbire")
      , (103, "mia na natu")
      , (104, "mia na n’né")
      , (105, "mia na n’tano")
      , (106, "mia na sita")
      , (107, "mia na saba")
      , (108, "mia na nane")
      , (109, "mia na khénta")
      , (110, "mia na kumi")
      , (123, "mia na shirini na natu")
      , (200, "mia mbire")
      , (300, "mia natu")
      , (321, "mia natu na shirini na m’moja")
      , (400, "mia n’né")
      , (500, "mia n’tano")
      , (600, "mia sita")
      , (700, "mia saba")
      , (800, "mia nane")
      , (900, "mia khénta")
      , (909, "mia khénta na khénta")
      , (990, "mia khénta na tusuine")
      , (999, "mia khénta na tusuine na khénta")
      , (1000, "álufu")
      , (1001, "álufu m’moja na m’moja")
      , (1008, "álufu m’moja na nane")
      , (1234, "álufu m’moja na mia mbire na talatini na n’né")
      , (2000, "álufu mbire")
      , (3000, "álufu natu")
      , (4000, "álufu n’né")
      , (4321, "álufu n’né na mia natu na shirini na m’moja")
      , (5000, "álufu n’tano")
      , (6000, "álufu sita")
      , (7000, "álufu saba")
      , (8000, "álufu nane")
      , (9000, "álufu khénta")
      ]
    )
  ]
