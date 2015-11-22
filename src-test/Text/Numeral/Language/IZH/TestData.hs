{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        izh

[@Native name@]     ižoran keel

[@English name@]    Ingrian
-}
module Text.Numeral.Language.IZH.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-ingrian/en/izh/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "null")
      , (1, "üks")
      , (2, "kaks")
      , (3, "kold")
      , (4, "neljä")
      , (5, "viis")
      , (6, "kuus")
      , (7, "seitsemän")
      , (8, "kaheksan")
      , (9, "üheksän")
      , (10, "kümmenän")
      , (11, "yksitoista")
      , (12, "kakstoist")
      , (13, "koldtoist")
      , (14, "neljätoist")
      , (15, "viistoist")
      , (16, "kuustoist")
      , (17, "seitsemäntoist")
      , (18, "kaheksantoist")
      , (19, "üheksäntoist")
      , (20, "kakskümmend")
      , (21, "kakskümmend üks")
      , (22, "kakskümmend kaks")
      , (23, "kakskümmend kold")
      , (24, "kakskümmend neljä")
      , (25, "kakskümmend viis")
      , (26, "kakskümmend kuus")
      , (27, "kakskümmend seitsemän")
      , (28, "kakskümmend kaheksan")
      , (29, "kakskümmend üheksän")
      , (30, "koldkümmend")
      , (31, "koldkümmend üks")
      , (32, "koldkümmend kaks")
      , (33, "koldkümmend kold")
      , (34, "koldkümmend neljä")
      , (35, "koldkümmend viis")
      , (36, "koldkümmend kuus")
      , (37, "koldkümmend seitsemän")
      , (38, "koldkümmend kaheksan")
      , (39, "koldkümmend üheksän")
      , (40, "neljäkümmend")
      , (41, "neljäkümmend üks")
      , (42, "neljäkümmend kaks")
      , (43, "neljäkümmend kold")
      , (44, "neljäkümmend neljä")
      , (45, "neljäkümmend viis")
      , (46, "neljäkümmend kuus")
      , (47, "neljäkümmend seitsemän")
      , (48, "neljäkümmend kaheksan")
      , (49, "neljäkümmend üheksän")
      , (50, "viiskümmend")
      , (51, "viiskümmend üks")
      , (52, "viiskümmend kaks")
      , (53, "viiskümmend kold")
      , (54, "viiskümmend neljä")
      , (55, "viiskümmend viis")
      , (56, "viiskümmend kuus")
      , (57, "viiskümmend seitsemän")
      , (58, "viiskümmend kaheksan")
      , (59, "viiskümmend üheksän")
      , (60, "kuuskümmend")
      , (61, "kuuskümmend üks")
      , (62, "kuuskümmend kaks")
      , (63, "kuuskümmend kold")
      , (64, "kuuskümmend neljä")
      , (65, "kuuskümmend viis")
      , (66, "kuuskümmend kuus")
      , (67, "kuuskümmend seitsemän")
      , (68, "kuuskümmend kaheksan")
      , (69, "kuuskümmend üheksän")
      , (70, "seitsemänkümmend")
      , (71, "seitsemänkümmend üks")
      , (72, "seitsemänkümmend kaks")
      , (73, "seitsemänkümmend kold")
      , (74, "seitsemänkümmend neljä")
      , (75, "seitsemänkümmend viis")
      , (76, "seitsemänkümmend kuus")
      , (77, "seitsemänkümmend seitsemän")
      , (78, "seitsemänkümmend kaheksan")
      , (79, "seitsemänkümmend üheksän")
      , (80, "kaheksankümmend")
      , (81, "kaheksankümmend üks")
      , (82, "kaheksankümmend kaks")
      , (83, "kaheksankümmend kold")
      , (84, "kaheksankümmend neljä")
      , (85, "kaheksankümmend viis")
      , (86, "kaheksankümmend kuus")
      , (87, "kaheksankümmend seitsemän")
      , (88, "kaheksankümmend kaheksan")
      , (89, "kaheksankümmend üheksän")
      , (90, "üheksänkümmend")
      , (91, "üheksänkümmend üks")
      , (92, "üheksänkümmend kaks")
      , (93, "üheksänkümmend kold")
      , (94, "üheksänkümmend neljä")
      , (95, "üheksänkümmend viis")
      , (96, "üheksänkümmend kuus")
      , (97, "üheksänkümmend seitsemän")
      , (98, "üheksänkümmend kaheksan")
      , (99, "üheksänkümmend üheksän")
      , (100, "sada")
      ]
    )
  ]
