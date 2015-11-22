{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        kap

[@Native name@]     бежкьалас миц

[@English name@]    Bezhta
-}
module Text.Numeral.Language.KAP.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-bezhta/en/kap/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "nol")
      , (1, "hõs")
      , (2, "q’ona")
      , (3, "łana")
      , (4, "ṏq’önä")
      , (5, "łina")
      , (6, "iłna")
      , (7, "aƛna")
      , (8, "beƛna")
      , (9, "äč’ena")
      , (10, "ac’ona")
      , (11, "ac’ona hõs")
      , (12, "ac’ona q’ona")
      , (13, "ac’ona łana")
      , (14, "ac’ona ṏq’önä")
      , (15, "ac’ona łina")
      , (16, "ac’ona iłna")
      , (17, "ac’ona aƛna")
      , (18, "ac’ona beƛna")
      , (19, "ac’ona äč’ena")
      , (20, "qona")
      , (21, "qona hõs")
      , (22, "qona q’ona")
      , (23, "qona łana")
      , (24, "qona ṏq’önä")
      , (25, "qona łina")
      , (26, "qona iłna")
      , (27, "qona aƛna")
      , (28, "qona beƛna")
      , (29, "qona äč’ena")
      , (30, "łanayig")
      , (31, "łanayig hõs")
      , (32, "łanayig q’ona")
      , (33, "łanayig łana")
      , (34, "łanayig ṏq’önä")
      , (35, "łanayig łina")
      , (36, "łanayig iłna")
      , (37, "łanayig aƛna")
      , (38, "łanayig beƛna")
      , (39, "łanayig äč’ena")
      , (40, "ṏq’önäyig")
      , (41, "ṏq’önäyig hõs")
      , (42, "ṏq’önäyig q’ona")
      , (43, "ṏq’önäyig łana")
      , (44, "ṏq’önäyig ṏq’önä")
      , (45, "ṏq’önäyig łina")
      , (46, "ṏq’önäyig iłna")
      , (47, "ṏq’önäyig aƛna")
      , (48, "ṏq’önäyig beƛna")
      , (49, "ṏq’önäyig äč’ena")
      , (50, "łinayig")
      , (51, "łinayig hõs")
      , (52, "łinayig q’ona")
      , (53, "łinayig łana")
      , (54, "łinayig ṏq’önä")
      , (55, "łinayig łina")
      , (56, "łinayig iłna")
      , (57, "łinayig aƛna")
      , (58, "łinayig beƛna")
      , (59, "łinayig äč’ena")
      , (60, "iłnayig")
      , (61, "iłnayig hõs")
      , (62, "iłnayig q’ona")
      , (63, "iłnayig łana")
      , (64, "iłnayig ṏq’önä")
      , (65, "iłnayig łina")
      , (66, "iłnayig iłna")
      , (67, "iłnayig aƛna")
      , (68, "iłnayig beƛna")
      , (69, "iłnayig äč’ena")
      , (70, "aƛnayig")
      , (71, "aƛnayig hõs")
      , (72, "aƛnayig q’ona")
      , (73, "aƛnayig łana")
      , (74, "aƛnayig ṏq’önä")
      , (75, "aƛnayig łina")
      , (76, "aƛnayig iłna")
      , (77, "aƛnayig aƛna")
      , (78, "aƛnayig beƛna")
      , (79, "aƛnayig äč’ena")
      , (80, "beƛnayig")
      , (81, "beƛnayig hõs")
      , (82, "beƛnayig q’ona")
      , (83, "beƛnayig łana")
      , (84, "beƛnayig ṏq’önä")
      , (85, "beƛnayig łina")
      , (86, "beƛnayig iłna")
      , (87, "beƛnayig aƛna")
      , (88, "beƛnayig beƛna")
      , (89, "beƛnayig äč’ena")
      , (90, "äč’enayig")
      , (91, "äč’enayig hõs")
      , (92, "äč’enayig q’ona")
      , (93, "äč’enayig łana")
      , (94, "äč’enayig ṏq’önä")
      , (95, "äč’enayig łina")
      , (96, "äč’enayig iłna")
      , (97, "äč’enayig aƛna")
      , (98, "äč’enayig beƛna")
      , (99, "äč’enayig äč’ena")
      , (100, "hõsč’it’")
      , (101, "hõsč’it’ hõs")
      , (102, "hõsč’it’ q’ona")
      , (103, "hõsč’it’ łana")
      , (104, "hõsč’it’ ṏq’önä")
      , (105, "hõsč’it’ łina")
      , (106, "hõsč’it’ iłna")
      , (107, "hõsč’it’ aƛna")
      , (108, "hõsč’it’ beƛna")
      , (109, "hõsč’it’ äč’ena")
      , (110, "hõsč’it’ ac’ona")
      , (123, "hõsč’it’ qona łana")
      , (200, "q’onač’it’")
      , (300, "łanač’it’")
      , (321, "łanač’it’ qona hõs")
      , (400, "ṏq’önäč’it’")
      , (500, "łinač’it’")
      , (600, "iłnač’it’")
      , (700, "aƛnač’it’")
      , (800, "beƛnač’it’")
      , (900, "äč’enač’it’")
      , (909, "äč’enač’it’ äč’ena")
      , (990, "äč’enač’it’ äč’enayig")
      , (999, "äč’enač’it’ äč’enayig äč’ena")
      , (1000, "hazay")
      ]
    )
  ]
