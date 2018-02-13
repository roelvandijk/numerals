{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        nrf

[@Native name@]     Jèrriais

[@English name@]    Jersey French / Jersey Norman French
-}
module Text.Numeral.Language.NRF.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "numerals" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-jerriais/en/fra-jer/
  https://www.omniglot.com/language/numbers/jerriais.htm
  http://officedujerriais.blogspot.nl/2012/02/numbers-in-jerriais.html
-}

cardinals :: (Integral i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "ieune")
      , (2, "deux")
      , (3, "trais")
      , (4, "quat’")
      , (5, "chîn")
      , (6, "six")
      , (7, "sept")
      , (8, "huit")
      , (9, "neuf")
      , (10, "dgix")
      , (11, "onze")
      , (12, "douze")
      , (13, "treize")
      , (14, "quatorze")
      , (15, "tchînze")
      , (16, "seize")
      , (17, "dgiêx-sept")
      , (18, "dgiêx-huit")
      , (19, "dgiêx-neuf")
      , (20, "vîngt")
      , (21, "vîngt’tch’ieune")
      , (22, "vîngt-deux")
      , (23, "vîngt-trais")
      , (24, "vîngt-quat’")
      , (25, "vîngt-chîn")
      , (26, "vîngt-six")
      , (27, "vîngt-sept")
      , (28, "vîngt-huit")
      , (29, "vîngt-neuf")
      , (30, "trente")
      , (31, "trente’tch’ieune")
      , (32, "trente-deux")
      , (33, "trente-trais")
      , (34, "trente-quat’")
      , (35, "trente-chîn")
      , (36, "trente-six")
      , (37, "trente-sept")
      , (38, "trente-huit")
      , (39, "trente-neuf")
      , (40, "quarante")
      , (41, "quarante’tch’ieune")
      , (42, "quarante-deux")
      , (43, "quarante-trais")
      , (44, "quarante-quat’")
      , (45, "quarante-chîn")
      , (46, "quarante-six")
      , (47, "quarante-sept")
      , (48, "quarante-huit")
      , (49, "quarante-neuf")
      , (50, "chînquante")
      , (51, "chînquante’tch’ieune")
      , (52, "chînquante-deux")
      , (53, "chînquante-trais")
      , (54, "chînquante-quat’")
      , (55, "chînquante-chîn")
      , (56, "chînquante-six")
      , (57, "chînquante-sept")
      , (58, "chînquante-huit")
      , (59, "chînquante-neuf")
      , (60, "souaixante")
      , (61, "souaixante’tch’ieune")
      , (62, "souaixante-deux")
      , (63, "souaixante-trais")
      , (64, "souaixante-quat’")
      , (65, "souaixante-chîn")
      , (66, "souaixante-six")
      , (67, "souaixante-sept")
      , (68, "souaixante-huit")
      , (69, "souaixante-neuf")
      , (70, "septante")
      , (71, "septante’tch’ieune")
      , (72, "septante-deux")
      , (73, "septante-trais")
      , (74, "septante-quat’")
      , (75, "septante-chîn")
      , (76, "septante-six")
      , (77, "septante-sept")
      , (78, "septante-huit")
      , (79, "septante-neuf")
      , (80, "quat’-vîngts")
      , (81, "quat’-vîngt-ieune")
      , (82, "quat’-vîngt-deux")
      , (83, "quat’-vîngt-trais")
      , (84, "quat’-vîngt-quat’")
      , (85, "quat’-vîngt-chîn")
      , (86, "quat’-vîngt-six")
      , (87, "quat’-vîngt-sept")
      , (88, "quat’-vîngt-huit")
      , (89, "quat’-vîngt-neuf")
      , (90, "nénante")
      , (91, "nénante’tch’ieune")
      , (92, "nénante-deux")
      , (93, "nénante-trais")
      , (94, "nénante-quat’")
      , (95, "nénante-chîn")
      , (96, "nénante-six")
      , (97, "nénante-sept")
      , (98, "nénante-huit")
      , (99, "nénante-neuf")
      , (100, "chent")
      , (101, "chent ieune")
      , (102, "chent deux")
      , (103, "chent trais")
      , (104, "chent quat’")
      , (105, "chent chîn")
      , (106, "chent six")
      , (107, "chent sept")
      , (108, "chent huit")
      , (109, "chent neuf")
      , (110, "chent dgix")
      , (123, "chent vîngt-trais")
      , (200, "deux chents")
      , (300, "trais chents")
      , (321, "trais chents vîngt’tch’ieune")
      , (400, "quat’ chents")
      , (500, "chîn chents")
      , (600, "siêx chents")
      , (700, "sept chents")
      , (800, "huit chents")
      , (900, "neu chents")
      , (909, "neu chents neuf")
      , (990, "neu chents nénante")
      , (999, "neu chents nénante-neuf")
      , (1000, "mille")
      , (2000, "deux mille")
      , (3000, "trais mille")
      , (4000, "quat' mille")
      , (5000, "chîn mille")
      , (6000, "siêx mille")
      , (7000, "sept mille")
      , (8000, "huit mille")
      , (9000, "neuf mille")
      , (dec 6, "un million")
      ]
    )
  ]
