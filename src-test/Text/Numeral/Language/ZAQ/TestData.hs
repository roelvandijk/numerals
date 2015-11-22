{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        zaq

[@Native name@]     -

[@English name@]    Aloápam Zapotec
-}
module Text.Numeral.Language.ZAQ.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-aloapam-zapotec/en/zaq/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "ttubi")
      , (2, "chupa")
      , (3, "tsunna")
      , (4, "ttapa")
      , (5, "gayu")
      , (6, "xxupa")
      , (7, "gasi")
      , (8, "xxunu")
      , (9, "jaa")
      , (10, "tsii")
      , (11, "sinia")
      , (12, "tsi’inu")
      , (13, "tsi’intsagüi")
      , (14, "sitá")
      , (15, "tsinu")
      , (16, "sixupa")
      , (17, "tsini")
      , (18, "sixunu")
      , (19, "chennia")
      , (20, "galhia")
      , (21, "ttuerua")
      , (22, "chuperua")
      , (23, "tsunerua")
      , (24, "ttaperua")
      , (25, "gayuerua")
      , (26, "xxuperua")
      , (27, "gasierua")
      , (28, "xxunuerua")
      , (29, "jaerua")
      , (30, "rerua")
      , (31, "rerua yu’u ttu")
      , (32, "rerua yu’u chupa")
      , (33, "rerua yu’u tsunna")
      , (34, "rerua yu’u ttapa")
      , (35, "rerua yu’u gayu")
      , (36, "rerua yu’u xxupa")
      , (37, "rerua yu’u gasi")
      , (38, "rerua yu’u xxunu")
      , (39, "rerua yu’u jaa")
      , (40, "chua")
      , (41, "chua yu’u ttu")
      , (42, "chua yu’u chupa")
      , (43, "chua yu’u tsunna")
      , (44, "chua yu’u ttapa")
      , (45, "chua yu’u gayu")
      , (46, "chua yu’u xxupa")
      , (47, "chua yu’u gasi")
      , (48, "chua yu’u xxunu")
      , (49, "chua yu’u jaa")
      , (50, "medi gayua")
      , (51, "medi gayua yu’u ttu")
      , (52, "medi gayua yu’u chupa")
      , (53, "medi gayua yu’u tsunna")
      , (54, "medi gayua yu’u ttapa")
      , (55, "medi gayua yu’u gayu")
      , (56, "medi gayua yu’u xxupa")
      , (57, "medi gayua yu’u gasi")
      , (58, "medi gayua yu’u xxunu")
      , (59, "medi gayua yu’u jaa")
      , (60, "gayuna")
      , (61, "gayuna yu’u ttu")
      , (62, "gayuna yu’u chupa")
      , (63, "gayuna yu’u tsunna")
      , (64, "gayuna yu’u ttapa")
      , (65, "gayuna yu’u gayu")
      , (66, "gayuna yu’u xxupa")
      , (67, "gayuna yu’u gasi")
      , (68, "gayuna yu’u xxunu")
      , (69, "gayuna yu’u jaa")
      , (70, "gayuna yu’u tsii")
      , (71, "gayuna yu’u sinia")
      , (72, "gayuna yu’u tsi’inu")
      , (73, "gayuna yu’u si’intse")
      , (74, "gayuna yu’u sitá")
      , (75, "gayuna yu’u tsinu")
      , (76, "gayuna yu’u sixupa")
      , (77, "gayuna yu’u tsini")
      , (78, "gayuna yu’u sixunu")
      , (79, "gayuna yu’u chennia")
      , (80, "ta")
      , (81, "ta yu’u ttu")
      , (82, "ta yu’u chupa")
      , (83, "ta yu’u tsunna")
      , (84, "ta yu’u ttapa")
      , (85, "ta yu’u gayu")
      , (86, "ta yu’u xxupa")
      , (87, "ta yu’u gasi")
      , (88, "ta yu’u xxunu")
      , (89, "ta yu’u jaa")
      , (90, "ta yu’u tsii")
      , (91, "ta yu’u sinia")
      , (92, "ta yu’u tsi’inu")
      , (93, "ta yu’u si’intse")
      , (94, "ta yu’u sitá")
      , (95, "ta yu’u tsinu")
      , (96, "ta yu’u sixupa")
      , (97, "ta yu’u tsini")
      , (98, "ta yu’u sixunu")
      , (99, "ta yu’u chennia")
      , (100, "ttu gayua")
      , (101, "ttu gayua ttubi")
      , (102, "ttu gayua chupa")
      , (103, "ttu gayua tsunna")
      , (104, "ttu gayua ttapa")
      , (105, "ttu gayua gayu")
      , (106, "ttu gayua xxupa")
      , (107, "ttu gayua gasi")
      , (108, "ttu gayua xxunu")
      , (109, "ttu gayua jaa")
      , (110, "ttu gayua tsii")
      , (123, "ttu gayua tsunerua")
      , (200, "chupa gayua")
      , (300, "tsunna gayua")
      , (321, "tsunna gayua ttuerua")
      , (400, "ttapa gayua")
      , (500, "gayu gayua")
      , (600, "xxupa gayua")
      , (700, "gasi gayua")
      , (800, "xxunu gayua")
      , (900, "jaa gayua")
      , (909, "jaa gayua jaa")
      , (990, "jaa gayua ta yu’u tsii")
      , (999, "jaa gayua ta yu’u chennia")
      ]
    )
  ]
