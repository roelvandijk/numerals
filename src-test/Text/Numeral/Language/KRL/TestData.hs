{-|
[@ISO639-1@]        -

[@ISO639-2@]        krl

[@ISO639-3@]        krl

[@Native name@]     karjala, karjal, kariela

[@English name@]    Karelian
-}
module Text.Numeral.Language.KRL.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-karelian/en/krl/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "yksi")
      , (2, "kakši")
      , (3, "kolme")
      , (4, "neljjä")
      , (5, "viizi")
      , (6, "kuuǯi")
      , (7, "seiččemän")
      , (8, "kahekšan")
      , (9, "yhekšän")
      , (10, "kymmenen")
      , (11, "yksitoista")
      , (12, "kakšitoista")
      , (13, "kolmetoista")
      , (14, "neljjätoista")
      , (15, "viiztoista")
      , (16, "kuuǯitoista")
      , (17, "seiččemäntoista")
      , (18, "kahekšantoista")
      , (19, "yhekšäntoista")
      , (20, "kakšikymmendä")
      , (21, "kakšikymmendä yksi")
      , (22, "kakšikymmendä kakši")
      , (23, "kakšikymmendä kolme")
      , (24, "kakšikymmendä neljjä")
      , (25, "kakšikymmendä viizi")
      , (26, "kakšikymmendä kuuǯi")
      , (27, "kakšikymmendä seiččemän")
      , (28, "kakšikymmendä kahekšan")
      , (29, "kakšikymmendä yhekšän")
      , (30, "kolmekymmendä")
      , (31, "kolmekymmendä yksi")
      , (32, "kolmekymmendä kakši")
      , (33, "kolmekymmendä kolme")
      , (34, "kolmekymmendä neljjä")
      , (35, "kolmekymmendä viizi")
      , (36, "kolmekymmendä kuuǯi")
      , (37, "kolmekymmendä seiččemän")
      , (38, "kolmekymmendä kahekšan")
      , (39, "kolmekymmendä yhekšän")
      , (40, "neljjäkymmendä")
      , (41, "neljjäkymmendä yksi")
      , (42, "neljjäkymmendä kakši")
      , (43, "neljjäkymmendä kolme")
      , (44, "neljjäkymmendä neljjä")
      , (45, "neljjäkymmendä viizi")
      , (46, "neljjäkymmendä kuuǯi")
      , (47, "neljjäkymmendä seiččemän")
      , (48, "neljjäkymmendä kahekšan")
      , (49, "neljjäkymmendä yhekšän")
      , (50, "viizikymmendä")
      , (51, "viizikymmendä yksi")
      , (52, "viizikymmendä kakši")
      , (53, "viizikymmendä kolme")
      , (54, "viizikymmendä neljjä")
      , (55, "viizikymmendä viizi")
      , (56, "viizikymmendä kuuǯi")
      , (57, "viizikymmendä seiččemän")
      , (58, "viizikymmendä kahekšan")
      , (59, "viizikymmendä yhekšän")
      , (60, "kuuǯikymmendä")
      , (61, "kuuǯikymmendä yksi")
      , (62, "kuuǯikymmendä kakši")
      , (63, "kuuǯikymmendä kolme")
      , (64, "kuuǯikymmendä neljjä")
      , (65, "kuuǯikymmendä viizi")
      , (66, "kuuǯikymmendä kuuǯi")
      , (67, "kuuǯikymmendä seiččemän")
      , (68, "kuuǯikymmendä kahekšan")
      , (69, "kuuǯikymmendä yhekšän")
      , (70, "seiččemänkymmendä")
      , (71, "seiččemänkymmendä yksi")
      , (72, "seiččemänkymmendä kakši")
      , (73, "seiččemänkymmendä kolme")
      , (74, "seiččemänkymmendä neljjä")
      , (75, "seiččemänkymmendä viizi")
      , (76, "seiččemänkymmendä kuuǯi")
      , (77, "seiččemänkymmendä seiččemän")
      , (78, "seiččemänkymmendä kahekšan")
      , (79, "seiččemänkymmendä yhekšän")
      , (80, "kahekšankymmendä")
      , (81, "kahekšankymmendä yksi")
      , (82, "kahekšankymmendä kakši")
      , (83, "kahekšankymmendä kolme")
      , (84, "kahekšankymmendä neljjä")
      , (85, "kahekšankymmendä viizi")
      , (86, "kahekšankymmendä kuuǯi")
      , (87, "kahekšankymmendä seiččemän")
      , (88, "kahekšankymmendä kahekšan")
      , (89, "kahekšankymmendä yhekšän")
      , (90, "yhekšänkymmendä")
      , (91, "yhekšänkymmendä yksi")
      , (92, "yhekšänkymmendä kakši")
      , (93, "yhekšänkymmendä kolme")
      , (94, "yhekšänkymmendä neljjä")
      , (95, "yhekšänkymmendä viizi")
      , (96, "yhekšänkymmendä kuuǯi")
      , (97, "yhekšänkymmendä seiččemän")
      , (98, "yhekšänkymmendä kahekšan")
      , (99, "yhekšänkymmendä yhekšän")
      , (100, "šada")
      ]
    )
  ]
