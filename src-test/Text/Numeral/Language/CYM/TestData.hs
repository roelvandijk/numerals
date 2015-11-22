{-|
[@ISO639-1@]        cy

[@ISO639-2B@]       wel

[@ISO639-2T@]       cym

[@ISO639-3@]        cym

[@Native name@]     Cymraeg

[@English name@]    Welsh
-}

module Text.Numeral.Language.CYM.TestData (cardinals, ordinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "numerals" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

-- Sources:
--   http://mylanguages.org/welsh_numbers.php

cardinals :: (Integral i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "dim")
      , (1, "un")
      , (2, "dau")
      , (3, "tri")
      , (4, "pedwar")
      , (5, "pump")
      , (6, "chwech")
      , (7, "saith")
      , (8, "wyth")
      , (9, "naw")
      , (10, "deg")
      , (11, "un deg un")
      , (12, "un deg dau")
      , (13, "un deg tri")
      , (14, "un deg pedwar")
      , (15, "un deg pump")
      , (16, "un deg chwech")
      , (17, "un deg saith")
      , (18, "un deg wyth")
      , (19, "un deg naw")
      , (20, "dau deg")
      , (21, "dau deg un")
      , (22, "dau deg dau")
      , (23, "dau deg tri")
      , (24, "dau deg pedwar")
      , (25, "dau deg pump")
      , (26, "dau deg chwech")
      , (27, "dau deg saith")
      , (28, "dau deg wyth")
      , (29, "dau deg naw")
      , (30, "tri deg")
      , (31, "tri deg un")
      , (32, "tri deg dau")
      , (33, "tri deg tri")
      , (34, "tri deg pedwar")
      , (35, "tri deg pump")
      , (36, "tri deg chwech")
      , (37, "tri deg saith")
      , (38, "tri deg wyth")
      , (39, "tri deg naw")
      , (40, "pedwar deg")
      , (41, "pedwar deg un")
      , (42, "pedwar deg dau")
      , (43, "pedwar deg tri")
      , (44, "pedwar deg pedwar")
      , (45, "pedwar deg pump")
      , (46, "pedwar deg chwech")
      , (47, "pedwar deg saith")
      , (48, "pedwar deg wyth")
      , (49, "pedwar deg naw")
      , (50, "pum deg")
      , (51, "pum deg un")
      , (52, "pum deg dau")
      , (53, "pum deg tri")
      , (54, "pum deg pedwar")
      , (55, "pum deg pump")
      , (56, "pum deg chwech")
      , (57, "pum deg saith")
      , (58, "pum deg wyth")
      , (59, "pum deg naw")
      , (60, "chwe deg")
      , (61, "chwe deg un")
      , (62, "chwe deg dau")
      , (63, "chwe deg tri")
      , (64, "chwe deg pedwar")
      , (65, "chwe deg pump")
      , (66, "chwe deg chwech")
      , (67, "chwe deg saith")
      , (68, "chwe deg wyth")
      , (69, "chwe deg naw")
      , (70, "saith deg")
      , (71, "saith deg un")
      , (72, "saith deg dau")
      , (73, "saith deg tri")
      , (74, "saith deg pedwar")
      , (75, "saith deg pump")
      , (76, "saith deg chwech")
      , (77, "saith deg saith")
      , (78, "saith deg wyth")
      , (79, "saith deg naw")
      , (80, "wyth deg")
      , (81, "wyth deg un")
      , (82, "wyth deg dau")
      , (83, "wyth deg tri")
      , (84, "wyth deg pedwar")
      , (85, "wyth deg pump")
      , (86, "wyth deg chwech")
      , (87, "wyth deg saith")
      , (88, "wyth deg wyth")
      , (89, "wyth deg naw")
      , (90, "naw deg")
      , (91, "naw deg un")
      , (92, "naw deg dau")
      , (93, "naw deg tri")
      , (94, "naw deg pedwar")
      , (95, "naw deg pump")
      , (96, "naw deg chwech")
      , (97, "naw deg saith")
      , (98, "naw deg wyth")
      , (99, "naw deg naw")
      , (100, "cant")
      , (1000, "un thousand")
      , (dec 6, "miliwn")
      ]
    )
  ]

ordinals :: (Integral i) => TestData i
ordinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "gyntaf")
      , (2, "ail")
      , (3, "trydydd")
      , (4, "pedwerydd")
      , (5, "pumed")
      , (6, "chweched")
      , (7, "seithfed")
      , (8, "wythfed")
      , (9, "nawfed")
      , (10, "degfed")
      , (11, "unfed ar ddeg")
      , (12, "deuddegfed")
      , (13, "drydedd ar ddeg")
      , (14, "ddeg")
      , (15, "pymthegfed")
      , (16, "ar bymtheg")
      , (17, "ail ar bymtheg")
      , (18, "deunawfed")
      , (19, "bedwaredd ganrif ar bymtheg")
      , (20, "ugeinfed")
      ]
    )
  ]
