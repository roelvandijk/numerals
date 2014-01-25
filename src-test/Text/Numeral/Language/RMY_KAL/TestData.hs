{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        -

[@Native name@]     -

[@English name@]    Kalderash Romani
-}
module Text.Numeral.Language.RMY_KAL.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-kalderash-romani/en/rmy-kal/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "núla")
      , (1, "jek")
      , (2, "duj")
      , (3, "trin")
      , (4, "štar")
      , (5, "panź")
      , (6, "šov")
      , (7, "jefta")
      , (8, "oxto")
      , (9, "iňa")
      , (10, "deš")
      , (11, "dešujek")
      , (12, "dešuduj")
      , (13, "dešutrin")
      , (14, "dešuštar")
      , (15, "dešupanź")
      , (16, "dešušov")
      , (17, "dešjefta")
      , (18, "dešoxto")
      , (19, "dešuiňa")
      , (20, "biš")
      , (21, "biš taj jek")
      , (22, "biš taj duj")
      , (23, "biš taj trin")
      , (24, "biš taj štar")
      , (25, "biš taj panź")
      , (26, "biš taj šov")
      , (27, "biš taj jefta")
      , (28, "biš taj oxto")
      , (29, "biš taj iňa")
      , (30, "tranda")
      , (31, "tranda taj jek")
      , (32, "tranda taj duj")
      , (33, "tranda taj trin")
      , (34, "tranda taj štar")
      , (35, "tranda taj panź")
      , (36, "tranda taj šov")
      , (37, "tranda taj jefta")
      , (38, "tranda taj oxto")
      , (39, "tranda taj iňa")
      , (40, "saranda")
      , (41, "saranda taj jek")
      , (42, "saranda taj duj")
      , (43, "saranda taj trin")
      , (44, "saranda taj štar")
      , (45, "saranda taj panź")
      , (46, "saranda taj šov")
      , (47, "saranda taj jefta")
      , (48, "saranda taj oxto")
      , (49, "saranda taj iňa")
      , (50, "pinda")
      , (51, "pinda taj jek")
      , (52, "pinda taj duj")
      , (53, "pinda taj trin")
      , (54, "pinda taj štar")
      , (55, "pinda taj panź")
      , (56, "pinda taj šov")
      , (57, "pinda taj jefta")
      , (58, "pinda taj oxto")
      , (59, "pinda taj iňa")
      , (60, "šovardeš")
      , (61, "šovardeš taj jek")
      , (62, "šovardeš taj duj")
      , (63, "šovardeš taj trin")
      , (64, "šovardeš taj štar")
      , (65, "šovardeš taj panź")
      , (66, "šovardeš taj šov")
      , (67, "šovardeš taj jefta")
      , (68, "šovardeš taj oxto")
      , (69, "šovardeš taj iňa")
      , (70, "jeftavardeš")
      , (71, "jeftavardeš taj jek")
      , (72, "jeftavardeš taj duj")
      , (73, "jeftavardeš taj trin")
      , (74, "jeftavardeš taj štar")
      , (75, "jeftavardeš taj panź")
      , (76, "jeftavardeš taj šov")
      , (77, "jeftavardeš taj jefta")
      , (78, "jeftavardeš taj oxto")
      , (79, "jeftavardeš taj iňa")
      , (80, "oxtovardeš")
      , (81, "oxtovardeš taj jek")
      , (82, "oxtovardeš taj duj")
      , (83, "oxtovardeš taj trin")
      , (84, "oxtovardeš taj štar")
      , (85, "oxtovardeš taj panź")
      , (86, "oxtovardeš taj šov")
      , (87, "oxtovardeš taj jefta")
      , (88, "oxtovardeš taj oxto")
      , (89, "oxtovardeš taj iňa")
      , (90, "iňavardeš")
      , (91, "iňavardeš taj jek")
      , (92, "iňavardeš taj duj")
      , (93, "iňavardeš taj trin")
      , (94, "iňavardeš taj štar")
      , (95, "iňavardeš taj panź")
      , (96, "iňavardeš taj šov")
      , (97, "iňavardeš taj jefta")
      , (98, "iňavardeš taj oxto")
      , (99, "iňavardeš taj iňa")
      , (100, "šêl")
      , (101, "šêl jek")
      , (102, "šêl duj")
      , (103, "šêl trin")
      , (104, "šêl štar")
      , (105, "šêl panź")
      , (106, "šêl šov")
      , (107, "šêl jefta")
      , (108, "šêl oxto")
      , (109, "šêl iňa")
      , (110, "šêl deš")
      , (123, "šêl biš taj trin")
      , (200, "duj šêla")
      , (300, "trin šêla")
      , (321, "trin šêla biš taj jek")
      , (400, "štar šêla")
      , (500, "panź šêla")
      , (600, "šov šêla")
      , (700, "jefta šêla")
      , (800, "oxto šêla")
      , (900, "iňa šêla")
      , (909, "iňa šêla iňa")
      , (990, "iňa šêla iňavardeš")
      , (999, "iňa šêla iňavardeš taj iňa")
      , (1000, "mija")
      , (1001, "mija jek")
      , (1008, "mija oxto")
      , (1234, "mija duj šêla tranda taj štar")
      , (2000, "duj miji")
      , (3000, "trin miji")
      , (4000, "štar miji")
      , (4321, "štar miji trin šêla biš taj jek")
      , (5000, "panź miji")
      , (6000, "šov miji")
      , (7000, "jefta miji")
      , (8000, "oxto miji")
      , (9000, "iňa miji")
      ]
    )
  ]
