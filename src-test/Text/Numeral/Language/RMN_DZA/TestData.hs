{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        rmn

[@Native name@]     -

[@English name@]    Balkan Romani
-}
module Text.Numeral.Language.RMN_DZA.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-dzambazi-romani/en/rmn-dza/
-}

-- Note: This is the Prilep dialect as spoken in Macedonia.
cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "jekh")
      , (2, "duj")
      , (3, "trin")
      , (4, "štar")
      , (5, "pandž")
      , (6, "šov")
      , (7, "efta")
      , (8, "oxto")
      , (9, "iňa")
      , (10, "deš")
      , (11, "dešujekh")
      , (12, "dešuduj")
      , (13, "dešutrin")
      , (14, "dešuštar")
      , (15, "dešupandž")
      , (16, "dešušov")
      , (17, "dešuefta")
      , (18, "dešuoxto")
      , (19, "dešuiňa")
      , (20, "biš")
      , (21, "bišthajekh")
      , (22, "bišthaduj")
      , (23, "bišthatrin")
      , (24, "bišthaštar")
      , (25, "bišthapandž")
      , (26, "bišthašov")
      , (27, "bišthajefta")
      , (28, "bišthajoxto")
      , (29, "bišthaiňa")
      , (30, "tranda")
      , (31, "trandathajekh")
      , (32, "trandathaduj")
      , (33, "trandathatrin")
      , (34, "trandathaštar")
      , (35, "trandathapandž")
      , (36, "trandathašov")
      , (37, "trandathajefta")
      , (38, "trandathajoxto")
      , (39, "trandathajiňa")
      , (40, "saranda")
      , (41, "sarandathajekh")
      , (42, "sarandathaduj")
      , (43, "sarandathatrin")
      , (44, "sarandathaštar")
      , (45, "sarandathapandž")
      , (46, "sarandathašov")
      , (47, "sarandathajefta")
      , (48, "sarandathajoxto")
      , (49, "sarandathajiňa")
      , (50, "pinda")
      , (51, "pindathajekh")
      , (52, "pindathaduj")
      , (53, "pindathatrin")
      , (54, "pindathaštar")
      , (55, "pindathapandž")
      , (56, "pindathašov")
      , (57, "pindathajefta")
      , (58, "pindathajoxto")
      , (59, "pindathajiňa")
      , (60, "šovardeš")
      , (61, "šovardešthajekh")
      , (62, "šovardešthaduj")
      , (63, "šovardešthatrin")
      , (64, "šovardešthaštar")
      , (65, "šovardešthapandž")
      , (66, "šovardešthašov")
      , (67, "šovardešthajefta")
      , (68, "šovardešthajoxto")
      , (69, "šovardešthajiňa")
      , (70, "eftavardeš")
      , (71, "eftavardešthajekh")
      , (72, "eftavardešthaduj")
      , (73, "eftavardešthatrin")
      , (74, "eftavardešthaštar")
      , (75, "eftavardešthapandž")
      , (76, "eftavardešthašov")
      , (77, "eftavardešthajefta")
      , (78, "eftavardešthajoxto")
      , (79, "eftavardešthajiňa")
      , (80, "oxtovardeš")
      , (81, "oxtovardešthajekh")
      , (82, "oxtovardešthaduj")
      , (83, "oxtovardešthatrin")
      , (84, "oxtovardešthaštar")
      , (85, "oxtovardešthapandž")
      , (86, "oxtovardešthašov")
      , (87, "oxtovardešthajefta")
      , (88, "oxtovardešthajoxto")
      , (89, "oxtovardešthajiňa")
      , (90, "iňavardeš")
      , (91, "iňavardešthajekh")
      , (92, "iňavardešthaduj")
      , (93, "iňavardešthatrin")
      , (94, "iňavardešthaštar")
      , (95, "iňavardešthapandž")
      , (96, "iňavardešthašov")
      , (97, "iňavardešthajefta")
      , (98, "iňavardešthajoxto")
      , (99, "iňavardešthajiňa")
      , (100, "šel")
      , (101, "šel jekh")
      , (102, "šel duj")
      , (103, "šel trin")
      , (104, "šel štar")
      , (105, "šel pandž")
      , (106, "šel šov")
      , (107, "šel efta")
      , (108, "šel oxto")
      , (109, "šel iňa")
      , (110, "šel deš")
      , (123, "šel bišthatrin")
      , (200, "duj šel")
      , (300, "trin šel")
      , (321, "trin šel bišthajekh")
      , (400, "štar šel")
      , (500, "panšel")
      , (600, "šov šel")
      , (700, "efta šel")
      , (800, "oxto šel")
      , (900, "iňa šel")
      , (909, "iňa šel iňa")
      , (990, "iňa šel iňavardeš")
      , (999, "iňa šel iňavardešthajiňa")
      , (1000, "miľa")
      , (1001, "miľa jekh")
      , (1008, "miľa oxto")
      , (1234, "miľa duj šel trandathaštar")
      , (2000, "duj miľe")
      , (3000, "trin miľe")
      , (4000, "štar miľe")
      , (4321, "štar miľe trin šel bišthajekh")
      , (5000, "pandž miľe")
      , (6000, "šov miľe")
      , (7000, "efta miľe")
      , (8000, "oxto miľe")
      , (9000, "iňa miľe")
      , (10000, "deš miľe")
      , (12345, "dešuduj miľe trin šel sarandathapandž")
      , (20000, "biš miľe")
      , (30000, "tranda miľe")
      , (40000, "saranda miľe")
      , (50000, "pinda miľe")
      , (54321, "pindathaštar miľe trin šel bišthajekh")
      , (60000, "šovardeš miľe")
      , (70000, "eftavardeš miľe")
      , (80000, "oxtovardeš miľe")
      , (90000, "iňavardeš miľe")
      , (100000, "šel miľe")
      , (123456, "šel bišthatrin miľe štar šel pindathašov")
      , (200000, "duj šel miľe")
      , (300000, "trin šel miľe")
      , (400000, "štar šel miľe")
      , (500000, "panšel miľe")
      , (600000, "šov šel miľe")
      , (654321, "šov šel pindathaštar miľe trin šel bišthajekh")
      , (700000, "efta šel miľe")
      , (800000, "oxto šel miľe")
      , (900000, "iňa šel miľe")
      , (1000000, "milioni")
      ]
    )
  ]
