{-|
[@ISO639-1@]        -

[@ISO639-2@]        min

[@ISO639-3@]        min

[@Native name@]     Baso Minangkabau, باسو مينڠكاباو

[@English name@]    Minangkabau
-}
module Text.Numeral.Language.MIN.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-minangkabau/en/min/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "ciek")
      , (2, "duo")
      , (3, "tigo")
      , (4, "ampek")
      , (5, "limo")
      , (6, "anam")
      , (7, "tujuah")
      , (8, "salapan")
      , (9, "sambilan")
      , (10, "sapuluah")
      , (11, "sabaleh")
      , (12, "duo baleh")
      , (13, "tigo baleh")
      , (14, "ampek baleh")
      , (15, "limo baleh")
      , (16, "anam baleh")
      , (17, "tujuah baleh")
      , (18, "salapan baleh")
      , (19, "sambilan baleh")
      , (20, "duo puluah")
      , (21, "duo puluah ciek")
      , (22, "duo puluah duo")
      , (23, "duo puluah tigo")
      , (24, "duo puluah ampek")
      , (25, "duo puluah limo")
      , (26, "duo puluah anam")
      , (27, "duo puluah tujuah")
      , (28, "duo puluah salapan")
      , (29, "duo puluah sambilan")
      , (30, "tigo puluah")
      , (31, "tigo puluah ciek")
      , (32, "tigo puluah duo")
      , (33, "tigo puluah tigo")
      , (34, "tigo puluah ampek")
      , (35, "tigo puluah limo")
      , (36, "tigo puluah anam")
      , (37, "tigo puluah tujuah")
      , (38, "tigo puluah salapan")
      , (39, "tigo puluah sambilan")
      , (40, "ampek puluah")
      , (41, "ampek puluah ciek")
      , (42, "ampek puluah duo")
      , (43, "ampek puluah tigo")
      , (44, "ampek puluah ampek")
      , (45, "ampek puluah limo")
      , (46, "ampek puluah anam")
      , (47, "ampek puluah tujuah")
      , (48, "ampek puluah salapan")
      , (49, "ampek puluah sambilan")
      , (50, "limo puluah")
      , (51, "limo puluah ciek")
      , (52, "limo puluah duo")
      , (53, "limo puluah tigo")
      , (54, "limo puluah ampek")
      , (55, "limo puluah limo")
      , (56, "limo puluah anam")
      , (57, "limo puluah tujuah")
      , (58, "limo puluah salapan")
      , (59, "limo puluah sambilan")
      , (60, "anam puluah")
      , (61, "anam puluah ciek")
      , (62, "anam puluah duo")
      , (63, "anam puluah tigo")
      , (64, "anam puluah ampek")
      , (65, "anam puluah limo")
      , (66, "anam puluah anam")
      , (67, "anam puluah tujuah")
      , (68, "anam puluah salapan")
      , (69, "anam puluah sambilan")
      , (70, "tujuah puluah")
      , (71, "tujuah puluah ciek")
      , (72, "tujuah puluah duo")
      , (73, "tujuah puluah tigo")
      , (74, "tujuah puluah ampek")
      , (75, "tujuah puluah limo")
      , (76, "tujuah puluah anam")
      , (77, "tujuah puluah tujuah")
      , (78, "tujuah puluah salapan")
      , (79, "tujuah puluah sambilan")
      , (80, "salapan puluah")
      , (81, "salapan puluah ciek")
      , (82, "salapan puluah duo")
      , (83, "salapan puluah tigo")
      , (84, "salapan puluah ampek")
      , (85, "salapan puluah limo")
      , (86, "salapan puluah anam")
      , (87, "salapan puluah tujuah")
      , (88, "salapan puluah salapan")
      , (89, "salapan puluah sambilan")
      , (90, "sambilan puluah")
      , (91, "sambilan puluah ciek")
      , (92, "sambilan puluah duo")
      , (93, "sambilan puluah tigo")
      , (94, "sambilan puluah ampek")
      , (95, "sambilan puluah limo")
      , (96, "sambilan puluah anam")
      , (97, "sambilan puluah tujuah")
      , (98, "sambilan puluah salapan")
      , (99, "sambilan puluah sambilan")
      , (100, "saratuih")
      ]
    )
  ]
