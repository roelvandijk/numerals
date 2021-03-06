{-|
[@ISO639-1@]        hu

[@ISO639-2@]        hun

[@ISO639-3@]        hun

[@Native name@]     magyar

[@English name@]    Hungarian
-}

module Text.Numeral.Language.HUN.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

cardinals :: (Integral i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "nulla")
      , (1, "egy")
      , (2, "kettö")
      , (3, "három")
      , (4, "négy")
      , (5, "öt")
      , (6, "hat")
      , (7, "hét")
      , (8, "nyolc")
      , (9, "kilenc")
      , (10, "tíz")
      , (11, "tízenegy")
      , (12, "tízenkettö")
      , (13, "tízenhárom")
      , (14, "tízennégy")
      , (15, "tízenöt")
      , (16, "tízenhat")
      , (17, "tízenhét")
      , (18, "tízennyolc")
      , (19, "tízenkilenc")
      , (20, "húsz")
      , (21, "húszonegy")
      , (22, "húszonkettö")
      , (23, "húszonhárom")
      , (24, "húszonnégy")
      , (25, "húszonöt")
      , (26, "húszonhat")
      , (27, "húszonhét")
      , (28, "húszonnyolc")
      , (29, "húszonkilenc")
      , (30, "harminc")
      , (31, "harmincegy")
      , (32, "harminckettö")
      , (33, "harminchárom")
      , (34, "harmincnégy")
      , (35, "harmincöt")
      , (36, "harminchat")
      , (37, "harminchét")
      , (38, "harmincnyolc")
      , (39, "harminckilenc")
      , (40, "negyven")
      , (41, "negyvenegy")
      , (42, "negyvenkettö")
      , (43, "negyvenhárom")
      , (44, "negyvennégy")
      , (45, "negyvenöt")
      , (46, "negyvenhat")
      , (47, "negyvenhét")
      , (48, "negyvennyolc")
      , (49, "negyvenkilenc")
      , (50, "ötven")
      , (51, "ötvenegy")
      , (52, "ötvenkettö")
      , (53, "ötvenhárom")
      , (54, "ötvennégy")
      , (55, "ötvenöt")
      , (56, "ötvenhat")
      , (57, "ötvenhét")
      , (58, "ötvennyolc")
      , (59, "ötvenkilenc")
      , (60, "hatvan")
      , (61, "hatvanegy")
      , (62, "hatvankettö")
      , (63, "hatvanhárom")
      , (64, "hatvannégy")
      , (65, "hatvanöt")
      , (66, "hatvanhat")
      , (67, "hatvanhét")
      , (68, "hatvannyolc")
      , (69, "hatvankilenc")
      , (70, "hetven")
      , (71, "hetvenegy")
      , (72, "hetvenkettö")
      , (73, "hetvenhárom")
      , (74, "hetvennégy")
      , (75, "hetvenöt")
      , (76, "hetvenhat")
      , (77, "hetvenhét")
      , (78, "hetvennyolc")
      , (79, "hetvenkilenc")
      , (80, "nyolcvan")
      , (81, "nyolcvanegy")
      , (82, "nyolcvankettö")
      , (83, "nyolcvanhárom")
      , (84, "nyolcvannégy")
      , (85, "nyolcvanöt")
      , (86, "nyolcvanhat")
      , (87, "nyolcvanhét")
      , (88, "nyolcvannyolc")
      , (89, "nyolcvankilenc")
      , (90, "kilencven")
      , (91, "kilencvenegy")
      , (92, "kilencvenkettö")
      , (93, "kilencvenhárom")
      , (94, "kilencvennégy")
      , (95, "kilencvenöt")
      , (96, "kilencvenhat")
      , (97, "kilencvenhét")
      , (98, "kilencvennyolc")
      , (99, "kilencvenkilenc")
      , (100, "száz")
      ]
    )
  ]
