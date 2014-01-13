{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        djk

[@Native name@]     Aukan

[@English name@]    Ndyuka
-}
module Text.Numeral.Language.DJK.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-aukan/en/djk/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "wan")
      , (2, "tu")
      , (3, "dii")
      , (4, "fo")
      , (5, "feifi")
      , (6, "sigisi")
      , (7, "seibin")
      , (8, "aitin")
      , (9, "neigin")
      , (10, "tin")
      , (11, "elufu")
      , (12, "twalufu")
      , (13, "tin na dii")
      , (14, "tin na fo")
      , (15, "tin na feifi")
      , (16, "tin na sigisi")
      , (17, "tin na seibin")
      , (18, "tin na aitin")
      , (19, "tin a neigin")
      , (20, "twenti")
      , (21, "twenti a wan")
      , (22, "twenti a tu")
      , (23, "twenti a dii")
      , (24, "twenti a fo")
      , (25, "twenti a feifi")
      , (26, "twenti a sigisi")
      , (27, "twenti a seibin")
      , (28, "twenti a aitin")
      , (29, "twenti a neigin")
      , (30, "diitenti")
      , (31, "diitenti a wan")
      , (32, "diitenti a tu")
      , (33, "diitenti a dii")
      , (34, "diitenti a fo")
      , (35, "diitenti a feifi")
      , (36, "diitenti a sigisi")
      , (37, "diitenti a seibin")
      , (38, "diitenti a aitin")
      , (39, "diitenti a neigin")
      , (40, "fotenti")
      , (41, "fotenti a wan")
      , (42, "fotenti a tu")
      , (43, "fotenti a dii")
      , (44, "fotenti a fo")
      , (45, "fotenti a feifi")
      , (46, "fotenti a sigisi")
      , (47, "fotenti a seibin")
      , (48, "fotenti a aitin")
      , (49, "fotenti a neigin")
      , (50, "feifitenti")
      , (51, "feifitenti a wan")
      , (52, "feifitenti a tu")
      , (53, "feifitenti a dii")
      , (54, "feifitenti a fo")
      , (55, "feifitenti a feifi")
      , (56, "feifitenti a sigisi")
      , (57, "feifitenti a seibin")
      , (58, "feifitenti a aitin")
      , (59, "feifitenti a neigin")
      , (60, "sigisitenti")
      , (61, "sigisitenti a wan")
      , (62, "sigisitenti a tu")
      , (63, "sigisitenti a dii")
      , (64, "sigisitenti a fo")
      , (65, "sigisitenti a feifi")
      , (66, "sigisitenti a sigisi")
      , (67, "sigisitenti a seibin")
      , (68, "sigisitenti a aitin")
      , (69, "sigisitenti a neigin")
      , (70, "seibintenti")
      , (71, "seibintenti a wan")
      , (72, "seibintenti a tu")
      , (73, "seibintenti a dii")
      , (74, "seibintenti a fo")
      , (75, "seibintenti a feifi")
      , (76, "seibintenti a sigisi")
      , (77, "seibintenti a seibin")
      , (78, "seibintenti a aitin")
      , (79, "seibintenti a neigin")
      , (80, "aitintenti")
      , (81, "aitintenti a wan")
      , (82, "aitintenti a tu")
      , (83, "aitintenti a dii")
      , (84, "aitintenti a fo")
      , (85, "aitintenti a feifi")
      , (86, "aitintenti a sigisi")
      , (87, "aitintenti a seibin")
      , (88, "aitintenti a aitin")
      , (89, "aitintenti a neigin")
      , (90, "neigintenti")
      , (91, "neigintenti a wan")
      , (92, "neigintenti a tu")
      , (93, "neigintenti a dii")
      , (94, "neigintenti a fo")
      , (95, "neigintenti a feifi")
      , (96, "neigintenti a sigisi")
      , (97, "neigintenti a seibin")
      , (98, "neigintenti a aitin")
      , (99, "neigintenti a neigin")
      , (100, "wan ondoo")
      , (101, "wan ondoo anga wan")
      , (102, "wan ondoo anga tu")
      , (103, "wan ondoo anga dii")
      , (104, "wan ondoo anga fo")
      , (105, "wan ondoo anga feifi")
      , (106, "wan ondoo anga sigisi")
      , (107, "wan ondoo anga seibin")
      , (108, "wan ondoo anga aitin")
      , (109, "wan ondoo anga neigin")
      , (110, "wan ondoo anga tin")
      , (123, "wan ondoo anga twenti a dii")
      , (200, "tu ondoo")
      , (300, "dii ondoo")
      , (321, "dii ondoo anga twenti a wan")
      , (400, "fo ondoo")
      , (500, "feifi ondoo")
      , (600, "sigisi ondoo")
      , (700, "seibin ondoo")
      , (800, "aitin ondoo")
      , (900, "neigin ondoo")
      , (909, "neigin ondoo anga neigin")
      , (990, "neigin ondoo anga neigintenti")
      , (999, "neigin ondoo anga neigintenti a neigin")
      , (1000, "wan dunsu")
      , (1001, "wan dunsu wan")
      , (1008, "wan dunsu aitin")
      , (1234, "wan dunsu tu ondoo anga anga diitenti a fo")
      , (2000, "tu dunsu")
      , (3000, "dii dunsu")
      , (4000, "fo dunsu")
      , (4321, "fo dunsu dii ondoo anga anga twenti a wan")
      , (5000, "feifi dunsu")
      , (6000, "sigisi dunsu")
      , (7000, "seibin dunsu")
      , (8000, "aitin dunsu")
      , (9000, "neigin dunsu")
      , (10000, "tin dunsu")
      , (12345, "twalufu dunsu dii ondoo anga anga fotenti a feifi")
      , (20000, "twenti dunsu")
      , (30000, "diitenti dunsu")
      , (40000, "fotenti dunsu")
      , (50000, "feifitenti dunsu")
      , (54321, "feifitenti a fo dunsu dii ondoo anga anga twenti a wan")
      , (60000, "sigisitenti dunsu")
      , (70000, "seibintenti dunsu")
      , (80000, "aitintenti dunsu")
      , (90000, "neigintenti dunsu")
      , (100000, "wan ondoo anga dunsu")
      , (123456, "wan ondoo anga twenti a dii dunsu fo ondoo anga anga feifitenti a sigisi")
      , (200000, "tu ondoo anga dunsu")
      , (300000, "dii ondoo anga dunsu")
      , (400000, "fo ondoo anga dunsu")
      , (500000, "feifi ondoo anga dunsu")
      , (600000, "sigisi ondoo anga dunsu")
      , (654321, "sigisi ondoo anga feifitenti a fo dunsu dii ondoo anga anga twenti a wan")
      , (700000, "seibin ondoo anga dunsu")
      , (800000, "aitin ondoo anga dunsu")
      , (900000, "neigin ondoo anga dunsu")
      , (1000000, "wan miliyun")
      ]
    )
  ]
