{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        sq

[@ISO639-2B@]       alb

[@ISO639-2T@]       sqi

[@ISO639-3@]        sqi

[@Native name@]     shqip

[@English name@]    Albanian
-}
module Text.Numeral.Language.SQI.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-albanian/en/sqi/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "zero")
      , (1, "një")
      , (2, "dy")
      , (3, "tre")
      , (4, "katër")
      , (5, "pesë")
      , (6, "gjashtë")
      , (7, "shtatë")
      , (8, "tetë")
      , (9, "nëntë")
      , (10, "dhjetë")
      , (11, "njëmbëdhjetë")
      , (12, "dymbëdhjete")
      , (13, "trembëdhjetë")
      , (14, "katërmbëdhjetë")
      , (15, "pesëmbëdhjetë")
      , (16, "gjashtëmbëdhjetë")
      , (17, "shtatëmbëdhjetë")
      , (18, "tetëmbëdhjetë")
      , (19, "nëntëmbëdhjetë")
      , (20, "njëzet")
      , (21, "njëzet e një")
      , (22, "njëzet e dy")
      , (23, "njëzet e tre")
      , (24, "njëzet e katër")
      , (25, "njëzet e pesë")
      , (26, "njëzet e gjashtë")
      , (27, "njëzet e shtatë")
      , (28, "njëzet e tetë")
      , (29, "njëzet e nëntë")
      , (30, "tridhjetë")
      , (31, "tridhjetë e një")
      , (32, "tridhjetë e dy")
      , (33, "tridhjetë e tre")
      , (34, "tridhjetë e katër")
      , (35, "tridhjetë e pesë")
      , (36, "tridhjetë e gjashtë")
      , (37, "tridhjetë e shtatë")
      , (38, "tridhjetë e tetë")
      , (39, "tridhjetë e nëntë")
      , (40, "dyzet")
      , (41, "dyzet e një")
      , (42, "dyzet e dy")
      , (43, "dyzet e tre")
      , (44, "dyzet e katër")
      , (45, "dyzet e pesë")
      , (46, "dyzet e gjashtë")
      , (47, "dyzet e shtatë")
      , (48, "dyzet e tetë")
      , (49, "dyzet e nëntë")
      , (50, "pesëdhjetë")
      , (51, "pesëdhjetë e një")
      , (52, "pesëdhjetë e dy")
      , (53, "pesëdhjetë e tre")
      , (54, "pesëdhjetë e katër")
      , (55, "pesëdhjetë e pesë")
      , (56, "pesëdhjetë e gjashtë")
      , (57, "pesëdhjetë e shtatë")
      , (58, "pesëdhjetë e tetë")
      , (59, "pesëdhjetë e nëntë")
      , (60, "gjashtëdhjetë")
      , (61, "gjashtëdhjetë e një")
      , (62, "gjashtëdhjetë e dy")
      , (63, "gjashtëdhjetë e tre")
      , (64, "gjashtëdhjetë e katër")
      , (65, "gjashtëdhjetë e pesë")
      , (66, "gjashtëdhjetë e gjashtë")
      , (67, "gjashtëdhjetë e shtatë")
      , (68, "gjashtëdhjetë e tetë")
      , (69, "gjashtëdhjetë e nëntë")
      , (70, "shtatëdhjetë")
      , (71, "shtatëdhjetë e një")
      , (72, "shtatëdhjetë e dy")
      , (73, "shtatëdhjetë e tre")
      , (74, "shtatëdhjetë e katër")
      , (75, "shtatëdhjetë e pesë")
      , (76, "shtatëdhjetë e gjashtë")
      , (77, "shtatëdhjetë e shtatë")
      , (78, "shtatëdhjetë e tetë")
      , (79, "shtatëdhjetë e nëntë")
      , (80, "tetëdhjetë")
      , (81, "tetëdhjetë e një")
      , (82, "tetëdhjetë e dy")
      , (83, "tetëdhjetë e tre")
      , (84, "tetëdhjetë e katër")
      , (85, "tetëdhjetë e pesë")
      , (86, "tetëdhjetë e gjashtë")
      , (87, "tetëdhjetë e shtatë")
      , (88, "tetëdhjetë e tetë")
      , (89, "tetëdhjetë e nëntë")
      , (90, "nëntëdhjetë")
      , (91, "nëntëdhjetë e një")
      , (92, "nëntëdhjetë e dy")
      , (93, "nëntëdhjetë e tre")
      , (94, "nëntëdhjetë e katër")
      , (95, "nëntëdhjetë e pesë")
      , (96, "nëntëdhjetë e gjashtë")
      , (97, "nëntëdhjetë e shtatë")
      , (98, "nëntëdhjetë e tetë")
      , (99, "nëntëdhjetë e nëntë")
      , (100, "njëqind")
      , (101, "njëqind e një")
      , (102, "njëqind e dy")
      , (103, "njëqind e tre")
      , (104, "njëqind e katër")
      , (105, "njëqind e pesë")
      , (106, "njëqind e gjashtë")
      , (107, "njëqind e shtatë")
      , (108, "njëqind e tetë")
      , (109, "njëqind e nëntë")
      , (110, "njëqind e dhjetë")
      , (123, "njëqind e njëzet e tre")
      , (200, "dyqind")
      , (300, "treqind")
      , (321, "treqind e njëzet e një")
      , (400, "katërqind")
      , (500, "pesëqind")
      , (600, "gjashtëqind")
      , (700, "shtatëqind")
      , (800, "tetëqind")
      , (900, "nëntëqind")
      , (909, "nëntëqind e nëntë")
      , (990, "nëntëqind e nëntëdhjetë")
      , (999, "nëntëqind e nëntëdhjetë e nëntë")
      , (1000, "një mijë")
      , (1001, "një mijë një")
      , (1008, "një mijë tetë")
      , (1234, "një mijë e dyqind e tridhjetë e katër")
      , (2000, "dy mijë")
      , (3000, "tre mijë")
      , (4000, "katër mijë")
      , (4321, "katër mijë e treqind e njëzet e një")
      , (5000, "pesë mijë")
      , (6000, "gjashtë mijë")
      , (7000, "shtatë mijë")
      , (8000, "tetë mijë")
      , (9000, "nëntë mijë")
      , (10000, "dhjetë mijë")
      , (12345, "dymbëdhjete mijë e treqind e dyzet e pesë")
      , (20000, "njëzet mijë")
      , (30000, "tridhjetë mijë")
      , (40000, "dyzet mijë")
      , (50000, "pesëdhjetë mijë")
      , (54321, "pesëdhjetë e katër mijë e treqind e njëzet e një")
      , (60000, "gjashtëdhjetë mijë")
      , (70000, "shtatëdhjetë mijë")
      , (80000, "tetëdhjetë mijë")
      , (90000, "nëntëdhjetë mijë")
      , (100000, "njëqind mijë")
      , (123456, "njëqind e njëzet e tre mijë e katërqind e pesëdhjetë e gjashtë")
      , (200000, "dyqind mijë")
      , (300000, "treqind mijë")
      , (400000, "katërqind mijë")
      , (500000, "pesëqind mijë")
      , (600000, "gjashtëqind mijë")
      , (654321, "gjashtëqind e pesëdhjetë e katër mijë e treqind e njëzet e një")
      , (700000, "shtatëqind mijë")
      , (800000, "tetëqind mijë")
      , (900000, "nëntëqind mijë")
      , (1000000, "një milion")
      , (1000000000, "një miliard")
      , (1000000000000, "një bilion")
      , (1000000000000000, "një trilion")
      ]
    )
  ]
