{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        lt

[@ISO639-2@]        lit

[@ISO639-3@]        lit

[@Native name@]     lietuvių kalba

[@English name@]    Lithuanian
-}
module Text.Numeral.Language.LIT.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude ( Integral, (+) )
import "base-unicode-symbols" Prelude.Unicode ( (⋅) )
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-lithuanian/en/lit/
-}

cardinals ∷ (Integral i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "nulis")
      , (1, "vienas")
      , (2, "du")
      , (3, "trys")
      , (4, "keturi")
      , (5, "penki")
      , (6, "šeši")
      , (7, "septyni")
      , (8, "aštuoni")
      , (9, "devyni")
      , (10, "dešimt")
      , (11, "vienuolika")
      , (12, "dvylika")
      , (13, "trylika")
      , (14, "keturiolika")
      , (15, "penkiolika")
      , (16, "šešiolika")
      , (17, "septyniolika")
      , (18, "aštuoniolika")
      , (19, "devyniolika")
      , (20, "dvidešimt")
      , (21, "dvidešimt vienas")
      , (22, "dvidešimt du")
      , (23, "dvidešimt trys")
      , (24, "dvidešimt keturi")
      , (25, "dvidešimt penki")
      , (26, "dvidešimt šeši")
      , (27, "dvidešimt septyni")
      , (28, "dvidešimt aštuoni")
      , (29, "dvidešimt devyni")
      , (30, "trisdešimt")
      , (31, "trisdešimt vienas")
      , (32, "trisdešimt du")
      , (33, "trisdešimt trys")
      , (34, "trisdešimt keturi")
      , (35, "trisdešimt penki")
      , (36, "trisdešimt šeši")
      , (37, "trisdešimt septyni")
      , (38, "trisdešimt aštuoni")
      , (39, "trisdešimt devyni")
      , (40, "keturiasdešimt")
      , (41, "keturiasdešimt vienas")
      , (42, "keturiasdešimt du")
      , (43, "keturiasdešimt trys")
      , (44, "keturiasdešimt keturi")
      , (45, "keturiasdešimt penki")
      , (46, "keturiasdešimt šeši")
      , (47, "keturiasdešimt septyni")
      , (48, "keturiasdešimt aštuoni")
      , (49, "keturiasdešimt devyni")
      , (50, "penkiasdešimt")
      , (51, "penkiasdešimt vienas")
      , (52, "penkiasdešimt du")
      , (53, "penkiasdešimt trys")
      , (54, "penkiasdešimt keturi")
      , (55, "penkiasdešimt penki")
      , (56, "penkiasdešimt šeši")
      , (57, "penkiasdešimt septyni")
      , (58, "penkiasdešimt aštuoni")
      , (59, "penkiasdešimt devyni")
      , (60, "šešiasdešimt")
      , (61, "šešiasdešimt vienas")
      , (62, "šešiasdešimt du")
      , (63, "šešiasdešimt trys")
      , (64, "šešiasdešimt keturi")
      , (65, "šešiasdešimt penki")
      , (66, "šešiasdešimt šeši")
      , (67, "šešiasdešimt septyni")
      , (68, "šešiasdešimt aštuoni")
      , (69, "šešiasdešimt devyni")
      , (70, "septyniasdešimt")
      , (71, "septyniasdešimt vienas")
      , (72, "septyniasdešimt du")
      , (73, "septyniasdešimt trys")
      , (74, "septyniasdešimt keturi")
      , (75, "septyniasdešimt penki")
      , (76, "septyniasdešimt šeši")
      , (77, "septyniasdešimt septyni")
      , (78, "septyniasdešimt aštuoni")
      , (79, "septyniasdešimt devyni")
      , (80, "aštuoniasdešimt")
      , (81, "aštuoniasdešimt vienas")
      , (82, "aštuoniasdešimt du")
      , (83, "aštuoniasdešimt trys")
      , (84, "aštuoniasdešimt keturi")
      , (85, "aštuoniasdešimt penki")
      , (86, "aštuoniasdešimt šeši")
      , (87, "aštuoniasdešimt septyni")
      , (88, "aštuoniasdešimt aštuoni")
      , (89, "aštuoniasdešimt devyni")
      , (90, "devyniasdešimt")
      , (91, "devyniasdešimt vienas")
      , (92, "devyniasdešimt du")
      , (93, "devyniasdešimt trys")
      , (94, "devyniasdešimt keturi")
      , (95, "devyniasdešimt penki")
      , (96, "devyniasdešimt šeši")
      , (97, "devyniasdešimt septyni")
      , (98, "devyniasdešimt aštuoni")
      , (99, "devyniasdešimt devyni")
      , (100, "šimtas")
      , (101, "šimtas vienas")
      , (102, "šimtas du")
      , (103, "šimtas trys")
      , (104, "šimtas keturi")
      , (105, "šimtas penki")
      , (106, "šimtas šeši")
      , (107, "šimtas septyni")
      , (108, "šimtas aštuoni")
      , (109, "šimtas devyni")
      , (110, "šimtas dešimt")
      , (123, "šimtas dvidešimt trys")
      , (200, "du šimtai")
      , (300, "trys šimtai")
      , (321, "trys šimtai dvidešimt vienas")
      , (400, "keturi šimtai")
      , (500, "penki šimtai")
      , (600, "šeši šimtai")
      , (700, "septyni šimtai")
      , (800, "aštuoni šimtai")
      , (900, "devyni šimtai")
      , (909, "devyni šimtai devyni")
      , (990, "devyni šimtai devyniasdešimt")
      , (999, "devyni šimtai devyniasdešimt devyni")
      , (1000, "tūkstantis")
      , (1001, "tūkstantis vienas")
      , (1008, "tūkstantis aštuoni")
      , (1234, "tūkstantis du šimtai trisdešimt keturi")
      , (2000, "du tūkstančiai")
      , (3000, "trys tūkstančiai")
      , (4000, "keturi tūkstančiai")
      , (4321, "keturi tūkstančiai trys šimtai dvidešimt vienas")
      , (5000, "penki tūkstančiai")
      , (6000, "šeši tūkstančiai")
      , (7000, "septyni tūkstančiai")
      , (8000, "aštuoni tūkstančiai")
      , (9000, "devyni tūkstančiai")
      , (10000, "dešimt tūkstančiai")
      , (12345, "dvylika tūkstančiai trys šimtai keturiasdešimt penki")
      , (20000, "dvidešimt tūkstančiai")
      , (30000, "trisdešimt tūkstančiai")
      , (40000, "keturiasdešimt tūkstančiai")
      , (50000, "penkiasdešimt tūkstančiai")
      , (54321, "penkiasdešimt keturi tūkstančiai trys šimtai dvidešimt vienas")
      , (60000, "šešiasdešimt tūkstančiai")
      , (70000, "septyniasdešimt tūkstančiai")
      , (80000, "aštuoniasdešimt tūkstančiai")
      , (90000, "devyniasdešimt tūkstančiai")
      , (100000, "šimtas tūkstančiai")
      , (123456, "šimtas dvidešimt trys tūkstančiai keturi šimtai penkiasdešimt šeši")
      , (200000, "du šimtai tūkstančiai")
      , (300000, "trys šimtai tūkstančiai")
      , (400000, "keturi šimtai tūkstančiai")
      , (500000, "penki šimtai tūkstančiai")
      , (600000, "šeši šimtai tūkstančiai")
      , (654321, "šeši šimtai penkiasdešimt keturi tūkstančiai trys šimtai dvidešimt vienas")
      , (700000, "septyni šimtai tūkstančiai")
      , (800000, "aštuoni šimtai tūkstančiai")
      , (900000, "devyni šimtai tūkstančiai")
      , (1000000, "milijonas")
      , (1000001, "milijonas vienas")
      , (1234567, "milijonas du šimtai trisdešimt keturi tūkstančiai penki šimtai šešiasdešimt septyni")
      , (2000000, "du milijonai")
      , (3000000, "trys milijonai")
      , (4000000, "keturi milijonai")
      , (5000000, "penki milijonai")
      , (6000000, "šeši milijonai")
      , (7000000, "septyni milijonai")
      , (7654321, "septyni milijonai šeši šimtai penkiasdešimt keturi tūkstančiai trys šimtai dvidešimt vienas")
      , (8000000, "aštuoni milijonai")
      , (9000000, "devyni milijonai")
      , (dec 9, "milijardas")
      , (dec 9 + 1, "milijardas vienas")
      , (2 ⋅ dec 9, "du milijardai")
      , (3 ⋅ dec 9, "trys milijardai")
      , (4 ⋅ dec 9, "keturi milijardai")
      , (5 ⋅ dec 9, "penki milijardai")
      , (6 ⋅ dec 9, "šeši milijardai")
      , (7 ⋅ dec 9, "septyni milijardai")
      , (8 ⋅ dec 9, "aštuoni milijardai")
      , (9 ⋅ dec 9, "devyni milijardai")
      , (dec 12, "trilijonas")
      , (dec 12 + 1, "trilijonas vienas")
      , (2 ⋅ dec 12, "du trilijonai")
      , (3 ⋅ dec 12, "trys trilijonai")
      , (4 ⋅ dec 12, "keturi trilijonai")
      , (5 ⋅ dec 12, "penki trilijonai")
      , (6 ⋅ dec 12, "šeši trilijonai")
      , (7 ⋅ dec 12, "septyni trilijonai")
      , (8 ⋅ dec 12, "aštuoni trilijonai")
      , (9 ⋅ dec 12, "devyni trilijonai")
      , (dec 15, "kvadrilijonas")
      , (dec 15 + 1, "kvadrilijonas vienas")
      , (2 ⋅ dec 15, "du kvadrilijonai")
      , (3 ⋅ dec 15, "trys kvadrilijonai")
      , (4 ⋅ dec 15, "keturi kvadrilijonai")
      , (5 ⋅ dec 15, "penki kvadrilijonai")
      , (6 ⋅ dec 15, "šeši kvadrilijonai")
      , (7 ⋅ dec 15, "septyni kvadrilijonai")
      , (8 ⋅ dec 15, "aštuoni kvadrilijonai")
      , (9 ⋅ dec 15, "devyni kvadrilijonai")
      , (dec 18, "kvintilijonas")
      , (dec 18 + 1, "kvintilijonas vienas")
      , (2 ⋅ dec 18, "du kvintilijonai")
      , (3 ⋅ dec 18, "trys kvintilijonai")
      , (4 ⋅ dec 18, "keturi kvintilijonai")
      , (5 ⋅ dec 18, "penki kvintilijonai")
      , (6 ⋅ dec 18, "šeši kvintilijonai")
      , (7 ⋅ dec 18, "septyni kvintilijonai")
      , (8 ⋅ dec 18, "aštuoni kvintilijonai")
      , (9 ⋅ dec 18, "devyni kvintilijonai")
      , (dec 21, "sikstilijonas")
      , (dec 21 + 1, "sikstilijonas vienas")
      , (2 ⋅ dec 21, "du sikstilijonai")
      , (3 ⋅ dec 21, "trys sikstilijonai")
      , (4 ⋅ dec 21, "keturi sikstilijonai")
      , (5 ⋅ dec 21, "penki sikstilijonai")
      , (6 ⋅ dec 21, "šeši sikstilijonai")
      , (7 ⋅ dec 21, "septyni sikstilijonai")
      , (8 ⋅ dec 21, "aštuoni sikstilijonai")
      , (9 ⋅ dec 21, "devyni sikstilijonai")
      , (dec 24, "septilijonas")
      , (dec 24 + 1, "septilijonas vienas")
      , (2 ⋅ dec 24, "du septilijonai")
      , (3 ⋅ dec 24, "trys septilijonai")
      , (4 ⋅ dec 24, "keturi septilijonai")
      , (5 ⋅ dec 24, "penki septilijonai")
      , (6 ⋅ dec 24, "šeši septilijonai")
      , (7 ⋅ dec 24, "septyni septilijonai")
      , (8 ⋅ dec 24, "aštuoni septilijonai")
      , (9 ⋅ dec 24, "devyni septilijonai")
      , (dec 27, "oktilijonas")
      , (dec 27 + 1, "oktilijonas vienas")
      , (2 ⋅ dec 27, "du oktilijonai")
      , (3 ⋅ dec 27, "trys oktilijonai")
      , (4 ⋅ dec 27, "keturi oktilijonai")
      , (5 ⋅ dec 27, "penki oktilijonai")
      , (6 ⋅ dec 27, "šeši oktilijonai")
      , (7 ⋅ dec 27, "septyni oktilijonai")
      , (8 ⋅ dec 27, "aštuoni oktilijonai")
      , (9 ⋅ dec 27, "devyni oktilijonai")
      , (dec 30, "naintilijonas")
      , (dec 30 + 1, "naintilijonas vienas")
      , (2 ⋅ dec 30, "du naintilijonai")
      , (3 ⋅ dec 30, "trys naintilijonai")
      , (4 ⋅ dec 30, "keturi naintilijonai")
      , (5 ⋅ dec 30, "penki naintilijonai")
      , (6 ⋅ dec 30, "šeši naintilijonai")
      , (7 ⋅ dec 30, "septyni naintilijonai")
      , (8 ⋅ dec 30, "aštuoni naintilijonai")
      , (9 ⋅ dec 30, "devyni naintilijonai")
      ]
    )
  ]