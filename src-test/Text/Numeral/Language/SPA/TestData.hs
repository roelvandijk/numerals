{-|
[@ISO639-1@]        es

[@ISO639-2B@]       spa

[@ISO639-3@]        spa

[@Native name@]     Español

[@English name@]    Spanish
-}

module Text.Numeral.Language.SPA.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude ( (+), Integral )
import "numerals" Text.Numeral.Misc ( dec )
import "numerals" Text.Numeral.Grammar
import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

-- Sources:
--   http://www.sf.airnet.ne.jp/~ts/language/number/spanish.html
--   http://spanish.about.com/cs/forbeginners/a/cardinalnum_beg.htm
--   http://www.learn-spanish-help.com/count-in-spanish.html
--   http://www.donquijote.org/spanishlanguage/numbers/numbers1.asp
--   http://en.wiktionary.org/wiki/Appendix:Spanish_numerals

cardinals :: (Integral i) => TestData i
cardinals =
  [ ( "neuter"
    , neuter defaultInflection
    , [ (-3, "menos tres")
      , (-2, "menos dos")
      , (-1, "menos uno")
      , (0, "cero")
      , (1, "uno")
      , (2, "dos")
      , (3, "tres")
      , (4, "cuatro")
      , (5, "cinco")
      , (6, "seis")
      , (7, "siete")
      , (8, "ocho")
      , (9, "nueve")
      , (10, "diez")
      , (11, "once")
      , (12, "doce")
      , (13, "trece")
      , (14, "catorce")
      , (15, "quince")
      , (16, "dieciséis")
      , (17, "diecisiete")
      , (18, "dieciocho")
      , (19, "diecinueve")
      , (20, "veinte")
      , (21, "veintiuno")
      , (22, "veintidós")
      , (23, "veintitrés")
      , (24, "veinticuatro")
      , (25, "veinticinco")
      , (26, "veintiséis")
      , (27, "veintisiete")
      , (28, "veintiocho")
      , (29, "veintinueve")
      , (30, "treinta")
      , (31, "treinta y uno")
      , (32, "treinta y dos")
      , (33, "treinta y tres")
      , (34, "treinta y cuatro")
      , (35, "treinta y cinco")
      , (36, "treinta y seis")
      , (37, "treinta y siete")
      , (38, "treinta y ocho")
      , (39, "treinta y nueve")
      , (40, "cuarenta")
      , (41, "cuarenta y uno")
      , (42, "cuarenta y dos")
      , (43, "cuarenta y tres")
      , (44, "cuarenta y cuatro")
      , (45, "cuarenta y cinco")
      , (46, "cuarenta y seis")
      , (47, "cuarenta y siete")
      , (48, "cuarenta y ocho")
      , (49, "cuarenta y nueve")
      , (50, "cincuenta")
      , (51, "cincuenta y uno")
      , (52, "cincuenta y dos")
      , (53, "cincuenta y tres")
      , (54, "cincuenta y cuatro")
      , (55, "cincuenta y cinco")
      , (56, "cincuenta y seis")
      , (57, "cincuenta y siete")
      , (58, "cincuenta y ocho")
      , (59, "cincuenta y nueve")
      , (60, "sesenta")
      , (61, "sesenta y uno")
      , (62, "sesenta y dos")
      , (63, "sesenta y tres")
      , (64, "sesenta y cuatro")
      , (65, "sesenta y cinco")
      , (66, "sesenta y seis")
      , (67, "sesenta y siete")
      , (68, "sesenta y ocho")
      , (69, "sesenta y nueve")
      , (70, "setenta")
      , (71, "setenta y uno")
      , (72, "setenta y dos")
      , (73, "setenta y tres")
      , (74, "setenta y cuatro")
      , (75, "setenta y cinco")
      , (76, "setenta y seis")
      , (77, "setenta y siete")
      , (78, "setenta y ocho")
      , (79, "setenta y nueve")
      , (80, "ochenta")
      , (81, "ochenta y uno")
      , (82, "ochenta y dos")
      , (83, "ochenta y tres")
      , (84, "ochenta y cuatro")
      , (85, "ochenta y cinco")
      , (86, "ochenta y seis")
      , (87, "ochenta y siete")
      , (88, "ochenta y ocho")
      , (89, "ochenta y nueve")
      , (90, "noventa")
      , (91, "noventa y uno")
      , (92, "noventa y dos")
      , (93, "noventa y tres")
      , (94, "noventa y cuatro")
      , (95, "noventa y cinco")
      , (96, "noventa y seis")
      , (97, "noventa y siete")
      , (98, "noventa y ocho")
      , (99, "noventa y nueve")
      , (100, "cien")
      , (101, "ciento uno")
      , (103, "ciento tres")
      , (110, "ciento diez")
      , (114, "ciento catorce")
      , (120, "ciento veinte")
      , (127, "ciento veintisiete")
      , (130, "ciento treinta")
      , (165, "ciento sesenta y cinco")
      , (198, "ciento noventa y ocho")
      , (199, "ciento noventa y nueve")
      , (200, "doscientos")
      , (210, "doscientos diez")
      , (220, "doscientos veinte")
      , (221, "doscientos veintiuno")
      , (256, "doscientos cincuenta y seis")
      , (300, "trescientos")
      , (310, "trescientos diez")
      , (400, "cuatrocientos")
      , (410, "cuatrocientos diez")
      , (496, "cuatrocientos noventa y seis")
      , (500, "quinientos")
      , (510, "quinientos diez")
      , (512, "quinientos doce")
      , (600, "seiscientos")
      , (610, "seiscientos diez")
      , (666, "seiscientos sesenta y seis")
      , (700, "setecientos")
      , (710, "setecientos diez")
      , (800, "ochocientos")
      , (810, "ochocientos diez")
      , (900, "novecientos")
      , (910, "novecientos diez")
      , (1000, "mil")
      , (1002, "mil dos")
      , (1010, "mil diez")
      , (1020, "mil veinte")
      , (1030, "mil treinta")
      , (1110, "mil ciento diez")
      , (1024, "mil veinticuatro")
      , (1729, "mil setecientos veintinueve")
      , (2000, "dos mil")
      , (2010, "dos mil diez")
      , (2020, "dos mil veinte")
      , (2120, "dos mil ciento veinte")
      , (2800, "dos mil ochocientos")
      , (3000, "tres mil")
      , (4000, "cuatro mil")
      , (5000, "cinco mil")
      , (6000, "seis mil")
      , (7000, "siete mil")
      , (8000, "ocho mil")
      , (9000, "nueve mil")
      , (dec 4, "diez mil")
      , (10010, "diez mil diez")
      , (10020, "diez mil veinte")
      , (10030, "diez mil treinta")
      , (15000, "quince mil")
      , (18000, "dieciocho mil")
      , (22000, "veintidós mil")
      , (28000, "veintiocho mil")
      , (37000, "treinta y siete mil")
      , (85000, "ochenta y cinco mil")
      , (dec 5, "cien mil")
      , (108000, "ciento ocho mil")
      , (160000, "ciento sesenta mil")
      , (585000, "quinientos ochenta y cinco mil")
      , (999000, "novecientos noventa y nueve mil")
      , (dec 6, "un millón")
      , (dec 6 + 1, "un millón uno")
      , (21 * dec 6 + 21, "veintiún millones veintiuno")
      , (500 * dec 6, "quinientos millones")
      , (521 * dec 6 + 1, "quinientos veintiún millones uno")
      , (dec 7, "diez millones")
      , (dec 8, "cien millones")
      , (dec 9, "mil millones")
      , (dec 10, "diez mil millones")
      , (dec 11, "cien mil millones")
      , (dec 12, "un billón")
      , (dec 18, "un trillón")
      , (dec 24, "un cuatrillón")
      , (15936535897932384626433832795, "quince mil novecientos treinta y seis cuatrillones quinientos treinta y cinco mil ochocientos noventa y siete trillones novecientos treinta y dos mil trescientos ochenta y cuatro billones seiscientos veintiséis mil cuatrocientos treinta y tres millones ochocientos treinta y dos mil setecientos noventa y cinco")
      , (dec 30, "un quintillón")
      , (31415926535897932384626433832795, "treinta y un quintillones cuatrocientos quince mil novecientos veintiséis cuatrillones quinientos treinta y cinco mil ochocientos noventa y siete trillones novecientos treinta y dos mil trescientos ochenta y cuatro billones seiscientos veintiséis mil cuatrocientos treinta y tres millones ochocientos treinta y dos mil setecientos noventa y cinco")
      , (dec 36, "un sextillón")
      , (dec 42, "un septillón")
      , (dec 48, "un octillón")
      , (dec 54, "un nonillón")
      , (dec 60, "un decillón")
      , (dec 66, "un undecillón")
      , (dec 72, "un duodecillón")
      , (dec 78, "un tredecillón")
      , (dec 84, "un cuatordecillón")
      , (dec 90, "un quindecillón")
      , (dec 96, "un sexdecillón")
      , (dec 102, "un septendecillón")
      , (dec 108, "un octodecillón")
      , (dec 114, "un novendecillón")
      , (dec 120, "un vigintillón")
      ]
    )
  , ( "feminine"
    , feminine defaultInflection
    , [ (1, "una")
      , (11, "once")
      , (21, "veintiuna")
      , (31, "treinta y una")
      , (41, "cuarenta y una")
      , (51, "cincuenta y una")
      , (61, "sesenta y una")
      , (71, "setenta y una")
      , (81, "ochenta y una")
      , (91, "noventa y una")
      , (100, "cien")
      , (101, "ciento una")
      , (200, "doscientas")
      , (221, "doscientas veintiuna")
      , (300, "trescientas")
      , (400, "cuatrocientas")
      , (500, "quinientas")
      , (600, "seiscientas")
      , (700, "setecientas")
      , (800, "ochocientas")
      , (900, "novecientas")
      , (dec 6, "un millón")
      , (dec 6 + 1, "un millón una")
      , (21 * dec 6 + 21, "veintiún millones veintiuna")
      ]
    )
  , ( "masculine"
    , masculine defaultInflection
    , [ (1, "un")
      , (11, "once")
      , (21, "veintiún")
      , (31, "treinta y un")
      , (41, "cuarenta y un")
      , (51, "cincuenta y un")
      , (61, "sesenta y un")
      , (71, "setenta y un")
      , (81, "ochenta y un")
      , (91, "noventa y un")
      , (100, "cien")
      , (101, "ciento un")
      , (200, "doscientos")
      , (221, "doscientos veintiún")
      , (dec 6, "un millón")
      , (dec 6 + 1, "un millón un")
      , (21 * dec 6 + 21, "veintiún millones veintiún")
      ]
    )
  ]
