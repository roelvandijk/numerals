{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        pl

[@ISO639-2@]        pol

[@ISO639-3@]        pol

[@Native name@]     język polski

[@English name@]    Polish
-}

module Text.Numeral.Language.PL.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "base-unicode-symbols" Prelude.Unicode ( (⋅) )
import "numerals-base" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals-base" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

-- Sources:
--   Krzysztof Skrzętnicki
--   http://www.polishforums.com/archives/2009/general-language-17/numbers-polish-language-6722/

cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "zero")
      , (1, "jeden")
      , (2, "dwa")
      , (3, "trzy")
      , (4, "cztery")
      , (5, "pięć")
      , (6, "sześć")
      , (7, "siedem")
      , (8, "osiem")
      , (9, "dziewięć")
      , (10, "dziesięć")
      , (11, "jedenaście")
      , (12, "dwanaście")
      , (13, "trzynaście")
      , (14, "czternaście")
      , (15, "piętnaście")
      , (16, "szesnaście")
      , (17, "siedemnaście")
      , (18, "osiemnaście")
      , (19, "dziewiętnaście")
      , (20, "dwadzieścia")
      , (21, "dwadzieścia jeden")
      , (22, "dwadzieścia dwa")
      , (23, "dwadzieścia trzy")
      , (24, "dwadzieścia cztery")
      , (25, "dwadzieścia pięć")
      , (26, "dwadzieścia sześć")
      , (27, "dwadzieścia siedem")
      , (28, "dwadzieścia osiem")
      , (29, "dwadzieścia dziewięć")
      , (30, "trzydzieści")
      , (31, "trzydzieści jeden")
      , (32, "trzydzieści dwa")
      , (33, "trzydzieści trzy")
      , (34, "trzydzieści cztery")
      , (35, "trzydzieści pięć")
      , (36, "trzydzieści sześć")
      , (37, "trzydzieści siedem")
      , (38, "trzydzieści osiem")
      , (39, "trzydzieści dziewięć")
      , (40, "czterdzieści")
      , (41, "czterdzieści jeden")
      , (42, "czterdzieści dwa")
      , (43, "czterdzieści trzy")
      , (44, "czterdzieści cztery")
      , (45, "czterdzieści pięć")
      , (46, "czterdzieści sześć")
      , (47, "czterdzieści siedem")
      , (48, "czterdzieści osiem")
      , (49, "czterdzieści dziewięć")
      , (50, "pięćdziesiąt")
      , (51, "pięćdziesiąt jeden")
      , (52, "pięćdziesiąt dwa")
      , (53, "pięćdziesiąt trzy")
      , (54, "pięćdziesiąt cztery")
      , (55, "pięćdziesiąt pięć")
      , (56, "pięćdziesiąt sześć")
      , (57, "pięćdziesiąt siedem")
      , (58, "pięćdziesiąt osiem")
      , (59, "pięćdziesiąt dziewięć")
      , (60, "sześćdziesiąt")
      , (61, "sześćdziesiąt jeden")
      , (62, "sześćdziesiąt dwa")
      , (63, "sześćdziesiąt trzy")
      , (64, "sześćdziesiąt cztery")
      , (65, "sześćdziesiąt pięć")
      , (66, "sześćdziesiąt sześć")
      , (67, "sześćdziesiąt siedem")
      , (68, "sześćdziesiąt osiem")
      , (69, "sześćdziesiąt dziewięć")
      , (70, "siedemdziesiąt")
      , (71, "siedemdziesiąt jeden")
      , (72, "siedemdziesiąt dwa")
      , (73, "siedemdziesiąt trzy")
      , (74, "siedemdziesiąt cztery")
      , (75, "siedemdziesiąt pięć")
      , (76, "siedemdziesiąt sześć")
      , (77, "siedemdziesiąt siedem")
      , (78, "siedemdziesiąt osiem")
      , (79, "siedemdziesiąt dziewięć")
      , (80, "osiemdziesiąt")
      , (81, "osiemdziesiąt jeden")
      , (82, "osiemdziesiąt dwa")
      , (83, "osiemdziesiąt trzy")
      , (84, "osiemdziesiąt cztery")
      , (85, "osiemdziesiąt pięć")
      , (86, "osiemdziesiąt sześć")
      , (87, "osiemdziesiąt siedem")
      , (88, "osiemdziesiąt osiem")
      , (89, "osiemdziesiąt dziewięć")
      , (90, "dziewięćdziesiąt")
      , (91, "dziewięćdziesiąt jeden")
      , (92, "dziewięćdziesiąt dwa")
      , (93, "dziewięćdziesiąt trzy")
      , (94, "dziewięćdziesiąt cztery")
      , (95, "dziewięćdziesiąt pięć")
      , (96, "dziewięćdziesiąt sześć")
      , (97, "dziewięćdziesiąt siedem")
      , (98, "dziewięćdziesiąt osiem")
      , (99, "dziewięćdziesiąt dziewięć")
      , (100, "sto")
      , (101, "sto jeden")
      , (110, "sto dziesięć")
      , (111, "sto jedenaście")
      , (120, "sto dwadzieścia")
      , (121, "sto dwadzieścia jeden")
      , (144, "sto czterdzieści cztery")
      , (200, "dwieście")
      , (300, "trzysta")
      , (400, "czterysta")
      , (500, "pięćset")
      , (600, "sześćset")
      , (700, "siedemset")
      , (777, "siedemset siedemdziesiąt siedem")
      , (800, "osiemset")
      , (900, "dziewięćset")
      , (1000, "tysiąc")
      , (1001, "tysiąc jeden")
      , (1010, "tysiąc dziesięć")
      , (1011, "tysiąc jedenaście")
      , (1024, "tysiąc dwadzieścia cztery")
      , (1100, "tysiąc sto")
      , (1728, "tysiąc siedemset dwadzieścia osiem")
      , (2000, "dwa tysiące")
      , (3000, "trzy tysiące")
      , (4000, "cztery tysiące")
      , (5000, "pięć tysięcy")
      , (6000, "sześć tysięcy")
      , (7000, "siedem tysięcy")
      , (8000, "osiem tysięcy")
      , (9000, "dziewięć tysięcy")
      , (10000, "dziesięć tysięcy")
      , (100000, "sto tysięcy")
      , (500000, "pięćset tysięcy")
      , (1000000, "milion")
      , (1048576, "milion czterdzieści osiem tysięcy pięćset siedemdziesiąt sześć")
      , (2 ⋅ dec 6, "dwa miliony")
      , (3 ⋅ dec 6, "trzy miliony")
      , (4 ⋅ dec 6, "cztery miliony")
      , (5 ⋅ dec 6, "pięć milionów")
      , (6 ⋅ dec 6, "sześć milionów")
      , (7 ⋅ dec 6, "siedem milionów")
      , (8 ⋅ dec 6, "osiem milionów")
      , (9 ⋅ dec 6, "dziewięć milionów")
      , (dec 7, "dziesięć milionów")
      , (dec 8, "sto milionów")
      , (dec 9, "miliard")
      , (2 ⋅ dec 9, "dwa miliardy")
      , (3 ⋅ dec 9, "trzy miliardy")
      , (4 ⋅ dec 9, "cztery miliardy")
      , (5 ⋅ dec 9, "pięć miliardów")
      , (6 ⋅ dec 9, "sześć miliardów")
      , (7 ⋅ dec 9, "siedem miliardów")
      , (8 ⋅ dec 9, "osiem miliardów")
      , (9 ⋅ dec 9, "dziewięć miliardów")
      , (dec 10, "dziesięć miliardów")
      , (dec 11, "sto miliardów")
      ]
    )
  ]
