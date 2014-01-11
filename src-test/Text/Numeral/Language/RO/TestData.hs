{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        ro

[@ISO639-2B@]       rum

[@ISO639-2T@]       ron

[@ISO639-3@]        ron

[@Native name@]     română

[@English name@]    Romanian
-}

module Text.Numeral.Language.RO.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "this" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "zero")
      , (1, "unu")
      , (2, "doi")
      , (3, "trei")
      , (4, "patru")
      , (5, "cinci")
      , (6, "sase")
      , (7, "sapte")
      , (8, "opt")
      , (9, "noua")
      , (10, "zece")
      , (11, "unsprezece")
      , (12, "doisprezece")
      , (13, "treisprezece")
      , (14, "paisprezece")
      , (15, "cincisprezece")
      , (16, "saisprezece")
      , (17, "saptesprezece")
      , (18, "optsprezece")
      , (19, "nouasprezece")
      , (20, "douazeci")
      , (21, "douazecisiunu")
      , (22, "douazecisidoi")
      , (23, "douazecisitrei")
      , (24, "douazecisipatru")
      , (25, "douazecisicinci")
      , (26, "douazecisisase")
      , (27, "douazecisisapte")
      , (28, "douazecisiopt")
      , (29, "douazecisinoua")
      , (30, "treizeci")
      , (31, "treizecisiunu")
      , (32, "treizecisidoi")
      , (33, "treizecisitrei")
      , (34, "treizecisipatru")
      , (35, "treizecisicinci")
      , (36, "treizecisisase")
      , (37, "treizecisisapte")
      , (38, "treizecisiopt")
      , (39, "treizecisinoua")
      , (40, "patruzeci")
      , (41, "patruzecisiunu")
      , (42, "patruzecisidoi")
      , (43, "patruzecisitrei")
      , (44, "patruzecisipatru")
      , (45, "patruzecisicinci")
      , (46, "patruzecisisase")
      , (47, "patruzecisisapte")
      , (48, "patruzecisiopt")
      , (49, "patruzecisinoua")
      , (50, "cincizeci")
      , (51, "cincizecisiunu")
      , (52, "cincizecisidoi")
      , (53, "cincizecisitrei")
      , (54, "cincizecisipatru")
      , (55, "cincizecisicinci")
      , (56, "cincizecisisase")
      , (57, "cincizecisisapte")
      , (58, "cincizecisiopt")
      , (59, "cincizecisinoua")
      , (60, "saizeci")
      , (61, "saizecisiunu")
      , (62, "saizecisidoi")
      , (63, "saizecisitrei")
      , (64, "saizecisipatru")
      , (65, "saizecisicinci")
      , (66, "saizecisisase")
      , (67, "saizecisisapte")
      , (68, "saizecisiopt")
      , (69, "saizecisinoua")
      , (70, "saptezeci")
      , (71, "saptezecisiunu")
      , (72, "saptezecisidoi")
      , (73, "saptezecisitrei")
      , (74, "saptezecisipatru")
      , (75, "saptezecisicinci")
      , (76, "saptezecisisase")
      , (77, "saptezecisisapte")
      , (78, "saptezecisiopt")
      , (79, "saptezecisinoua")
      , (80, "optzeci")
      , (81, "optzecisiunu")
      , (82, "optzecisidoi")
      , (83, "optzecisitrei")
      , (84, "optzecisipatru")
      , (85, "optzecisicinci")
      , (86, "optzecisisase")
      , (87, "optzecisisapte")
      , (88, "optzecisiopt")
      , (89, "optzecisinoua")
      , (90, "nouazeci")
      , (91, "nouazecisiunu")
      , (92, "nouazecisidoi")
      , (93, "nouazecisitrei")
      , (94, "nouazecisipatru")
      , (95, "nouazecisicinci")
      , (96, "nouazecisisase")
      , (97, "nouazecisisapte")
      , (98, "nouazecisiopt")
      , (99, "nouazecisinoua")
      , (100, "o suta")
      ]
    )
  ]
