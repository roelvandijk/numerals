{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        mnk

[@Native name@]     Mandingo, لغة مندنكا

[@English name@]    Mandinka
-}
module Text.Numeral.Language.MNK.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-mandinka/en/mnk/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "kiliŋ")
      , (2, "fula")
      , (3, "saba")
      , (4, "naani")
      , (5, "luulu")
      , (6, "wooro")
      , (7, "worowula")
      , (8, "sey")
      , (9, "kononto")
      , (10, "taŋ")
      , (11, "taŋ niŋ kiliŋ")
      , (12, "taŋ niŋ fula")
      , (13, "taŋ niŋ saba")
      , (14, "taŋ niŋ naani")
      , (15, "taŋ niŋ luulu")
      , (16, "taŋ niŋ wooro")
      , (17, "taŋ niŋ worowula")
      , (18, "taŋ niŋ sey")
      , (19, "taŋ niŋ kononto")
      , (20, "muwaŋ")
      , (21, "muwaŋ niŋ kiliŋ")
      , (22, "muwaŋ niŋ fula")
      , (23, "muwaŋ niŋ saba")
      , (24, "muwaŋ niŋ naani")
      , (25, "muwaŋ niŋ luulu")
      , (26, "muwaŋ niŋ wooro")
      , (27, "muwaŋ niŋ worowula")
      , (28, "muwaŋ niŋ sey")
      , (29, "muwaŋ niŋ kononto")
      , (30, "taŋ saba")
      , (31, "taŋ saba niŋ kiliŋ")
      , (32, "taŋ saba niŋ fula")
      , (33, "taŋ saba niŋ saba")
      , (34, "taŋ saba niŋ naani")
      , (35, "taŋ saba niŋ luulu")
      , (36, "taŋ saba niŋ wooro")
      , (37, "taŋ saba niŋ worowula")
      , (38, "taŋ saba niŋ sey")
      , (39, "taŋ saba niŋ kononto")
      , (40, "taŋ naani")
      , (41, "taŋ naani niŋ kiliŋ")
      , (42, "taŋ naani niŋ fula")
      , (43, "taŋ naani niŋ saba")
      , (44, "taŋ naani niŋ naani")
      , (45, "taŋ naani niŋ luulu")
      , (46, "taŋ naani niŋ wooro")
      , (47, "taŋ naani niŋ worowula")
      , (48, "taŋ naani niŋ sey")
      , (49, "taŋ naani niŋ kononto")
      , (50, "taŋ luulu")
      , (51, "taŋ luulu niŋ kiliŋ")
      , (52, "taŋ luulu niŋ fula")
      , (53, "taŋ luulu niŋ saba")
      , (54, "taŋ luulu niŋ naani")
      , (55, "taŋ luulu niŋ luulu")
      , (56, "taŋ luulu niŋ wooro")
      , (57, "taŋ luulu niŋ worowula")
      , (58, "taŋ luulu niŋ sey")
      , (59, "taŋ luulu niŋ kononto")
      , (60, "taŋ wooro")
      , (61, "taŋ wooro niŋ kiliŋ")
      , (62, "taŋ wooro niŋ fula")
      , (63, "taŋ wooro niŋ saba")
      , (64, "taŋ wooro niŋ naani")
      , (65, "taŋ wooro niŋ luulu")
      , (66, "taŋ wooro niŋ wooro")
      , (67, "taŋ wooro niŋ worowula")
      , (68, "taŋ wooro niŋ sey")
      , (69, "taŋ wooro niŋ kononto")
      , (70, "taŋ worowula")
      , (71, "taŋ worowula niŋ kiliŋ")
      , (72, "taŋ worowula niŋ fula")
      , (73, "taŋ worowula niŋ saba")
      , (74, "taŋ worowula niŋ naani")
      , (75, "taŋ worowula niŋ luulu")
      , (76, "taŋ worowula niŋ wooro")
      , (77, "taŋ worowula niŋ worowula")
      , (78, "taŋ worowula niŋ sey")
      , (79, "taŋ worowula niŋ kononto")
      , (80, "taŋ sey")
      , (81, "taŋ sey niŋ kiliŋ")
      , (82, "taŋ sey niŋ fula")
      , (83, "taŋ sey niŋ saba")
      , (84, "taŋ sey niŋ naani")
      , (85, "taŋ sey niŋ luulu")
      , (86, "taŋ sey niŋ wooro")
      , (87, "taŋ sey niŋ worowula")
      , (88, "taŋ sey niŋ sey")
      , (89, "taŋ sey niŋ kononto")
      , (90, "taŋ konoto")
      , (91, "taŋ konoto niŋ kiliŋ")
      , (92, "taŋ konoto niŋ fula")
      , (93, "taŋ konoto niŋ saba")
      , (94, "taŋ konoto niŋ naani")
      , (95, "taŋ konoto niŋ luulu")
      , (96, "taŋ konoto niŋ wooro")
      , (97, "taŋ konoto niŋ worowula")
      , (98, "taŋ konoto niŋ sey")
      , (99, "taŋ konoto niŋ kononto")
      , (100, "keme kiliŋ")
      , (101, "keme kiliŋ niŋ kiliŋ")
      , (102, "keme kiliŋ niŋ fula")
      , (103, "keme kiliŋ niŋ saba")
      , (104, "keme kiliŋ niŋ naani")
      , (105, "keme kiliŋ niŋ luulu")
      , (106, "keme kiliŋ niŋ wooro")
      , (107, "keme kiliŋ niŋ worowula")
      , (108, "keme kiliŋ niŋ sey")
      , (109, "keme kiliŋ niŋ kononto")
      , (110, "keme kiliŋ niŋ taŋ")
      , (123, "keme kiliŋ niŋ muwaŋ niŋ saba")
      , (200, "keme fula")
      , (300, "keme saba")
      , (321, "keme saba niŋ muwaŋ niŋ kiliŋ")
      , (400, "keme naani")
      , (500, "keme luulu")
      , (600, "keme wooro")
      , (700, "keme worowula")
      , (800, "keme sey")
      , (900, "keme kononto")
      , (909, "keme kononto niŋ kononto")
      , (990, "keme kononto niŋ taŋ konoto")
      , (999, "keme kononto niŋ taŋ konoto niŋ kononto")
      , (1000, "wuli kiliŋ")
      , (1001, "wuli kiliŋ niŋ kiliŋ")
      , (1008, "wuli kiliŋ niŋ sey")
      , (1234, "wuli kiliŋ niŋ keme fula niŋ taŋ saba niŋ naani")
      , (2000, "wuli fula")
      , (3000, "wuli saba")
      , (4000, "wuli naani")
      , (4321, "wuli naani niŋ keme saba niŋ muwaŋ niŋ kiliŋ")
      , (5000, "wuli luulu")
      , (6000, "wuli wooro")
      , (7000, "wuli worowula")
      , (8000, "wuli sey")
      , (9000, "wuli kononto")
      ]
    )
  ]
