{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        cku

[@Native name@]     Kowassá:ti

[@English name@]    Koasati
-}
module Text.Numeral.Language.CKU.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-koasati/en/cku/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "ikson")
      , (1, "chaffaakan")
      , (2, "toklon")
      , (3, "totchiinan")
      , (4, "ostaakan")
      , (5, "chahappaakan")
      , (6, "hannaalin")
      , (7, "ontoklon")
      , (8, "ontotchiinan")
      , (9, "chakkaalin")
      , (10, "pokkoolin")
      , (11, "pokkol awah chaffaakan")
      , (12, "pokkol awah toklon")
      , (13, "pokkol awah totchiinan")
      , (14, "pokkol awah ostaakan")
      , (15, "pokkol awah chahappaakan")
      , (16, "pokkol awah hannaalin")
      , (17, "pokkol awah ontoklon")
      , (18, "pokkol awah ontotchiinan")
      , (19, "pokkol awah chakkaalin")
      , (20, "poltoklon")
      , (21, "poltoklon polo awah chaffaakan")
      , (22, "poltoklon polo awah toklon")
      , (23, "poltoklon polo awah totchiinan")
      , (24, "poltoklon polo awah ostaakan")
      , (25, "poltoklon polo awah chahappaakan")
      , (26, "poltoklon polo awah hannaalin")
      , (27, "poltoklon polo awah ontoklon")
      , (28, "poltoklon polo awah ontotchiinan")
      , (29, "poltoklon polo awah chakkaalin")
      , (30, "poltotchiinan")
      , (31, "poltotchiinan polo awah chaffaakan")
      , (32, "poltotchiinan polo awah toklon")
      , (33, "poltotchiinan polo awah totchiinan")
      , (34, "poltotchiinan polo awah ostaakan")
      , (35, "poltotchiinan polo awah chahappaakan")
      , (36, "poltotchiinan polo awah hannaalin")
      , (37, "poltotchiinan polo awah ontoklon")
      , (38, "poltotchiinan polo awah ontotchiinan")
      , (39, "poltotchiinan polo awah chakkaalin")
      , (40, "polostaakan")
      , (41, "polostaakan polo awah chaffaakan")
      , (42, "polostaakan polo awah toklon")
      , (43, "polostaakan polo awah totchiinan")
      , (44, "polostaakan polo awah ostaakan")
      , (45, "polostaakan polo awah chahappaakan")
      , (46, "polostaakan polo awah hannaalin")
      , (47, "polostaakan polo awah ontoklon")
      , (48, "polostaakan polo awah ontotchiinan")
      , (49, "polostaakan polo awah chakkaalin")
      , (50, "polchahappaakan")
      , (51, "polchahappaakan polo awah chaffaakan")
      , (52, "polchahappaakan polo awah toklon")
      , (53, "polchahappaakan polo awah totchiinan")
      , (54, "polchahappaakan polo awah ostaakan")
      , (55, "polchahappaakan polo awah chahappaakan")
      , (56, "polchahappaakan polo awah hannaalin")
      , (57, "polchahappaakan polo awah ontoklon")
      , (58, "polchahappaakan polo awah ontotchiinan")
      , (59, "polchahappaakan polo awah chakkaalin")
      , (60, "polahannaalin")
      , (61, "polahannaalin polo awah chaffaakan")
      , (62, "polahannaalin polo awah toklon")
      , (63, "polahannaalin polo awah totchiinan")
      , (64, "polahannaalin polo awah ostaakan")
      , (65, "polahannaalin polo awah chahappaakan")
      , (66, "polahannaalin polo awah hannaalin")
      , (67, "polahannaalin polo awah ontoklon")
      , (68, "polahannaalin polo awah ontotchiinan")
      , (69, "polahannaalin polo awah chakkaalin")
      , (70, "polontoklon")
      , (71, "polontoklon polo awah chaffaakan")
      , (72, "polontoklon polo awah toklon")
      , (73, "polontoklon polo awah totchiinan")
      , (74, "polontoklon polo awah ostaakan")
      , (75, "polontoklon polo awah chahappaakan")
      , (76, "polontoklon polo awah hannaalin")
      , (77, "polontoklon polo awah ontoklon")
      , (78, "polontoklon polo awah ontotchiinan")
      , (79, "polontoklon polo awah chakkaalin")
      , (80, "polontotchiinan")
      , (81, "polontotchiinan polo awah chaffaakan")
      , (82, "polontotchiinan polo awah toklon")
      , (83, "polontotchiinan polo awah totchiinan")
      , (84, "polontotchiinan polo awah ostaakan")
      , (85, "polontotchiinan polo awah chahappaakan")
      , (86, "polontotchiinan polo awah hannaalin")
      , (87, "polontotchiinan polo awah ontoklon")
      , (88, "polontotchiinan polo awah ontotchiinan")
      , (89, "polontotchiinan polo awah chakkaalin")
      , (90, "polchakkaalin")
      , (91, "polchakkaalin polo awah chaffaakan")
      , (92, "polchakkaalin polo awah toklon")
      , (93, "polchakkaalin polo awah totchiinan")
      , (94, "polchakkaalin polo awah ostaakan")
      , (95, "polchakkaalin polo awah chahappaakan")
      , (96, "polchakkaalin polo awah hannaalin")
      , (97, "polchakkaalin polo awah ontoklon")
      , (98, "polchakkaalin polo awah ontotchiinan")
      , (99, "polchakkaalin polo awah chakkaalin")
      , (100, "chokpi chaffaakan")
      , (101, "chokpi chaffaakan awah chaffaakan")
      , (102, "chokpi chaffaakan awah toklon")
      , (103, "chokpi chaffaakan awah totchiinan")
      , (104, "chokpi chaffaakan awah ostaakan")
      , (105, "chokpi chaffaakan awah chahappaakan")
      , (106, "chokpi chaffaakan awah hannaalin")
      , (107, "chokpi chaffaakan awah ontoklon")
      , (108, "chokpi chaffaakan awah ontotchiinan")
      , (109, "chokpi chaffaakan awah chakkaalin")
      , (110, "chokpi chaffaakan awah pokkoolin")
      , (123, "chokpi chaffaakan awah poltoklon polo awah totchiinan")
      , (200, "chokpi toklon")
      , (300, "chokpi totchiinan")
      , (321, "chokpi totchiinan awah poltoklon polo awah chaffaakan")
      , (400, "chokpi ostaakan")
      , (500, "chokpi chahappaakan")
      , (600, "chokpi hannaalin")
      , (700, "chokpi ontoklon")
      , (800, "chokpi ontotchiinan")
      , (900, "chokpi chakkaalin")
      , (909, "chokpi chakkaalin awah chakkaalin")
      , (990, "chokpi chakkaalin awah polchakkaalin")
      , (999, "chokpi chakkaalin awah polchakkaalin polo awah chakkaalin")
      , (1000, "chokpachoba chaffaakan")
      , (1001, "chokpachoba chaffaakan awah chaffaakan")
      , (1008, "chokpachoba chaffaakan awah ontotchiinan")
      , (1234, "chokpachoba chaffaakan awah chokpi toklon awah poltotchiinan polo awah ostaakan")
      , (2000, "chokpachoba toklon")
      , (3000, "chokpachoba totchiinan")
      , (4000, "chokpachoba ostaakan")
      , (4321, "chokpachoba ostaakan awah chokpi totchiinan awah poltoklon polo awah chaffaakan")
      , (5000, "chokpachoba chahappaakan")
      , (6000, "chokpachoba hannaalin")
      , (7000, "chokpachoba ontoklon")
      , (8000, "chokpachoba ontotchiinan")
      , (9000, "chokpachoba chakkaalin")
      ]
    )
  ]
