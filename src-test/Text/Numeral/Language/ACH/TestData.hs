{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        ach

[@ISO639-3@]        ach

[@Native name@]     Lwo

[@English name@]    Acholi
-}
module Text.Numeral.Language.ACH.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude     ( Num )
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-acholi/en/ach/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "acel")
      , (2, "aryo")
      , (3, "adek")
      , (4, "aŋwen")
      , (5, "abic")
      , (6, "abicel")
      , (7, "abiro")
      , (8, "aboro")
      , (9, "aboŋwen")
      , (10, "apar")
      , (11, "apar wiye acel")
      , (12, "apar wiye aryo")
      , (13, "apar wiye adek")
      , (14, "apar wiye aŋwen")
      , (15, "apar wiye abic")
      , (16, "apar wiye abicel")
      , (17, "apar wiye abiro")
      , (18, "apar wiye aboro")
      , (19, "apar wiye aboŋwen")
      , (20, "pyeraryo")
      , (21, "pyeraryo wiye acel")
      , (22, "pyeraryo wiye aryo")
      , (23, "pyeraryo wiye adek")
      , (24, "pyeraryo wiye aŋwen")
      , (25, "pyeraryo wiye abic")
      , (26, "pyeraryo wiye abicel")
      , (27, "pyeraryo wiye abiro")
      , (28, "pyeraryo wiye aboro")
      , (29, "pyeraryo wiye aboŋwen")
      , (30, "pyeradek")
      , (31, "pyeradek wiye acel")
      , (32, "pyeradek wiye aryo")
      , (33, "pyeradek wiye adek")
      , (34, "pyeradek wiye aŋwen")
      , (35, "pyeradek wiye abic")
      , (36, "pyeradek wiye abicel")
      , (37, "pyeradek wiye abiro")
      , (38, "pyeradek wiye aboro")
      , (39, "pyeradek wiye aboŋwen")
      , (40, "pyeraŋwen")
      , (41, "pyeraŋwen wiye acel")
      , (42, "pyeraŋwen wiye aryo")
      , (43, "pyeraŋwen wiye adek")
      , (44, "pyeraŋwen wiye aŋwen")
      , (45, "pyeraŋwen wiye abic")
      , (46, "pyeraŋwen wiye abicel")
      , (47, "pyeraŋwen wiye abiro")
      , (48, "pyeraŋwen wiye aboro")
      , (49, "pyeraŋwen wiye aboŋwen")
      , (50, "pyerabic")
      , (51, "pyerabic wiye acel")
      , (52, "pyerabic wiye aryo")
      , (53, "pyerabic wiye adek")
      , (54, "pyerabic wiye aŋwen")
      , (55, "pyerabic wiye abic")
      , (56, "pyerabic wiye abicel")
      , (57, "pyerabic wiye abiro")
      , (58, "pyerabic wiye aboro")
      , (59, "pyerabic wiye aboŋwen")
      , (60, "pyerabicel")
      , (61, "pyerabicel wiye acel")
      , (62, "pyerabicel wiye aryo")
      , (63, "pyerabicel wiye adek")
      , (64, "pyerabicel wiye aŋwen")
      , (65, "pyerabicel wiye abic")
      , (66, "pyerabicel wiye abicel")
      , (67, "pyerabicel wiye abiro")
      , (68, "pyerabicel wiye aboro")
      , (69, "pyerabicel wiye aboŋwen")
      , (70, "pyerabiro")
      , (71, "pyerabiro wiye acel")
      , (72, "pyerabiro wiye aryo")
      , (73, "pyerabiro wiye adek")
      , (74, "pyerabiro wiye aŋwen")
      , (75, "pyerabiro wiye abic")
      , (76, "pyerabiro wiye abicel")
      , (77, "pyerabiro wiye abiro")
      , (78, "pyerabiro wiye aboro")
      , (79, "pyerabiro wiye aboŋwen")
      , (80, "pyeraboro")
      , (81, "pyeraboro wiye acel")
      , (82, "pyeraboro wiye aryo")
      , (83, "pyeraboro wiye adek")
      , (84, "pyeraboro wiye aŋwen")
      , (85, "pyeraboro wiye abic")
      , (86, "pyeraboro wiye abicel")
      , (87, "pyeraboro wiye abiro")
      , (88, "pyeraboro wiye aboro")
      , (89, "pyeraboro wiye aboŋwen")
      , (90, "pyeraboŋwen")
      , (91, "pyeraboŋwen wiye acel")
      , (92, "pyeraboŋwen wiye aryo")
      , (93, "pyeraboŋwen wiye adek")
      , (94, "pyeraboŋwen wiye aŋwen")
      , (95, "pyeraboŋwen wiye abic")
      , (96, "pyeraboŋwen wiye abicel")
      , (97, "pyeraboŋwen wiye abiro")
      , (98, "pyeraboŋwen wiye aboro")
      , (99, "pyeraboŋwen wiye aboŋwen")
      , (100, "miya")
      , (101, "miya acel ki acel")
      , (102, "miya acel ki aryo")
      , (103, "miya acel ki adek")
      , (104, "miya acel ki aŋwen")
      , (105, "miya acel ki abic")
      , (106, "miya acel ki abicel")
      , (107, "miya acel ki abiro")
      , (108, "miya acel ki aboro")
      , (109, "miya acel ki aboŋwen")
      , (110, "miya acel ki apar")
      , (123, "miya acel ki pyeraryo wiye adek")
      , (200, "miya aryo")
      , (300, "miya adek")
      , (321, "miya adek ki pyeraryo wiye acel")
      , (400, "miya aŋwen")
      , (500, "miya abic")
      , (600, "miya abicel")
      , (700, "miya abiro")
      , (800, "miya aboro")
      , (900, "miya aboŋwen")
      , (909, "miya aboŋwen ki aboŋwen")
      , (990, "miya aboŋwen ki pyeraboŋwen")
      , (999, "miya aboŋwen ki pyeraboŋwen wiye aboŋwen")
      , (1000, "elfu")
      , (1001, "elfu acel ki acel")
      , (1008, "elfu acel ki aboro")
      , (1234, "elfu acel ki miya aryo ki pyeradek wiye aŋwen")
      , (2000, "elfu aryo")
      , (3000, "elfu adek")
      , (4000, "elfu aŋwen")
      , (4321, "elfu aŋwen ki miya adek ki pyeraryo wiye acel")
      , (5000, "elfu abic")
      , (6000, "elfu abicel")
      , (7000, "elfu abiro")
      , (8000, "elfu aboro")
      , (9000, "elfu aboŋwen")
      , (10000, "elfu apar")
      , (12345, "apar wiye aryo elfu miya adek ki pyeraŋwen wiye abic")
      , (20000, "elfu pyeraryo")
      , (30000, "elfu pyeradek")
      , (40000, "elfu pyeraŋwen")
      , (50000, "elfu pyerabic")
      , (54321, "elfu pyerabic wiye aŋwen ki miya adek ki pyeraryo wiye acel")
      , (60000, "elfu pyerabicel")
      , (70000, "elfu pyerabiro")
      , (80000, "elfu pyeraboro")
      , (90000, "elfu pyeraboŋwen")
      ]
    )
  ]
