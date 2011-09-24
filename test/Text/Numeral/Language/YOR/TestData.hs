{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        yo

[@ISO639-2@]        yor

[@ISO639-3@]        yor

[@Native name@]     èdè Yorùbá

[@English name@]    Yoruba
-}

module Text.Numeral.Language.YOR.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Num )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

cardinals ∷ (Num i, IsString s) ⇒ [(i, s)]
cardinals =
  [ (1, "ikan")
  , (2, "meji")
  , (3, "meta")
  , (4, "merin")
  , (5, "marun")
  , (6, "mefa")
  , (7, "meje")
  , (8, "mejo")
  , (9, "mesan")
  , (10, "mewa")
  , (11, "mokanla")
  , (12, "mejila")
  , (13, "metala")
  , (14, "merinla")
  , (15, "medogun")
  , (16, "merindilogun")
  , (17, "metadilogun")
  , (18, "mejidilogun")
  , (19, "mokandilogun")
  , (20, "ogun")
  , (21, "mokanlelogun")
  , (22, "mejilelogun")
  , (23, "metalelogun")
  , (24, "merinlelogun")
  , (25, "medogbon")
  , (26, "merindilogbon")
  , (27, "metadilogbon")
  , (28, "mejidilogbon")
  , (29, "mokandilogbon")
  , (30, "ogbon")
  , (31, "mokanlelogbon")
  , (32, "mejilelogbon")
  , (33, "metalelogbon")
  , (34, "merinlelogbon")
  , (35, "marundilogoji")
  , (36, "merindilogoji")
  , (37, "metadilogoji")
  , (38, "mejidilogoji")
  , (39, "mokandilogoji")
  , (40, "ogoji")
  , (41, "mokanlelogoji")
  , (42, "mejilelogoji")
  , (43, "metalelogoji")
  , (44, "merinlelogoji")
  , (45, "marundiladota")
  , (46, "merindiladota")
  , (47, "metadiladota")
  , (48, "mejidiladota")
  , (49, "mokandiladota")
  , (50, "adota")
  , (51, "mokanleladota")
  , (52, "mejileladota")
  , (53, "metaleladota")
  , (54, "merinleladota")
  , (55, "marundilogota")
  , (56, "merindilogota")
  , (57, "metadilogota")
  , (58, "mejidilogota")
  , (59, "mokandilogota")
  , (60, "ogota")
  , (61, "mokanlelogota")
  , (62, "mejilelogota")
  , (63, "metalelogota")
  , (64, "merinlelogota")
  , (65, "marundiladorin")
  , (66, "merindiladorin")
  , (67, "metadiladorin")
  , (68, "mejidiladorin")
  , (69, "mokandiladorin")
  , (70, "adorin")
  , (71, "mokanleladorin")
  , (72, "mejileladorin")
  , (73, "metaleladorin")
  , (74, "merinleladorin")
  , (75, "marundilogorin")
  , (76, "merindilogorin")
  , (77, "metadilogorin")
  , (78, "mejidilogorin")
  , (79, "mokandilogorin")
  , (80, "ogorin")
  , (81, "mokanlelogorin")
  , (82, "mejilelogorin")
  , (83, "metalelogorin")
  , (84, "merinlelogorin")
  , (85, "marundiladorun")
  , (86, "merindiladorun")
  , (87, "metadiladorun")
  , (88, "mejidiladorun")
  , (89, "mokandiladorun")
  , (90, "adorun")
  , (91, "mokanleladorun")
  , (92, "mejileladorun")
  , (93, "metaleladorun")
  , (94, "merinleladorun")
  , (95, "marundilogorun")
  , (96, "merindilogorun")
  , (97, "metadilogorun")
  , (98, "mejidilogorun")
  , (99, "mokandilogorun")
  , (100, "ogorun")
  , (200, "ogorun meji")
  , (300, "ogorun meta")
  , (400, "ogorun merin")
  , (500, "ogorun marun")
  , (600, "ogorun mefa")
  , (700, "ogorun meje")
  , (800, "ogorun mejo")
  , (900, "ogorun mesan")
  , (1000, "egberun")
  ]

