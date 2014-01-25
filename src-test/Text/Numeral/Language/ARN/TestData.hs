{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        arn

[@Native name@]     Mapudungun

[@English name@]    Mapuche
-}
module Text.Numeral.Language.ARN.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-mapudungun/en/arn/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "sero")
      , (1, "kiñe")
      , (2, "epu")
      , (3, "küla")
      , (4, "meli")
      , (5, "kechu")
      , (6, "kayu")
      , (7, "reqle")
      , (8, "pura")
      , (9, "aylla")
      , (10, "mari")
      , (11, "mari kiñe")
      , (12, "mari epu")
      , (13, "mari küla")
      , (14, "mari meli")
      , (15, "mari kechu")
      , (16, "mari kayu")
      , (17, "mari reqle")
      , (18, "mari pura")
      , (19, "mari aylla")
      , (20, "epu mari")
      , (21, "epu mari kiñe")
      , (22, "epu mari epu")
      , (23, "epu mari küla")
      , (24, "epu mari meli")
      , (25, "epu mari kechu")
      , (26, "epu mari kayu")
      , (27, "epu mari reqle")
      , (28, "epu mari pura")
      , (29, "epu mari aylla")
      , (30, "küla mari")
      , (31, "küla mari kiñe")
      , (32, "küla mari epu")
      , (33, "küla mari küla")
      , (34, "küla mari meli")
      , (35, "küla mari kechu")
      , (36, "küla mari kayu")
      , (37, "küla mari reqle")
      , (38, "küla mari pura")
      , (39, "küla mari aylla")
      , (40, "meli mari")
      , (41, "meli mari kiñe")
      , (42, "meli mari epu")
      , (43, "meli mari küla")
      , (44, "meli mari meli")
      , (45, "meli mari kechu")
      , (46, "meli mari kayu")
      , (47, "meli mari reqle")
      , (48, "meli mari pura")
      , (49, "meli mari aylla")
      , (50, "kechu mari")
      , (51, "kechu mari kiñe")
      , (52, "kechu mari epu")
      , (53, "kechu mari küla")
      , (54, "kechu mari meli")
      , (55, "kechu mari kechu")
      , (56, "kechu mari kayu")
      , (57, "kechu mari reqle")
      , (58, "kechu mari pura")
      , (59, "kechu mari aylla")
      , (60, "kayu mari")
      , (61, "kayu mari kiñe")
      , (62, "kayu mari epu")
      , (63, "kayu mari küla")
      , (64, "kayu mari meli")
      , (65, "kayu mari kechu")
      , (66, "kayu mari kayu")
      , (67, "kayu mari reqle")
      , (68, "kayu mari pura")
      , (69, "kayu mari aylla")
      , (70, "reqle mari")
      , (71, "reqle mari kiñe")
      , (72, "reqle mari epu")
      , (73, "reqle mari küla")
      , (74, "reqle mari meli")
      , (75, "reqle mari kechu")
      , (76, "reqle mari kayu")
      , (77, "reqle mari reqle")
      , (78, "reqle mari pura")
      , (79, "reqle mari aylla")
      , (80, "pura mari")
      , (81, "pura mari kiñe")
      , (82, "pura mari epu")
      , (83, "pura mari küla")
      , (84, "pura mari meli")
      , (85, "pura mari kechu")
      , (86, "pura mari kayu")
      , (87, "pura mari reqle")
      , (88, "pura mari pura")
      , (89, "pura mari aylla")
      , (90, "aylla mari")
      , (91, "aylla mari kiñe")
      , (92, "aylla mari epu")
      , (93, "aylla mari küla")
      , (94, "aylla mari meli")
      , (95, "aylla mari kechu")
      , (96, "aylla mari kayu")
      , (97, "aylla mari reqle")
      , (98, "aylla mari pura")
      , (99, "aylla mari aylla")
      , (100, "kiñe pataka")
      , (101, "kiñe pataka kiñe")
      , (102, "kiñe pataka epu")
      , (103, "kiñe pataka küla")
      , (104, "kiñe pataka meli")
      , (105, "kiñe pataka kechu")
      , (106, "kiñe pataka kayu")
      , (107, "kiñe pataka reqle")
      , (108, "kiñe pataka pura")
      , (109, "kiñe pataka aylla")
      , (110, "kiñe pataka mari")
      , (123, "kiñe pataka epu mari küla")
      , (200, "epu pataka")
      , (300, "küla pataka")
      , (321, "küla pataka epu mari kiñe")
      , (400, "meli pataka")
      , (500, "kechu pataka")
      , (600, "kayu pataka")
      , (700, "reqle pataka")
      , (800, "pura pataka")
      , (900, "aylla pataka")
      , (909, "aylla pataka aylla")
      , (990, "aylla pataka aylla mari")
      , (999, "aylla pataka aylla mari aylla")
      , (1000, "kiñe warangka")
      , (1001, "kiñe warangka kiñe")
      , (1008, "kiñe warangka pura")
      , (1234, "kiñe warangka epu pataka küla mari meli")
      , (2000, "epu warangka")
      , (3000, "küla warangka")
      , (4000, "meli warangka")
      , (4321, "meli warangka küla pataka epu mari kiñe")
      , (5000, "kechu warangka")
      , (6000, "kayu warangka")
      , (7000, "reqle warangka")
      , (8000, "pura warangka")
      , (9000, "aylla warangka")
      ]
    )
  ]
