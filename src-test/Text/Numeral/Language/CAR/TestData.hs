{-|
[@ISO639-1@]        -

[@ISO639-2@]        car

[@ISO639-3@]        car

[@Native name@]     -

[@English name@]    Carib
-}
module Text.Numeral.Language.CAR.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-kalina/en/car/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "òwin")
      , (2, "oko")
      , (3, "oruwa")
      , (4, "okupàen")
      , (5, "ainatone")
      , (6, "òwin-tòima")
      , (7, "oko-tòima")
      , (8, "oruwa-tòima")
      , (9, "okupàen-tòima")
      , (10, "ainapatoro")
      , (11, "ainapatoro itùponaka òwin")
      , (12, "ainapatoro itùponaka oko")
      , (13, "ainapatoro itùponaka oruwa")
      , (14, "ainapatoro itùponaka okupàen")
      , (15, "atonèpu")
      , (16, "ainapatoro itùponaka òwin-tòima")
      , (17, "ainapatoro itùponaka oko-tòima")
      , (18, "ainapatoro itùponaka oruwa-tòima")
      , (19, "ainapatoro itùponaka okupàen-tòima")
      , (20, "òwin-karìna")
      , (21, "òwin-karìna itùponaka òwin")
      , (22, "òwin-karìna itùponaka oko")
      , (23, "òwin-karìna itùponaka oruwa")
      , (24, "òwin-karìna itùponaka okupàen")
      , (25, "òwin-karìna itùponaka ainatone")
      , (26, "òwin-karìna itùponaka òwin-tòima")
      , (27, "òwin-karìna itùponaka oko-tòima")
      , (28, "òwin-karìna itùponaka oruwa-tòima")
      , (29, "òwin-karìna itùponaka okupàen-tòima")
      , (30, "òwin-karìna itùponaka ainapatoro")
      , (31, "òwin-karìna itùponaka ainapatoro itùponaka òwin")
      , (32, "òwin-karìna itùponaka ainapatoro itùponaka oko")
      , (33, "òwin-karìna itùponaka ainapatoro itùponaka oruwa")
      , (34, "òwin-karìna itùponaka ainapatoro itùponaka okupàen")
      , (35, "òwin-karìna itùponaka atonèpu")
      , (36, "òwin-karìna itùponaka ainapatoro itùponaka òwin-tòima")
      , (37, "òwin-karìna itùponaka ainapatoro itùponaka oko-tòima")
      , (38, "òwin-karìna itùponaka ainapatoro itùponaka oruwa-tòima")
      , (39, "òwin-karìna itùponaka ainapatoro itùponaka okupàen-tòima")
      , (40, "oko-karìna")
      , (41, "oko-karìna itùponaka òwin")
      , (42, "oko-karìna itùponaka oko")
      , (43, "oko-karìna itùponaka oruwa")
      , (44, "oko-karìna itùponaka okupàen")
      , (45, "oko-karìna itùponaka ainatone")
      , (46, "oko-karìna itùponaka òwin-tòima")
      , (47, "oko-karìna itùponaka oko-tòima")
      , (48, "oko-karìna itùponaka oruwa-tòima")
      , (49, "oko-karìna itùponaka okupàen-tòima")
      , (50, "oko-karìna itùponaka ainapatoro")
      , (51, "oko-karìna itùponaka ainapatoro itùponaka òwin")
      , (52, "oko-karìna itùponaka ainapatoro itùponaka oko")
      , (53, "oko-karìna itùponaka ainapatoro itùponaka oruwa")
      , (54, "oko-karìna itùponaka ainapatoro itùponaka okupàen")
      , (55, "oko-karìna itùponaka atonèpu")
      , (56, "oko-karìna itùponaka ainapatoro itùponaka òwin-tòima")
      , (57, "oko-karìna itùponaka ainapatoro itùponaka oko-tòima")
      , (58, "oko-karìna itùponaka ainapatoro itùponaka oruwa-tòima")
      , (59, "oko-karìna itùponaka ainapatoro itùponaka okupàen-tòima")
      , (60, "oruwa-karìna")
      , (61, "oruwa-karìna itùponaka òwin")
      , (62, "oruwa-karìna itùponaka oko")
      , (63, "oruwa-karìna itùponaka oruwa")
      , (64, "oruwa-karìna itùponaka okupàen")
      , (65, "oruwa-karìna itùponaka ainatone")
      , (66, "oruwa-karìna itùponaka òwin-tòima")
      , (67, "oruwa-karìna itùponaka oko-tòima")
      , (68, "oruwa-karìna itùponaka oruwa-tòima")
      , (69, "oruwa-karìna itùponaka okupàen-tòima")
      , (70, "oruwa-karìna itùponaka ainapatoro")
      , (71, "oruwa-karìna itùponaka ainapatoro itùponaka òwin")
      , (72, "oruwa-karìna itùponaka ainapatoro itùponaka oko")
      , (73, "oruwa-karìna itùponaka ainapatoro itùponaka oruwa")
      , (74, "oruwa-karìna itùponaka ainapatoro itùponaka okupàen")
      , (75, "oruwa-karìna itùponaka atonèpu")
      , (76, "oruwa-karìna itùponaka ainapatoro itùponaka òwin-tòima")
      , (77, "oruwa-karìna itùponaka ainapatoro itùponaka oko-tòima")
      , (78, "oruwa-karìna itùponaka ainapatoro itùponaka oruwa-tòima")
      , (79, "oruwa-karìna itùponaka ainapatoro itùponaka okupàen-tòima")
      , (80, "okupàen-karìna")
      , (81, "okupàen-karìna itùponaka òwin")
      , (82, "okupàen-karìna itùponaka oko")
      , (83, "okupàen-karìna itùponaka oruwa")
      , (84, "okupàen-karìna itùponaka okupàen")
      , (85, "okupàen-karìna itùponaka ainatone")
      , (86, "okupàen-karìna itùponaka òwin-tòima")
      , (87, "okupàen-karìna itùponaka oko-tòima")
      , (88, "okupàen-karìna itùponaka oruwa-tòima")
      , (89, "okupàen-karìna itùponaka okupàen-tòima")
      , (90, "okupàen-karìna itùponaka ainapatoro")
      , (91, "okupàen-karìna itùponaka ainapatoro itùponaka òwin")
      , (92, "okupàen-karìna itùponaka ainapatoro itùponaka oko")
      , (93, "okupàen-karìna itùponaka ainapatoro itùponaka oruwa")
      , (94, "okupàen-karìna itùponaka ainapatoro itùponaka okupàen")
      , (95, "okupàen-karìna itùponaka atonèpu")
      , (96, "okupàen-karìna itùponaka ainapatoro itùponaka òwin-tòima")
      , (97, "okupàen-karìna itùponaka ainapatoro itùponaka oko-tòima")
      , (98, "okupàen-karìna itùponaka ainapatoro itùponaka oruwa-tòima")
      , (99, "okupàen-karìna itùponaka ainapatoro itùponaka okupàen-tòima")
      , (100, "ainatone-karìna")
      , (101, "ainatone-karìna itùponaka òwin")
      , (102, "ainatone-karìna itùponaka oko")
      , (103, "ainatone-karìna itùponaka oruwa")
      , (104, "ainatone-karìna itùponaka okupàen")
      , (105, "ainatone-karìna itùponaka ainatone")
      , (106, "ainatone-karìna itùponaka òwin-tòima")
      , (107, "ainatone-karìna itùponaka oko-tòima")
      , (108, "ainatone-karìna itùponaka oruwa-tòima")
      , (109, "ainatone-karìna itùponaka okupàen-tòima")
      , (110, "ainatone-karìna itùponaka ainapatoro")
      , (123, "òwin-tòima-karìna itùponaka oruwa")
      , (200, "ainapatoro-karìna")
      , (300, "atonèpu-karìna")
      , (321, "atonèpu-itùponaka-òwin-karìna itùponaka òwin")
      ]
    )
  ]
