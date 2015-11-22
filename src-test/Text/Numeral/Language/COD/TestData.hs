{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        cod

[@Native name@]     Kokáma

[@English name@]    Cocama
-}
module Text.Numeral.Language.COD.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-cocama/en/cod/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "huepe")
      , (2, "mucuica")
      , (3, "mutsapɨrɨca")
      , (4, "iruaca")
      , (5, "pichca")
      , (6, "socta")
      , (7, "cansi")
      , (8, "pusa")
      , (9, "iscun")
      , (10, "chunga")
      , (11, "chunga huepe")
      , (12, "chunga mucuica")
      , (13, "chunga mutsapɨrɨca")
      , (14, "chunga iruaca")
      , (15, "chunga pichca")
      , (16, "chunga socta")
      , (17, "chunga cansi")
      , (18, "chunga pusa")
      , (19, "chunga iscun")
      , (20, "mucuica chunga")
      , (21, "mucuica chunga huepe")
      , (22, "mucuica chunga mucuica")
      , (23, "mucuica chunga mutsapɨrɨca")
      , (24, "mucuica chunga iruaca")
      , (25, "mucuica chunga pichca")
      , (26, "mucuica chunga socta")
      , (27, "mucuica chunga cansi")
      , (28, "mucuica chunga pusa")
      , (29, "mucuica chunga iscun")
      , (30, "mutsapɨrɨca chunga")
      , (31, "mutsapɨrɨca chunga huepe")
      , (32, "mutsapɨrɨca chunga mucuica")
      , (33, "mutsapɨrɨca chunga mutsapɨrɨca")
      , (34, "mutsapɨrɨca chunga iruaca")
      , (35, "mutsapɨrɨca chunga pichca")
      , (36, "mutsapɨrɨca chunga socta")
      , (37, "mutsapɨrɨca chunga cansi")
      , (38, "mutsapɨrɨca chunga pusa")
      , (39, "mutsapɨrɨca chunga iscun")
      , (40, "iruaca chunga")
      , (41, "iruaca chunga huepe")
      , (42, "iruaca chunga mucuica")
      , (43, "iruaca chunga mutsapɨrɨca")
      , (44, "iruaca chunga iruaca")
      , (45, "iruaca chunga pichca")
      , (46, "iruaca chunga socta")
      , (47, "iruaca chunga cansi")
      , (48, "iruaca chunga pusa")
      , (49, "iruaca chunga iscun")
      , (50, "pichca chunga")
      , (51, "pichca chunga huepe")
      , (52, "pichca chunga mucuica")
      , (53, "pichca chunga mutsapɨrɨca")
      , (54, "pichca chunga iruaca")
      , (55, "pichca chunga pichca")
      , (56, "pichca chunga socta")
      , (57, "pichca chunga cansi")
      , (58, "pichca chunga pusa")
      , (59, "pichca chunga iscun")
      , (60, "socta chunga")
      , (61, "socta chunga huepe")
      , (62, "socta chunga mucuica")
      , (63, "socta chunga mutsapɨrɨca")
      , (64, "socta chunga iruaca")
      , (65, "socta chunga pichca")
      , (66, "socta chunga socta")
      , (67, "socta chunga cansi")
      , (68, "socta chunga pusa")
      , (69, "socta chunga iscun")
      , (70, "cansi chunga")
      , (71, "cansi chunga huepe")
      , (72, "cansi chunga mucuica")
      , (73, "cansi chunga mutsapɨrɨca")
      , (74, "cansi chunga iruaca")
      , (75, "cansi chunga pichca")
      , (76, "cansi chunga socta")
      , (77, "cansi chunga cansi")
      , (78, "cansi chunga pusa")
      , (79, "cansi chunga iscun")
      , (80, "pusa chunga")
      , (81, "pusa chunga huepe")
      , (82, "pusa chunga mucuica")
      , (83, "pusa chunga mutsapɨrɨca")
      , (84, "pusa chunga iruaca")
      , (85, "pusa chunga pichca")
      , (86, "pusa chunga socta")
      , (87, "pusa chunga cansi")
      , (88, "pusa chunga pusa")
      , (89, "pusa chunga iscun")
      , (90, "iscun chunga")
      , (91, "iscun chunga huepe")
      , (92, "iscun chunga mucuica")
      , (93, "iscun chunga mutsapɨrɨca")
      , (94, "iscun chunga iruaca")
      , (95, "iscun chunga pichca")
      , (96, "iscun chunga socta")
      , (97, "iscun chunga cansi")
      , (98, "iscun chunga pusa")
      , (99, "iscun chunga iscun")
      , (100, "pacha")
      , (101, "pacha huepe")
      , (102, "pacha mucuica")
      , (103, "pacha mutsapɨrɨca")
      , (104, "pacha iruaca")
      , (105, "pacha pichca")
      , (106, "pacha socta")
      , (107, "pacha cansi")
      , (108, "pacha pusa")
      , (109, "pacha iscun")
      , (110, "pacha chunga")
      , (123, "pacha mucuica chunga mutsapɨrɨca")
      , (200, "mucuica pacha")
      , (300, "mutsapɨrɨca pacha")
      , (321, "mutsapɨrɨca pacha mucuica chunga huepe")
      , (400, "iruaca pacha")
      , (500, "pichca pacha")
      , (600, "socta pacha")
      , (700, "cansi pacha")
      , (800, "pusa pacha")
      , (900, "iscun pacha")
      , (909, "iscun pacha iscun")
      , (990, "iscun pacha iscun chunga")
      , (999, "iscun pacha iscun chunga iscun")
      , (1000, "huaranga")
      , (1001, "huaranga huepe")
      , (1008, "huaranga pusa")
      , (1234, "huaranga mucuica pacha mutsapɨrɨca chunga iruaca")
      , (2000, "mucuica huaranga")
      , (3000, "mutsapɨrɨca huaranga")
      , (4000, "iruaca huaranga")
      , (4321, "iruaca huaranga mutsapɨrɨca pacha mucuica chunga huepe")
      , (5000, "pichca huaranga")
      , (6000, "socta huaranga")
      , (7000, "cansi huaranga")
      , (8000, "pusa huaranga")
      , (9000, "iscun huaranga")
      ]
    )
  ]
