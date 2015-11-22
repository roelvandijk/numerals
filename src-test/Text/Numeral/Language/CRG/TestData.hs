{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        crg

[@Native name@]     Michif

[@English name@]    Michif
-}
module Text.Numeral.Language.CRG.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-michif/en/crg/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "aeñ")
      , (2, "deu")
      , (3, "trwá")
      , (4, "kátr")
      , (5, "saeñk")
      , (6, "sis")
      , (7, "set")
      , (8, "wit")
      , (9, "naef")
      , (10, "jis")
      , (11, "óñz")
      , (12, "dóz")
      , (13, "trayz")
      , (14, "katorz")
      , (15, "kaeñz")
      , (16, "saeñz")
      , (17, "jis set")
      , (18, "jis wit")
      , (19, "jis naef")
      , (20, "vaeñ")
      , (21, "vaeñ aeñ")
      , (22, "vaeñ deu")
      , (23, "vaeñ trwá")
      , (24, "vaeñ kátr")
      , (25, "vaeñ saeñk")
      , (26, "vaeñ sis")
      , (27, "vaeñ set")
      , (28, "vaeñ wit")
      , (29, "vaeñ naef")
      , (30, "tráñt")
      , (31, "tráñt aeñ")
      , (32, "tráñt deu")
      , (33, "tráñt trwá")
      , (34, "tráñt kátr")
      , (35, "tráñt saeñk")
      , (36, "tráñt sis")
      , (37, "tráñt set")
      , (38, "tráñt wit")
      , (39, "tráñt naef")
      , (40, "karánt")
      , (41, "karánt aeñ")
      , (42, "karánt deu")
      , (43, "karánt trwá")
      , (44, "karánt kátr")
      , (45, "karánt saeñk")
      , (46, "karánt sis")
      , (47, "karánt set")
      , (48, "karánt wit")
      , (49, "karánt naef")
      , (50, "saeñkánt")
      , (51, "saeñkánt aeñ")
      , (52, "saeñkánt deu")
      , (53, "saeñkánt trwá")
      , (54, "saeñkánt kátr")
      , (55, "saeñkánt saeñk")
      , (56, "saeñkánt sis")
      , (57, "saeñkánt set")
      , (58, "saeñkánt wit")
      , (59, "saeñkánt naef")
      , (60, "swesáñt")
      , (61, "swesáñt aeñ")
      , (62, "swesáñt deu")
      , (63, "swesáñt trwá")
      , (64, "swesáñt kátr")
      , (65, "swesáñt saeñk")
      , (66, "swesáñt sis")
      , (67, "swesáñt set")
      , (68, "swesáñt wit")
      , (69, "swesáñt naef")
      , (70, "swesáñty jis")
      , (71, "swesáñty jis aeñ")
      , (72, "swesáñty jis deu")
      , (73, "swesáñty jis trwá")
      , (74, "swesáñty jis kátr")
      , (75, "swesáñty jis saeñk")
      , (76, "swesáñty jis sis")
      , (77, "swesáñty jis set")
      , (78, "swesáñty jis wit")
      , (79, "swesáñty jis naef")
      , (80, "katávaeñ")
      , (81, "katávaeñ aeñ")
      , (82, "katávaeñ deu")
      , (83, "katávaeñ trwá")
      , (84, "katávaeñ kátr")
      , (85, "katávaeñ saeñk")
      , (86, "katávaeñ sis")
      , (87, "katávaeñ set")
      , (88, "katávaeñ wit")
      , (89, "katávaeñ naef")
      , (90, "katrávaen jis")
      , (91, "katrávaen jis aeñ")
      , (92, "katrávaen jis deu")
      , (93, "katrávaen jis trwá")
      , (94, "katrávaen jis kátr")
      , (95, "katrávaen jis saeñk")
      , (96, "katrávaen jis sis")
      , (97, "katrávaen jis set")
      , (98, "katrávaen jis wit")
      , (99, "katrávaen jis naef")
      , (100, "sáñ")
      ]
    )
  ]
