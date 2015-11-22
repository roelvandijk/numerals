{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        coo

[@Native name@]     -

[@English name@]    Comox
-}
module Text.Numeral.Language.COO.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-comox/en/coo/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "paʔa")
      , (2, "saʔa")
      , (3, "čɛlas")
      , (4, "mos")
      , (5, "θiyɛčɩs")
      , (6, "t̓əxəm")
      , (7, "tᶿočɩs")
      , (8, "taʔačɩs")
      , (9, "tɩgiχʷ")
      , (10, "opən")
      , (11, "ʔopən hekʷ paʔa")
      , (12, "ʔopən hekʷ saʔa")
      , (13, "ʔopən hekʷ čɛlas")
      , (14, "ʔopən hekʷ mos")
      , (15, "ʔopən hekʷ θiyɛčɩs")
      , (16, "ʔopən hekʷ t̓əxəm")
      , (17, "ʔopən hekʷ tᶿočɩs")
      , (18, "ʔopən hekʷ taʔačɩs")
      , (19, "ʔopən hekʷ tɩgiχʷ")
      , (20, "θamšɛ")
      , (21, "θamšɛ heykʷ paʔa")
      , (22, "θamšɛ heykʷ saʔa")
      , (23, "θamšɛ heykʷ čɛlas")
      , (24, "θamšɛ heykʷ mos")
      , (25, "θamšɛ heykʷ θiyɛčɩs")
      , (26, "θamšɛ heykʷ t̓əxəm")
      , (27, "θamšɛ heykʷ tᶿočɩs")
      , (28, "θamšɛ heykʷ taʔačɩs")
      , (29, "θamšɛ heykʷ tɩgiχʷ")
      , (30, "čɩnuxʷ šɛ")
      , (31, "čɩnuxʷ šɛ heykʷ paʔa")
      , (32, "čɩnuxʷ šɛ heykʷ saʔa")
      , (33, "čɩnuxʷ šɛ heykʷ čɛlas")
      , (34, "čɩnuxʷ šɛ heykʷ mos")
      , (35, "čɩnuxʷ šɛ heykʷ θiyɛčɩs")
      , (36, "čɩnuxʷ šɛ heykʷ t̓əxəm")
      , (37, "čɩnuxʷ šɛ heykʷ tᶿočɩs")
      , (38, "čɩnuxʷ šɛ heykʷ taʔačɩs")
      , (39, "čɩnuxʷ šɛ heykʷ tɩgiχʷ")
      , (40, "mosaɬ šɛ")
      , (41, "mosaɬ šɛ heykʷ paʔa")
      , (42, "mosaɬ šɛ heykʷ saʔa")
      , (43, "mosaɬ šɛ heykʷ čɛlas")
      , (44, "mosaɬ šɛ heykʷ mos")
      , (45, "mosaɬ šɛ heykʷ θiyɛčɩs")
      , (46, "mosaɬ šɛ heykʷ t̓əxəm")
      , (47, "mosaɬ šɛ heykʷ tᶿočɩs")
      , (48, "mosaɬ šɛ heykʷ taʔačɩs")
      , (49, "mosaɬ šɛ heykʷ tɩgiχʷ")
      , (50, "θiyɛčɩsaɬšɛ")
      , (51, "θiyɛčɩsaɬšɛ heykʷ paʔa")
      , (52, "θiyɛčɩsaɬšɛ heykʷ saʔa")
      , (53, "θiyɛčɩsaɬšɛ heykʷ čɛlas")
      , (54, "θiyɛčɩsaɬšɛ heykʷ mos")
      , (55, "θiyɛčɩsaɬšɛ heykʷ θiyɛčɩs")
      , (56, "θiyɛčɩsaɬšɛ heykʷ t̓əxəm")
      , (57, "θiyɛčɩsaɬšɛ heykʷ tᶿočɩs")
      , (58, "θiyɛčɩsaɬšɛ heykʷ taʔačɩs")
      , (59, "θiyɛčɩsaɬšɛ heykʷ tɩgiχʷ")
      , (60, "t̓əχmaɬ šɛ")
      , (61, "t̓əχmaɬ šɛ heykʷ paʔa")
      , (62, "t̓əχmaɬ šɛ heykʷ saʔa")
      , (63, "t̓əχmaɬ šɛ heykʷ čɛlas")
      , (64, "t̓əχmaɬ šɛ heykʷ mos")
      , (65, "t̓əχmaɬ šɛ heykʷ θiyɛčɩs")
      , (66, "t̓əχmaɬ šɛ heykʷ t̓əxəm")
      , (67, "t̓əχmaɬ šɛ heykʷ tᶿočɩs")
      , (68, "t̓əχmaɬ šɛ heykʷ taʔačɩs")
      , (69, "t̓əχmaɬ šɛ heykʷ tɩgiχʷ")
      , (70, "tᶿočɩsaɬ šɛ")
      , (71, "tᶿočɩsaɬ šɛ heykʷ paʔa")
      , (72, "tᶿočɩsaɬ šɛ heykʷ saʔa")
      , (73, "tᶿočɩsaɬ šɛ heykʷ čɛlas")
      , (74, "tᶿočɩsaɬ šɛ heykʷ mos")
      , (75, "tᶿočɩsaɬ šɛ heykʷ θiyɛčɩs")
      , (76, "tᶿočɩsaɬ šɛ heykʷ t̓əxəm")
      , (77, "tᶿočɩsaɬ šɛ heykʷ tᶿočɩs")
      , (78, "tᶿočɩsaɬ šɛ heykʷ taʔačɩs")
      , (79, "tᶿočɩsaɬ šɛ heykʷ tɩgiχʷ")
      , (80, "taʔačɩsaɬ šɛ")
      , (81, "taʔačɩsaɬ šɛ heykʷ paʔa")
      , (82, "taʔačɩsaɬ šɛ heykʷ saʔa")
      , (83, "taʔačɩsaɬ šɛ heykʷ čɛlas")
      , (84, "taʔačɩsaɬ šɛ heykʷ mos")
      , (85, "taʔačɩsaɬ šɛ heykʷ θiyɛčɩs")
      , (86, "taʔačɩsaɬ šɛ heykʷ t̓əxəm")
      , (87, "taʔačɩsaɬ šɛ heykʷ tᶿočɩs")
      , (88, "taʔačɩsaɬ šɛ heykʷ taʔačɩs")
      , (89, "taʔačɩsaɬ šɛ heykʷ tɩgiχʷ")
      , (90, "tɩgixʷaɬ šɛ")
      , (91, "tɩgixʷaɬ šɛ heykʷ paʔa")
      , (92, "tɩgixʷaɬ šɛ heykʷ saʔa")
      , (93, "tɩgixʷaɬ šɛ heykʷ čɛlas")
      , (94, "tɩgixʷaɬ šɛ heykʷ mos")
      , (95, "tɩgixʷaɬ šɛ heykʷ θiyɛčɩs")
      , (96, "tɩgixʷaɬ šɛ heykʷ t̓əxəm")
      , (97, "tɩgixʷaɬ šɛ heykʷ tᶿočɩs")
      , (98, "tɩgixʷaɬ šɛ heykʷ taʔačɩs")
      , (99, "tɩgixʷaɬ šɛ heykʷ tɩgiχʷ")
      , (100, "opən təsɛʔɛč")
      ]
    )
  ]
