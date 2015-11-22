{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        amp

[@Native name@]     -

[@English name@]    Alamblak
-}

module Text.Numeral.Language.AMP.TestData (cardinals) where


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
  http://www.sf.airnet.ne.jp/~ts/language/number/alamblak.html
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "rpat")
      , (2, "hosf")
      , (3, "hosfirpat")
      , (4, "hosfihosf")
      , (5, "tir yohtt")
      , (6, "tir yohtti rpat")
      , (7, "tir yohtti hosf")
      , (8, "tir yohtti hosfirpat")
      , (9, "tir yohtti hosfihosf")
      , (10, "tir hosf")
      , (11, "tir hosfi rpat")
      , (12, "tir hosfi hosf")
      , (13, "tir hosfi hosfirpat")
      , (14, "tir hosfi hosfihosf")
      , (15, "tir hosfirpat")
      , (16, "tir hosfirpati rpat")
      , (17, "tir hosfirpati hosf")
      , (18, "tir hosfirpati hosfirpat")
      , (19, "tir hosfirpati hosfihosf")
      , (20, "yima yohtt")
      , (21, "yima yohtti rpat")
      , (22, "yima yohtti hosf")
      , (23, "yima yohtti hosfirpat")
      , (24, "yima yohtti hosfihosf")
      , (25, "yima yohtti tir yohtt")
      , (26, "yima yohtti tir yohtti rpat")
      , (27, "yima yohtti tir yohtti hosf")
      , (28, "yima yohtti tir yohtti hosfirpat")
      , (29, "yima yohtti tir yohtti hosfihosf")
      , (30, "yima yohtti tir hosf")
      , (31, "yima yohtti tir hosfi rpat")
      , (32, "yima yohtti tir hosfi hosf")
      , (33, "yima yohtti tir hosfi hosfirpat")
      , (34, "yima yohtti tir hosfi hosfihosf")
      , (35, "yima yohtti tir hosfirpat")
      , (36, "yima yohtti tir hosfirpati rpat")
      , (37, "yima yohtti tir hosfirpati hosf")
      , (38, "yima yohtti tir hosfirpati hosfirpat")
      , (39, "yima yohtti tir hosfirpati hosfihosf")
      , (40, "yima hosf")
      , (41, "yima hosfi rpat")
      , (42, "yima hosfi hosf")
      , (43, "yima hosfi hosfirpat")
      , (44, "yima hosfi hosfihosf")
      , (45, "yima hosfi tir yohtt")
      , (46, "yima hosfi tir yohtti rpat")
      , (47, "yima hosfi tir yohtti hosf")
      , (48, "yima hosfi tir yohtti hosfirpat")
      , (49, "yima hosfi tir yohtti hosfihosf")
      , (50, "yima hosfi tir hosf")
      , (51, "yima hosfi tir hosfi rpat")
      , (52, "yima hosfi tir hosfi hosf")
      , (53, "yima hosfi tir hosfi hosfirpat")
      , (54, "yima hosfi tir hosfi hosfihosf")
      , (55, "yima hosfi tir hosfirpat")
      , (56, "yima hosfi tir hosfirpati rpat")
      , (57, "yima hosfi tir hosfirpati hosf")
      , (58, "yima hosfi tir hosfirpati hosfirpat")
      , (59, "yima hosfi tir hosfirpati hosfihosf")
      , (60, "yima hosfirpat")
      , (61, "yima hosfirpati rpat")
      , (62, "yima hosfirpati hosf")
      , (63, "yima hosfirpati hosfirpat")
      , (64, "yima hosfirpati hosfihosf")
      , (65, "yima hosfirpati tir yohtt")
      , (66, "yima hosfirpati tir yohtti rpat")
      , (67, "yima hosfirpati tir yohtti hosf")
      , (68, "yima hosfirpati tir yohtti hosfirpat")
      , (69, "yima hosfirpati tir yohtti hosfihosf")
      , (70, "yima hosfirpati tir hosf")
      , (71, "yima hosfirpati tir hosfi rpat")
      , (72, "yima hosfirpati tir hosfi hosf")
      , (73, "yima hosfirpati tir hosfi hosfirpat")
      , (74, "yima hosfirpati tir hosfi hosfihosf")
      , (75, "yima hosfirpati tir hosfirpat")
      , (76, "yima hosfirpati tir hosfirpati rpat")
      , (77, "yima hosfirpati tir hosfirpati hosf")
      , (78, "yima hosfirpati tir hosfirpati hosfirpat")
      , (79, "yima hosfirpati tir hosfirpati hosfihosf")
      , (80, "yima hosfihosf")
      , (81, "yima hosfihosfi rpat")
      , (82, "yima hosfihosfi hosf")
      , (83, "yima hosfihosfi hosfirpat")
      , (84, "yima hosfihosfi hosfihosf")
      , (85, "yima hosfihosfi tir yohtt")
      , (86, "yima hosfihosfi tir yohtti rpat")
      , (87, "yima hosfihosfi tir yohtti hosf")
      , (88, "yima hosfihosfi tir yohtti hosfirpat")
      , (89, "yima hosfihosfi tir yohtti hosfihosf")
      , (90, "yima hosfihosfi tir hosf")
      , (91, "yima hosfihosfi tir hosfi rpat")
      , (92, "yima hosfihosfi tir hosfi hosf")
      , (93, "yima hosfihosfi tir hosfi hosfirpat")
      , (94, "yima hosfihosfi tir hosfi hosfihosf")
      , (95, "yima hosfihosfi tir hosfirpat")
      , (96, "yima hosfihosfi tir hosfirpati rpat")
      , (97, "yima hosfihosfi tir hosfirpati hosf")
      , (98, "yima hosfihosfi tir hosfirpati hosfirpat")
      , (99, "yima hosfihosfi tir hosfirpati hosfihosf")
      , (100, "yima tir yohtt")
      ]
    )
  ]
