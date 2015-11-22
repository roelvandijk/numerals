{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        xpq

[@Native name@]     -

[@English name@]    Mohegan-Pequot
-}
module Text.Numeral.Language.XPQ.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-mohegan-pequot/en/xpq/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "nuqut")
      , (2, "nis")
      , (3, "shwi")
      , (4, "yáw")
      , (5, "nupáw")
      , (6, "qutôsk")
      , (7, "nisôsk")
      , (8, "shwôsk")
      , (9, "pásukokun")
      , (10, "páyaq")
      , (11, "páyaq napni nuqut")
      , (12, "páyaq napni nis")
      , (13, "páyaq napni shwi")
      , (14, "páyaq napni yáw")
      , (15, "páyaq napni nupáw")
      , (16, "páyaq napni qutôsk")
      , (17, "páyaq napni nisôsk")
      , (18, "páyaq napni shwôsk")
      , (19, "páyaq napni pásukokun")
      , (20, "nisuncák")
      , (21, "nisuncák napni nuqut")
      , (22, "nisuncák napni nis")
      , (23, "nisuncák napni shwi")
      , (24, "nisuncák napni yáw")
      , (25, "nisuncák napni nupáw")
      , (26, "nisuncák napni qutôsk")
      , (27, "nisuncák napni nisôsk")
      , (28, "nisuncák napni shwôsk")
      , (29, "nisuncák napni pásukokun")
      , (30, "swuncák")
      , (31, "swuncák napni nuqut")
      , (32, "swuncák napni nis")
      , (33, "swuncák napni shwi")
      , (34, "swuncák napni yáw")
      , (35, "swuncák napni nupáw")
      , (36, "swuncák napni qutôsk")
      , (37, "swuncák napni nisôsk")
      , (38, "swuncák napni shwôsk")
      , (39, "swuncák napni pásukokun")
      , (40, "yáwuncák")
      , (41, "yáwuncák napni nuqut")
      , (42, "yáwuncák napni nis")
      , (43, "yáwuncák napni shwi")
      , (44, "yáwuncák napni yáw")
      , (45, "yáwuncák napni nupáw")
      , (46, "yáwuncák napni qutôsk")
      , (47, "yáwuncák napni nisôsk")
      , (48, "yáwuncák napni shwôsk")
      , (49, "yáwuncák napni pásukokun")
      , (50, "nupáw-cahshuncák")
      , (51, "nupáw-cahshuncák napni nuqut")
      , (52, "nupáw-cahshuncák napni nis")
      , (53, "nupáw-cahshuncák napni shwi")
      , (54, "nupáw-cahshuncák napni yáw")
      , (55, "nupáw-cahshuncák napni nupáw")
      , (56, "nupáw-cahshuncák napni qutôsk")
      , (57, "nupáw-cahshuncák napni nisôsk")
      , (58, "nupáw-cahshuncák napni shwôsk")
      , (59, "nupáw-cahshuncák napni pásukokun")
      , (60, "qutôsk-cahshuncák")
      , (61, "qutôsk-cahshuncák napni nuqut")
      , (62, "qutôsk-cahshuncák napni nis")
      , (63, "qutôsk-cahshuncák napni shwi")
      , (64, "qutôsk-cahshuncák napni yáw")
      , (65, "qutôsk-cahshuncák napni nupáw")
      , (66, "qutôsk-cahshuncák napni qutôsk")
      , (67, "qutôsk-cahshuncák napni nisôsk")
      , (68, "qutôsk-cahshuncák napni shwôsk")
      , (69, "qutôsk-cahshuncák napni pásukokun")
      , (70, "nisôsk-cahshuncák")
      , (71, "nisôsk-cahshuncák napni nuqut")
      , (72, "nisôsk-cahshuncák napni nis")
      , (73, "nisôsk-cahshuncák napni shwi")
      , (74, "nisôsk-cahshuncák napni yáw")
      , (75, "nisôsk-cahshuncák napni nupáw")
      , (76, "nisôsk-cahshuncák napni qutôsk")
      , (77, "nisôsk-cahshuncák napni nisôsk")
      , (78, "nisôsk-cahshuncák napni shwôsk")
      , (79, "nisôsk-cahshuncák napni pásukokun")
      , (80, "shwôsk-cahshuncák")
      , (81, "shwôsk-cahshuncák napni nuqut")
      , (82, "shwôsk-cahshuncák napni nis")
      , (83, "shwôsk-cahshuncák napni shwi")
      , (84, "shwôsk-cahshuncák napni yáw")
      , (85, "shwôsk-cahshuncák napni nupáw")
      , (86, "shwôsk-cahshuncák napni qutôsk")
      , (87, "shwôsk-cahshuncák napni nisôsk")
      , (88, "shwôsk-cahshuncák napni shwôsk")
      , (89, "shwôsk-cahshuncák napni pásukokun")
      , (90, "pásukokun-cahshuncák")
      , (91, "pásukokun-cahshuncák napni nuqut")
      , (92, "pásukokun-cahshuncák napni nis")
      , (93, "pásukokun-cahshuncák napni shwi")
      , (94, "pásukokun-cahshuncák napni yáw")
      , (95, "pásukokun-cahshuncák napni nupáw")
      , (96, "pásukokun-cahshuncák napni qutôsk")
      , (97, "pásukokun-cahshuncák napni nisôsk")
      , (98, "pásukokun-cahshuncák napni shwôsk")
      , (99, "pásukokun-cahshuncák napni pásukokun")
      , (100, "pásuq")
      ]
    )
  ]
