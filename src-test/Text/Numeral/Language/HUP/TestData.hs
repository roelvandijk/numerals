{-|
[@ISO639-1@]        -

[@ISO639-2@]        hup

[@ISO639-3@]        hup

[@Native name@]     -

[@English name@]    Hupa
-}
module Text.Numeral.Language.HUP.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-hupa/en/hup/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "ła’")
      , (2, "nahx")
      , (3, "ta:q’")
      , (4, "dink’")
      , (5, "chwola’")
      , (6, "xosta:n")
      , (7, "xohk’it")
      , (8, "ke:nim")
      , (9, "miq’os-t’aw")
      , (10, "minłung")
      , (11, "minłung-miwah-na:ła’")
      , (12, "minłung-miwah-na:nahx")
      , (13, "minłung-miwah-na:ta:q’")
      , (14, "minłung-miwah-na:dink’")
      , (15, "minłung-miwah-na:chwola’")
      , (16, "minłung-miwah-na:xosta:n")
      , (17, "minłung-miwah-na:xohk’it")
      , (18, "minłung-miwah-na:ke:nim")
      , (19, "minłung-miwah-na:miq’os-t’aw")
      , (20, "nahdiminłung")
      , (21, "nahdiminłung-miwah-na:ła’")
      , (22, "nahdiminłung-miwah-na:nahx")
      , (23, "nahdiminłung-miwah-na:ta:q’")
      , (24, "nahdiminłung-miwah-na:dink’")
      , (25, "nahdiminłung-miwah-na:chwola’")
      , (26, "nahdiminłung-miwah-na:xosta:n")
      , (27, "nahdiminłung-miwah-na:xohk’it")
      , (28, "nahdiminłung-miwah-na:ke:nim")
      , (29, "nahdiminłung-miwah-na:miq’os-t’aw")
      , (30, "ta:q’idiminłung")
      , (31, "ta:q’idiminłung-miwah-na:ła’")
      , (32, "ta:q’idiminłung-miwah-na:nahx")
      , (33, "ta:q’idiminłung-miwah-na:ta:q’")
      , (34, "ta:q’idiminłung-miwah-na:dink’")
      , (35, "ta:q’idiminłung-miwah-na:chwola’")
      , (36, "ta:q’idiminłung-miwah-na:xosta:n")
      , (37, "ta:q’idiminłung-miwah-na:xohk’it")
      , (38, "ta:q’idiminłung-miwah-na:ke:nim")
      , (39, "ta:q’idiminłung-miwah-na:miq’os-t’aw")
      , (40, "dink’idiminłung")
      , (41, "dink’idiminłung-miwah-na:ła’")
      , (42, "dink’idiminłung-miwah-na:nahx")
      , (43, "dink’idiminłung-miwah-na:ta:q’")
      , (44, "dink’idiminłung-miwah-na:dink’")
      , (45, "dink’idiminłung-miwah-na:chwola’")
      , (46, "dink’idiminłung-miwah-na:xosta:n")
      , (47, "dink’idiminłung-miwah-na:xohk’it")
      , (48, "dink’idiminłung-miwah-na:ke:nim")
      , (49, "dink’idiminłung-miwah-na:miq’os-t’aw")
      , (50, "chwola’diminłung")
      , (51, "chwola’diminłung-miwah-na:ła’")
      , (52, "chwola’diminłung-miwah-na:nahx")
      , (53, "chwola’diminłung-miwah-na:ta:q’")
      , (54, "chwola’diminłung-miwah-na:dink’")
      , (55, "chwola’diminłung-miwah-na:chwola’")
      , (56, "chwola’diminłung-miwah-na:xosta:n")
      , (57, "chwola’diminłung-miwah-na:xohk’it")
      , (58, "chwola’diminłung-miwah-na:ke:nim")
      , (59, "chwola’diminłung-miwah-na:miq’os-t’aw")
      , (60, "xosta:ndiminłung")
      , (61, "xosta:ndiminłung-miwah-na:ła’")
      , (62, "xosta:ndiminłung-miwah-na:nahx")
      , (63, "xosta:ndiminłung-miwah-na:ta:q’")
      , (64, "xosta:ndiminłung-miwah-na:dink’")
      , (65, "xosta:ndiminłung-miwah-na:chwola’")
      , (66, "xosta:ndiminłung-miwah-na:xosta:n")
      , (67, "xosta:ndiminłung-miwah-na:xohk’it")
      , (68, "xosta:ndiminłung-miwah-na:ke:nim")
      , (69, "xosta:ndiminłung-miwah-na:miq’os-t’aw")
      , (70, "xohk’e:diminłung")
      , (71, "xohk’e:diminłung-miwah-na:ła’")
      , (72, "xohk’e:diminłung-miwah-na:nahx")
      , (73, "xohk’e:diminłung-miwah-na:ta:q’")
      , (74, "xohk’e:diminłung-miwah-na:dink’")
      , (75, "xohk’e:diminłung-miwah-na:chwola’")
      , (76, "xohk’e:diminłung-miwah-na:xosta:n")
      , (77, "xohk’e:diminłung-miwah-na:xohk’it")
      , (78, "xohk’e:diminłung-miwah-na:ke:nim")
      , (79, "xohk’e:diminłung-miwah-na:miq’os-t’aw")
      , (80, "ke:nimdiminłung")
      , (81, "ke:nimdiminłung-miwah-na:ła’")
      , (82, "ke:nimdiminłung-miwah-na:nahx")
      , (83, "ke:nimdiminłung-miwah-na:ta:q’")
      , (84, "ke:nimdiminłung-miwah-na:dink’")
      , (85, "ke:nimdiminłung-miwah-na:chwola’")
      , (86, "ke:nimdiminłung-miwah-na:xosta:n")
      , (87, "ke:nimdiminłung-miwah-na:xohk’it")
      , (88, "ke:nimdiminłung-miwah-na:ke:nim")
      , (89, "ke:nimdiminłung-miwah-na:miq’os-t’aw")
      , (90, "miq’ost’ahdiminłung")
      , (91, "miq’ost’ahdiminłung-miwah-na:ła’")
      , (92, "miq’ost’ahdiminłung-miwah-na:nahx")
      , (93, "miq’ost’ahdiminłung-miwah-na:ta:q’")
      , (94, "miq’ost’ahdiminłung-miwah-na:dink’")
      , (95, "miq’ost’ahdiminłung-miwah-na:chwola’")
      , (96, "miq’ost’ahdiminłung-miwah-na:xosta:n")
      , (97, "miq’ost’ahdiminłung-miwah-na:xohk’it")
      , (98, "miq’ost’ahdiminłung-miwah-na:ke:nim")
      , (99, "miq’ost’ahdiminłung-miwah-na:miq’os-t’aw")
      , (100, "ła’-dikin")
      , (101, "ła’-dikin-miwah-na:ła’")
      , (102, "ła’-dikin-miwah-na:nahx")
      , (103, "ła’-dikin-miwah-na:ta:q’")
      , (104, "ła’-dikin-miwah-na:dink’")
      , (105, "ła’-dikin-miwah-na:chwola’")
      , (106, "ła’-dikin-miwah-na:xosta:n")
      , (107, "ła’-dikin-miwah-na:xohk’it")
      , (108, "ła’-dikin-miwah-na:ke:nim")
      , (109, "ła’-dikin-miwah-na:miq’os-t’aw")
      , (110, "ła’-dikin-miwah-na:minłung")
      , (123, "ła’-dikin-miwah-na:nahdiminłung-miwah-na:ta:q’")
      , (200, "nahx-dikin")
      , (300, "ta:q’i-dikin")
      , (321, "ta:q’-dikin-miwah-na:nahdiminłung-miwah-na:ła’")
      , (400, "dink’i-dikin")
      , (500, "chwola’-dikin")
      , (600, "xosta:n-dikin")
      , (700, "xohk’e-dikin")
      , (800, "ke:nim-dikin")
      , (900, "miq’ost’ah-dikin")
      , (909, "miq’os-t’aw-dikin-miwah-na:miq’os-t’aw")
      , (990, "miq’os-t’aw-dikin-miwah-na:miq’ost’ahdiminłung")
      , (999, "miq’os-t’aw-dikin-miwah-na:miq’ost’ahdiminłung-miwah-na:miq’os-t’aw")
      , (1000, "minłun-dikin")
      ]
    )
  ]
