{-|
[@ISO639-1@]        sv

[@ISO639-2B@]       swe

[@ISO639-3@]        swe

[@Native name@]     svenska

[@English name@]    Swedish
-}

module Text.Numeral.Language.SWE.TestData
    ( cardinals
    , ordinals
    , partitives
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "numerals" Text.Numeral.Grammar ( neuter, common )
import "numerals" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

-- Sources:
--   http://en.wikipedia.org/wiki/Swedish_grammar#Numerals
--   http://publications.europa.eu/code/sv/sv-4100600sv.htm
--   http://www.lysator.liu.se/language/Languages/Swedish/Grammar.html#numbers
--   http://en.wikibooks.org/wiki/Swedish/Numerals
--   http://www.speakswedish.co.uk/vocab/numbers
--   http://aclweb.org/anthology/C/C73/C73-2031.pdf
--
-- Rules for writing large words seem hard to come by online.
-- http://publications.europa.eu/code/sv/sv-4100600sv.htm provides
-- some guidelines:
--
-- Rule 1: Even hundreds, thousands, and so on, are generally written
-- as two words: två tusen, fyra hundra, sex miljoner
--
-- Rule 2: Numbers smaller than one hundred are always written as one
-- word, as are other number under one million: fyrtiosju,
-- niotusenfemhundrasexton
--
-- Rule 3: Numbers larger than one million should be written so that
-- the words tusen, miljon, miljard, and so on always stand on their
-- own: tjugofem miljarder tre miljoner

cardinals :: (Integral i) => TestData i
cardinals =
  [ ( "neuter"
    , neuter defaultInflection
      -- Rule 2 (one word)
    , [ (0, "noll")
      , (1, "ett")
      , (2, "två")
      , (3, "tre")
      , (4, "fyra")
      , (5, "fem")
      , (6, "sex")
      , (7, "sju")
      , (8, "åtta")
      , (9, "nio")
      , (10, "tio")
      , (11, "elva")
      , (12, "tolv")
      , (13, "tretton")
      , (14, "fjorton")
      , (15, "femton")
      , (16, "sexton")
      , (17, "sjutton")
      , (18, "arton")
      , (19, "nitton")
      , (20, "tjugo")
      , (21, "tjugoett")
      , (22, "tjugotvå")
      , (23, "tjugotre")
      , (24, "tjugofyra")
      , (25, "tjugofem")
      , (26, "tjugosex")
      , (27, "tjugosju")
      , (28, "tjugoåtta")
      , (29, "tjugonio")
      , (30, "trettio")
      , (31, "trettioett")
      , (32, "trettiotvå")
      , (33, "trettiotre")
      , (34, "trettiofyra")
      , (35, "trettiofem")
      , (36, "trettiosex")
      , (37, "trettiosju")
      , (38, "trettioåtta")
      , (39, "trettionio")
      , (40, "fyrtio")
      , (41, "fyrtioett")
      , (42, "fyrtiotvå")
      , (43, "fyrtiotre")
      , (44, "fyrtiofyra")
      , (45, "fyrtiofem")
      , (46, "fyrtiosex")
      , (47, "fyrtiosju")
      , (48, "fyrtioåtta")
      , (49, "fyrtionio")
      , (50, "femtio")
      , (51, "femtioett")
      , (52, "femtiotvå")
      , (53, "femtiotre")
      , (54, "femtiofyra")
      , (55, "femtiofem")
      , (56, "femtiosex")
      , (57, "femtiosju")
      , (58, "femtioåtta")
      , (59, "femtionio")
      , (60, "sextio")
      , (61, "sextioett")
      , (62, "sextiotvå")
      , (63, "sextiotre")
      , (64, "sextiofyra")
      , (65, "sextiofem")
      , (66, "sextiosex")
      , (67, "sextiosju")
      , (68, "sextioåtta")
      , (69, "sextionio")
      , (70, "sjuttio")
      , (71, "sjuttioett")
      , (72, "sjuttiotvå")
      , (73, "sjuttiotre")
      , (74, "sjuttiofyra")
      , (75, "sjuttiofem")
      , (76, "sjuttiosex")
      , (77, "sjuttiosju")
      , (78, "sjuttioåtta")
      , (79, "sjuttionio")
      , (80, "åttio")
      , (81, "åttioett")
      , (82, "åttiotvå")
      , (83, "åttiotre")
      , (84, "åttiofyra")
      , (85, "åttiofem")
      , (86, "åttiosex")
      , (87, "åttiosju")
      , (88, "åttioåtta")
      , (89, "åttionio")
      , (90, "nittio")
      , (91, "nittioett")
      , (92, "nittiotvå")
      , (93, "nittiotre")
      , (94, "nittiofyra")
      , (95, "nittiofem")
      , (96, "nittiosex")
      , (97, "nittiosju")
      , (98, "nittioåtta")
      , (99, "nittionio")
      , (101, "etthundraett")
      , (102, "etthundratvå")
      , (549, "femhundrafyrtionio")
        -- "ettusen" is a special spelling case where "ett tusen"
        -- written as one word becomes ettusen, dropping one 't'.
      , (1349, "ettusentrehundrafyrtionio")
      , (3502, "tretusenfemhundratvå")
      , (9516, "niotusenfemhundrasexton")
      , (30934, "trettiotusenniohundratrettiofyra")
      , (837132, "åttahundratrettiosjutusenetthundratrettiotvå")
      , (999999, "niohundranittioniotusenniohundranittionio")
        -- Rule 1 (even hundreds and so on)
      , (100, "ett hundra")
      , (200, "två hundra")
      , (300, "tre hundra")
      , (500, "fem hundra")
      , (600, "sex hundra")
      , (800, "åtta hundra")
      , (1000, "ett tusen")
      , (3000, "tre tusen")
      , (5000, "fem tusen")
      , (10000, "tio tusen")
      , (20000, "tjugo tusen")
      , (50000, "femtio tusen")
      , (dec 5, "etthundra tusen")
      , (dec 6, "en miljon")
      , (2 * dec 6, "två miljoner")
      , (10 * dec 6, "tio miljoner")
      , (dec 8, "etthundra miljoner")
      , (2 * dec 8, "tvåhundra miljoner")
      , (dec 9, "en miljard")
      , (dec 12, "en biljon")
      , (dec 15, "en biljard")
      , (dec 18, "en triljon")
      , (dec 24, "en kvadriljon")
      , (dec 30, "en kvintiljon")
      , (dec 36, "en sextiljon")
      , (dec 42, "en septiljon")
      , (dec 48, "en oktiljon")
      , (dec 54, "en noniljon")
      , (dec 60, "en deciljon")
      , (dec 66, "en undeciljon")
        -- Rule 3: Numbers larger than one million split up on tusen, miljon, etc
      , (123456789, "etthundratjugotre miljoner fyrahundrafemtiosex tusen sjuhundraåttionio")
      , (37149204, "trettiosju miljoner etthundrafyrtionio tusen tvåhundrafyra")
      , (302145568, "trehundratvå miljoner etthundrafyrtiofem tusen femhundrasextioåtta")
      , (1000100, "en miljon etthundra")
      , (1305100, "en miljon trehundrafem tusen etthundra")
      , (300000102, "trehundra miljoner etthundratvå")
      , (300500000, "trehundra miljoner femhundra tusen")
      , (25003000000, "tjugofem miljarder tre miljoner")
      ]
    )
  , ( "common"
    , common defaultInflection
    , [ (1, "en")
      , (21, "tjugoen")
      ]
    )
  ]

ordinals :: (Integral i) => TestData i
ordinals =
  [ ( "neuter"
    , neuter defaultInflection
    , [ (0, "nollte")
      , (1, "första")
      , (2, "andra")
      , (3, "tredje")
      , (4, "fjärde")
      , (5, "femte")
      , (6, "sjätte")
      , (7, "sjunde")
      , (8, "åttonde")
      , (9, "nionde")
      , (10, "tionde")
      , (11, "elfte")
      , (12, "tolfte")
      , (13, "trettonde")
      , (14, "fjortonde")
      , (15, "femtonde")
      , (16, "sextonde")
      , (17, "sjuttonde")
      , (18, "artonde")
      , (19, "nittonde")
      , (20, "tjugonde")
      , (21, "tjugoförsta")
      , (22, "tjugoandra")
      , (23, "tjugotredje")
      , (30, "trettionde")
      , (40, "fyrtionde")
      , (50, "femtionde")
      , (60, "sextionde")
      , (70, "sjuttionde")
      , (80, "åttionde")
      , (90, "nittionde")
      , (100, "ett hundrade")
      , (200, "två hundrade")
      , (500, "fem hundrade")
      , (1000, "ett tusende")
      , (2000, "två tusende")
      , (5000, "fem tusende")
      , (10000, "tio tusende")
      , (20000, "tjugo tusende")
      , (50000, "femtio tusende")
      , (dec 6, "en miljonte")
      , (dec 9, "en miljarte")
      , (dec 12, "en biljonte")
      , (dec 18, "en triljonte")
      ]
    )
  ]

partitives :: (Integral i) => TestData (i, i)
partitives =
  [ ( "default"
    , defaultInflection
    , [ ((1, 2), "en halv")
      , ((1, 3), "en tredjedel")
      , ((3, 4), "tre fjärdedelar")
      , ((2, 5), "två femtedelar")
      , ((5, 6), "fem sjättedelar")
      , ((4, 7), "fyra sjundedelar")
      , ((1, 8), "en åttondel")
      , ((8, 9), "åtta niondelar")
      , ((1, 10), "en tiondel")
      , ((1, 11), "en elftedel")
      , ((1, 12), "en tolftedel")
      , ((1, 13), "en trettondel")
      , ((1, 14), "en fjortondel")
      , ((1, 15), "en femtondel")
      , ((1, 16), "en sextondel")
      , ((1, 17), "en sjuttondel")
      , ((1, 18), "en artondel")
      , ((1, 19), "en nittondel")
      , ((1, 20), "en tjugondel")
      ]
    )
  ]
