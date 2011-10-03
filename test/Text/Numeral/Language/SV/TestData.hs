{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        sv

[@ISO639-2B@]       swe

[@ISO639-3@]        swe

[@Native name@]     svenska

[@English name@]    Swedish
-}

module Text.Numeral.Language.SV.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "numerals-base" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals-base" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "default"
    , defaultInflection
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
      , (100, "hundra")
      , (200, "tvåhundra")
      , (300, "trehundra")
      , (500, "femhundra")
      , (600, "sexhundra")
      , (800, "åttahundra")
      , (1000, "tusen")
      , (3000, "tretusen")
      , (3502, "tretusenfemhundratvå")
      , (5000, "femtusen")
      , (dec 4, "tiotusen")
      , (dec 5, "hundratusen")
      , (dec 6, "miljon")
      , (dec 9, "miljard")
      , (dec 12, "biljon")
      , (dec 15, "biljard")
      , (dec 18, "triljon")
      , (dec 24, "kvadriljon")
      , (dec 30, "kvintiljon")
      , (dec 36, "sextiljon")
      , (dec 42, "septiljon")
      , (dec 48, "oktiljon")
      , (dec 54, "noniljon")
      , (dec 60, "deciljon")
      , (dec 66, "undeciljon")
      ]
    )
  ]
