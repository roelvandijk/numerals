{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        frr

[@ISO639-3@]        frr

[@Native name@]     Frasch, Fresk, Freesk, Friisk

[@English name@]    North Frisian
-}
module Text.Numeral.Language.FRR.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-north-frisian/en/frr/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "iinj")
      , (2, "tou")
      , (3, "tri")
      , (4, "fjouer")
      , (5, "fiiw")
      , (6, "seeks")
      , (7, "soowen")
      , (8, "oocht")
      , (9, "nüügen")
      , (10, "tiin")
      , (11, "alwen")
      , (12, "tweelwen")
      , (13, "tratäin")
      , (14, "fjouertäin")
      , (15, "füftäin")
      , (16, "seekstäin")
      , (17, "soowentäin")
      , (18, "oochttäin")
      , (19, "nüügentäin")
      , (20, "twunti")
      , (21, "iinjäntwunti")
      , (22, "touäntwunti")
      , (23, "triäntwunti")
      , (24, "fjoueräntwunti")
      , (25, "fiiwäntwunti")
      , (26, "seeksäntwunti")
      , (27, "soowenäntwunti")
      , (28, "oochtäntwunti")
      , (29, "nüügenäntwunti")
      , (30, "dörti")
      , (31, "iinjändörti")
      , (32, "touändörti")
      , (33, "triändörti")
      , (34, "fjouerändörti")
      , (35, "fiiwändörti")
      , (36, "seeksändörti")
      , (37, "soowenändörti")
      , (38, "oochtändörti")
      , (39, "nüügenändörti")
      , (40, "fäärti")
      , (41, "iinjänfäärti")
      , (42, "touänfäärti")
      , (43, "triänfäärti")
      , (44, "fjoueränfäärti")
      , (45, "fiiwänfäärti")
      , (46, "seeksänfäärti")
      , (47, "soowenänfäärti")
      , (48, "oochtänfäärti")
      , (49, "nüügenänfäärti")
      , (50, "füfti")
      , (51, "iinjänfüfti")
      , (52, "touänfüfti")
      , (53, "triänfüfti")
      , (54, "fjoueränfüfti")
      , (55, "fiiwänfüfti")
      , (56, "seeksänfüfti")
      , (57, "soowenänfüfti")
      , (58, "oochtänfüfti")
      , (59, "nüügenänfüfti")
      , (60, "süsti")
      , (61, "iinjänsüsti")
      , (62, "touänsüsti")
      , (63, "triänsüsti")
      , (64, "fjoueränsüsti")
      , (65, "fiiwänsüsti")
      , (66, "seeksänsüsti")
      , (67, "soowenänsüsti")
      , (68, "oochtänsüsti")
      , (69, "nüügenänsüsti")
      , (70, "sööwenti")
      , (71, "iinjänsööwenti")
      , (72, "touänsööwenti")
      , (73, "triänsööwenti")
      , (74, "fjoueränsööwenti")
      , (75, "fiiwänsööwenti")
      , (76, "seeksänsööwenti")
      , (77, "soowenänsööwenti")
      , (78, "oochtänsööwenti")
      , (79, "nüügenänsööwenti")
      , (80, "tachenti")
      , (81, "iinjäntachenti")
      , (82, "touäntachenti")
      , (83, "triäntachenti")
      , (84, "fjoueräntachenti")
      , (85, "fiiwäntachenti")
      , (86, "seeksäntachenti")
      , (87, "soowenäntachenti")
      , (88, "oochtäntachenti")
      , (89, "nüügenäntachenti")
      , (90, "näägenti")
      , (91, "iinjännäägenti")
      , (92, "touännäägenti")
      , (93, "triännäägenti")
      , (94, "fjouerännäägenti")
      , (95, "fiiwännäägenti")
      , (96, "seeksännäägenti")
      , (97, "soowenännäägenti")
      , (98, "oochtännäägenti")
      , (99, "nüügenännäägenti")
      , (100, "hunert")
      , (101, "hunert iinj")
      , (102, "hunert tou")
      , (103, "hunert tri")
      , (104, "hunert fjouer")
      , (105, "hunert fiiw")
      , (106, "hunert seeks")
      , (107, "hunert soowen")
      , (108, "hunert oocht")
      , (109, "hunert nüügen")
      , (110, "hunert tiin")
      , (123, "hunert triäntwunti")
      , (200, "touhunert")
      , (300, "trihunert")
      , (321, "trihunert iinjäntwunti")
      , (400, "fjouerhunert")
      , (500, "fiiwhunert")
      , (600, "seekshunert")
      , (700, "soowenhunert")
      , (800, "oochthunert")
      , (900, "nüügenhunert")
      , (909, "nüügenhunert nüügen")
      , (990, "nüügenhunert näägenti")
      , (999, "nüügenhunert nüügenännäägenti")
      , (1000, "duusend")
      , (1001, "duusend iinj")
      , (1008, "duusend oocht")
      , (1234, "duusend touhunert fjouerändörti")
      , (2000, "touduusend")
      , (3000, "triduusend")
      , (4000, "fjouerduusend")
      , (4321, "fjouerduusend trihunert iinjäntwunti")
      , (5000, "fiiwduusend")
      , (6000, "seeksduusend")
      , (7000, "soowenduusend")
      , (8000, "oochtduusend")
      , (9000, "nüügenduusend")
      , (10000, "tiinduusend")
      , (12345, "tweelwen duusend trihunert fiiwänfäärti")
      , (20000, "twunti duusend")
      , (30000, "dörti duusend")
      , (40000, "fäärti duusend")
      , (50000, "füfti duusend")
      , (54321, "fjoueränfüfti duusend trihunert iinjäntwunti")
      , (60000, "süsti duusend")
      , (70000, "sööwenti duusend")
      , (80000, "tachenti duusend")
      , (90000, "näägenti duusend")
      , (100000, "hunertduusend")
      , (123456, "hunert triäntwunti duusend fjouerhunert seeksänfüfti")
      , (200000, "touhunertduusend")
      , (300000, "trihunertduusend")
      , (400000, "fjouerhunertduusend")
      , (500000, "fiiwhunertduusend")
      , (600000, "seekshunertduusend")
      , (654321, "seekshunert fjoueränfüfti duusend trihunert iinjäntwunti")
      , (700000, "soowenhunertduusend")
      , (800000, "oochthunertduusend")
      , (900000, "nüügenhunertduusend")
      ]
    )
  ]
