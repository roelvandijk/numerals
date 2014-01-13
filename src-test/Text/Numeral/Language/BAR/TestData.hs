{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        bar

[@Native name@]     -

[@English name@]    Bavarian
-}
module Text.Numeral.Language.BAR.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-bavarian/en/bar/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "oas")
      , (2, "zwoa")
      , (3, "drei")
      , (4, "fiare")
      , (5, "fimfe")
      , (6, "sechse")
      , (7, "sieme")
      , (8, "åchte")
      , (9, "neine")
      , (10, "zene")
      , (11, "öife")
      , (12, "zwöife")
      , (13, "dreizea")
      , (14, "fiazea")
      , (15, "fuchzea")
      , (16, "sechzea")
      , (17, "sibzea")
      , (18, "åchzea")
      , (19, "neizea")
      , (20, "zwånzge")
      , (21, "oanazwånzge")
      , (22, "zwoarazwånzge")
      , (23, "dreiazwånzge")
      , (24, "fiarazwånzge")
      , (25, "fimfazwånzge")
      , (26, "sechsazwånzge")
      , (27, "simmazwånzge")
      , (28, "åchtazwånzge")
      , (29, "neinazwånzge")
      , (30, "dreißge")
      , (31, "oanadreißge")
      , (32, "zwoaradreißge")
      , (33, "dreiadreißge")
      , (34, "fiaradreißge")
      , (35, "fimfadreißge")
      , (36, "sechsadreißge")
      , (37, "simmadreißge")
      , (38, "åchtadreißge")
      , (39, "neinadreißge")
      , (40, "fiazge")
      , (41, "oanafiazge")
      , (42, "zwoarafiazge")
      , (43, "dreiafiazge")
      , (44, "fiarafiazge")
      , (45, "fimfafiazge")
      , (46, "sechsafiazge")
      , (47, "simmafiazge")
      , (48, "åchtafiazge")
      , (49, "neinafiazge")
      , (50, "fuchzge")
      , (51, "oanafuchzge")
      , (52, "zwoarafuchzge")
      , (53, "dreiafuchzge")
      , (54, "fiarafuchzge")
      , (55, "fimfafuchzge")
      , (56, "sechsafuchzge")
      , (57, "simmafuchzge")
      , (58, "åchtafuchzge")
      , (59, "neinafuchzge")
      , (60, "sechzge")
      , (61, "oanasechzge")
      , (62, "zwoarasechzge")
      , (63, "dreiasechzge")
      , (64, "fiarasechzge")
      , (65, "fimfasechzge")
      , (66, "sechsasechzge")
      , (67, "simmasechzge")
      , (68, "åchtasechzge")
      , (69, "neinasechzge")
      , (70, "sibzge")
      , (71, "oanasibzge")
      , (72, "zwoarasibzge")
      , (73, "dreiasibzge")
      , (74, "fiarasibzge")
      , (75, "fimfasibzge")
      , (76, "sechsasibzge")
      , (77, "simmasibzge")
      , (78, "åchtasibzge")
      , (79, "neinasibzge")
      , (80, "åchtzge")
      , (81, "oanaåchtzge")
      , (82, "zwoaraåchtzge")
      , (83, "dreiaåchtzge")
      , (84, "fiaraåchtzge")
      , (85, "fimfaåchtzge")
      , (86, "sechsaåchtzge")
      , (87, "simmaåchtzge")
      , (88, "åchtaåchtzge")
      , (89, "neinaåchtzge")
      , (90, "neinzge")
      , (91, "oananeinzge")
      , (92, "zwoaraneinzge")
      , (93, "dreianeinzge")
      , (94, "fiaraneinzge")
      , (95, "fimfaneinzge")
      , (96, "sechsaneinzge")
      , (97, "simmaneinzge")
      , (98, "åchtaneinzge")
      , (99, "neinaneinzge")
      , (100, "hundad")
      , (101, "hundad oas")
      , (102, "hundad zwoa")
      , (103, "hundad drei")
      , (104, "hundad fiare")
      , (105, "hundad fimfe")
      , (106, "hundad sechse")
      , (107, "hundad sieme")
      , (108, "hundad åchte")
      , (109, "hundad neine")
      , (110, "hundad zene")
      , (123, "hundad dreiazwånzge")
      , (200, "zwoahundad")
      , (300, "dreihundad")
      , (321, "dreihundad oanazwånzge")
      , (400, "fiahundad")
      , (500, "fimfhundad")
      , (600, "sechshundad")
      , (700, "simhundad")
      , (800, "åchthundad")
      , (900, "neihundad")
      , (909, "neihundad neine")
      , (990, "neihundad neinzge")
      , (999, "neihundad neinaneinzge")
      , (1000, "dausnd")
      ]
    )
  ]
