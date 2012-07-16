{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        hr

[@ISO639-2@]        hrv

[@ISO639-3@]        hrv

[@Native name@]     Hrvatski

[@English name@]    Croatian
-}

module Text.Numeral.Language.HR.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "numerals-base" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- HR - Croatian
--------------------------------------------------------------------------------

cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "nula")
      , (1, "jedan")
      , (2, "dva")
      , (3, "tri")
      , (4, "cetiri")
      , (5, "pet")
      , (6, "šest")
      , (7, "sedam")
      , (8, "osam")
      , (9, "devet")
      , (10, "deset")
      , (11, "jedanaest")
      , (12, "dvanaest")
      , (13, "trinaest")
      , (14, "cetrnaest")
      , (15, "petnaest")
      , (16, "šesnaest")
      , (17, "sedamnaest")
      , (18, "osamnaest")
      , (19, "devetnaest")
      , (20, "dvadeset")
      , (21, "dvadeseti jedan")
      , (22, "dvadeseti dva")
      , (23, "dvadeseti tri")
      , (24, "dvadeseti cetiri")
      , (25, "dvadeseti pet")
      , (26, "dvadeseti šest")
      , (27, "dvadeseti sedam")
      , (28, "dvadeseti osam")
      , (29, "dvadeseti devet")
      , (30, "drideset")
      , (31, "drideseti jedan")
      , (32, "drideseti dva")
      , (33, "drideseti tri")
      , (34, "drideseti cetiri")
      , (35, "drideseti pet")
      , (36, "drideseti šest")
      , (37, "drideseti sedam")
      , (38, "drideseti osam")
      , (39, "drideseti devet")
      , (40, "cetrdeset")
      , (41, "cetrdeseti jedan")
      , (42, "cetrdeseti dva")
      , (43, "cetrdeseti tri")
      , (44, "cetrdeseti cetiri")
      , (45, "cetrdeseti pet")
      , (46, "cetrdeseti šest")
      , (47, "cetrdeseti sedam")
      , (48, "cetrdeseti osam")
      , (49, "cetrdeseti devet")
      , (50, "pedeset")
      , (51, "pedeseti jedan")
      , (52, "pedeseti dva")
      , (53, "pedeseti tri")
      , (54, "pedeseti cetiri")
      , (55, "pedeseti pet")
      , (56, "pedeseti šest")
      , (57, "pedeseti sedam")
      , (58, "pedeseti osam")
      , (59, "pedeseti devet")
      , (60, "šezdezet")
      , (61, "šezdezeti jedan")
      , (62, "šezdezeti dva")
      , (63, "šezdezeti tri")
      , (64, "šezdezeti cetiri")
      , (65, "šezdezeti pet")
      , (66, "šezdezeti šest")
      , (67, "šezdezeti sedam")
      , (68, "šezdezeti osam")
      , (69, "šezdezeti devet")
      , (70, "sedamdezet")
      , (71, "sedamdezeti jedan")
      , (72, "sedamdezeti dva")
      , (73, "sedamdezeti tri")
      , (74, "sedamdezeti cetiri")
      , (75, "sedamdezeti pet")
      , (76, "sedamdezeti šest")
      , (77, "sedamdezeti sedam")
      , (78, "sedamdezeti osam")
      , (79, "sedamdezeti devet")
      , (80, "osamdeset")
      , (81, "osamdeseti jedan")
      , (82, "osamdeseti dva")
      , (83, "osamdeseti tri")
      , (84, "osamdeseti cetiri")
      , (85, "osamdeseti pet")
      , (86, "osamdeseti šest")
      , (87, "osamdeseti sedam")
      , (88, "osamdeseti osam")
      , (89, "osamdeseti devet")
      , (90, "deveteset")
      , (91, "deveteseti jedan")
      , (92, "deveteseti dva")
      , (93, "deveteseti tri")
      , (94, "deveteseti cetiri")
      , (95, "deveteseti pet")
      , (96, "deveteseti šest")
      , (97, "deveteseti sedam")
      , (98, "deveteseti osam")
      , (99, "deveteseti devet")
      , (100, "sto")
      ]
    )
  ]
