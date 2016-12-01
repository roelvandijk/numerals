{-|
[@ISO639-1@]        hr

[@ISO639-2B@]       cro

[@ISO639-3@]        cro

[@Native name@]     Hrvatski

[@English name@]    Croatian
-}


module Text.Numeral.Language.CRO.TestData
    ( cardinals
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Monoid ( (<>) )
import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

cardinals :: (Num i) => TestData i
cardinals =
  [ ("default"
    , defaultInflection
    , [ (0, "nula")
      , (1, "jedan")
      , (2, "dva")
      , (3, "tri")
      , (4, "četiri")
      , (5, "pet")
      , (6, "šest")
      , (7, "sedam")
      , (8, "osam")
      , (9, "devet")
      , (10, "deset")
      , (11, "jedanaest")
      , (12, "dvanaest")
      , (13, "trinaest")
      , (14, "četrnaest")
      , (15, "petnaest")
      , (16, "šesnaest")
      , (17, "sedamnaest")
      , (18, "osamnaest")
      , (19, "devetnaest")
      , (20, "dvadeset")
      , (21, "dvadeset jedan")
      , (22, "dvadeset dva")
      , (23, "dvadeset tri")
      , (24, "dvadeset četiri")
      , (25, "dvadeset pet")
      , (26, "dvadeset šest")
      , (27, "dvadeset sedam")
      , (28, "dvadeset osam")
      , (29, "dvadeset devet")
      , (30, "trideset")
      , (31, "trideset jedan")
      , (32, "trideset dva")
      , (33, "trideset tri")
      , (34, "trideset četiri")
      , (35, "trideset pet")
      , (36, "trideset šest")
      , (37, "trideset sedam")
      , (38, "trideset osam")
      , (39, "trideset devet")
      , (40, "četrdeset")
      , (50, "pedeset")
      , (60, "šesdeset")
      , (62, "šesdeset dva")
      , (70, "sedamdeset")
      , (80, "osamdeset")
      , (90, "devedeset")
      , (93, "devedeset tri")
      , (98, "devedeset osam")
      , (99, "devedeset devet")
      , (100, "sto")
      , (101, "sto jedan")
      , (102, "sto dva")
      , (110, "sto deset")
      , (111, "sto jedanaest")
      , (116, "sto šesnaest")
      , (126, "sto dvadeset šest")
      , (150, "sto pedeset")
      , (156, "sto pedeset šest")
      , (500, "petsto")
      , (634, "šesto trideset četiri")
      , (999, "devetsto devedeset devet")
      , (1000, "tisuću")
      , (1100, "tisuću sto")
      , (1101, "tisuću sto jedan")
      , (1102, "tisuću sto dva")
      , (1110, "tisuću sto deset")
      , (1111, "tisuću sto jedanaest")
      , (1116, "tisuću sto šesnaest")
      , (1126, "tisuću sto dvadeset šest")
      , (1150, "tisuću sto pedeset")
      , (1156, "tisuću sto pedeset šest")
      , (1500, "tisuću petsto")
      , (1634, "tisuću šesto trideset četiri")
      , (17999, "sedamnaest tisuća devetsto devedeset devet")
      , (25358, "dvadeset pet tisuća tristo pedeset osam")
      , (105358, "sto pet tisuća tristo pedeset osam")
      , (246912, "dvjesto četrdeset šest tisuća devetsto dvanaest")
      ]
    )
  ]
