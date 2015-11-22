{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        tar

[@Native name@]     Ralámuli ra'ícha

[@English name@]    Central Tarahumara
-}
module Text.Numeral.Language.TAR.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-central-tarahumara/en/tar/
  http://lingweb.eva.mpg.de/numeral/Tarahumara-Central.htm
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "biré")
      , (2, "ocuá")
      , (3, "biquiyá")
      , (4, "nahuosa")
      , (5, "marí")
      , (6, "usani")
      , (7, "quicháo")
      , (8, "osá nahuó") -- two fours
      , (9, "químacoy")  -- not ten
      , (10, "macoy")
      , (11, "macoy miná biré")
      , (12, "macoy miná ocuá")
      , (13, "macoy miná biquiyá")
      , (14, "macoy miná nahuosa")
      , (15, "macoy miná marí")
      , (16, "macoy miná usani")
      , (17, "macoy miná quicháo")
      , (18, "macoy miná osá nahuó")
      , (19, "macoy miná químacoy")
      , (20, "osá macoy")
      , (21, "osá macoy miná biré")
      , (22, "osá macoy miná ocuá")
      , (23, "osá macoy miná biquiyá")
      , (24, "osá macoy miná nahuosa")
      , (25, "osá macoy miná marí")
      , (26, "osá macoy miná usani")
      , (27, "osá macoy miná quicháo")
      , (28, "osá macoy miná osá nahuó")
      , (29, "osá macoy miná químacoy")
      , (30, "baisá macoy")
      , (31, "baisá macoy miná biré")
      , (32, "baisá macoy miná ocuá")
      , (33, "baisá macoy miná biquiyá")
      , (34, "baisá macoy miná nahuosa")
      , (35, "baisá macoy miná marí")
      , (36, "baisá macoy miná usani")
      , (37, "baisá macoy miná quicháo")
      , (38, "baisá macoy miná osá nahuó")
      , (39, "baisá macoy miná químacoy")
      , (40, "nahuosa macoy")
      , (41, "nahuosa macoy miná biré")
      , (42, "nahuosa macoy miná ocuá")
      , (43, "nahuosa macoy miná biquiyá")
      , (44, "nahuosa macoy miná nahuosa")
      , (45, "nahuosa macoy miná marí")
      , (46, "nahuosa macoy miná usani")
      , (47, "nahuosa macoy miná quicháo")
      , (48, "nahuosa macoy miná osá nahuó")
      , (49, "nahuosa macoy miná químacoy")
      , (50, "marisa macoy")
      , (51, "marisa macoy miná biré")
      , (52, "marisa macoy miná ocuá")
      , (53, "marisa macoy miná biquiyá")
      , (54, "marisa macoy miná nahuosa")
      , (55, "marisa macoy miná marí")
      , (56, "marisa macoy miná usani")
      , (57, "marisa macoy miná quicháo")
      , (58, "marisa macoy miná osá nahuó")
      , (59, "marisa macoy miná químacoy")
      , (60, "usansa macoy")
      , (61, "usansa macoy miná biré")
      , (62, "usansa macoy miná ocuá")
      , (63, "usansa macoy miná biquiyá")
      , (64, "usansa macoy miná nahuosa")
      , (65, "usansa macoy miná marí")
      , (66, "usansa macoy miná usani")
      , (67, "usansa macoy miná quicháo")
      , (68, "usansa macoy miná osá nahuó")
      , (69, "usansa macoy miná químacoy")
      , (70, "quicháosa macoy")
      , (71, "quicháosa macoy miná biré")
      , (72, "quicháosa macoy miná ocuá")
      , (73, "quicháosa macoy miná biquiyá")
      , (74, "quicháosa macoy miná nahuosa")
      , (75, "quicháosa macoy miná marí")
      , (76, "quicháosa macoy miná usani")
      , (77, "quicháosa macoy miná quicháo")
      , (78, "quicháosa macoy miná osá nahuó")
      , (79, "quicháosa macoy miná químacoy")
        -- The "osá naósa" is from Lowland Tarahumara, not Central
        -- Tarahumara. But this is the only available source.
      , (80, "osá nahuosa macoy")
      , (81, "osá nahuosa macoy miná biré")
      , (82, "osá nahuosa macoy miná ocuá")
      , (83, "osá nahuosa macoy miná biquiyá")
      , (84, "osá nahuosa macoy miná nahuosa")
      , (85, "osá nahuosa macoy miná marí")
      , (86, "osá nahuosa macoy miná usani")
      , (87, "osá nahuosa macoy miná quicháo")
      , (88, "osá nahuosa macoy miná osá nahuó")
      , (89, "osá nahuosa macoy miná químacoy")
      , (90, "que macoisa macoy")
      , (91, "que macoisa macoy miná biré")
      , (92, "que macoisa macoy miná ocuá")
      , (93, "que macoisa macoy miná biquiyá")
      , (94, "que macoisa macoy miná nahuosa")
      , (95, "que macoisa macoy miná marí")
      , (96, "que macoisa macoy miná usani")
      , (97, "que macoisa macoy miná quicháo")
      , (98, "que macoisa macoy miná osá nahuó")
      , (99, "que macoisa macoy miná químacoy")
      , (100, "biré ciento")
      ]
    )
  ]
