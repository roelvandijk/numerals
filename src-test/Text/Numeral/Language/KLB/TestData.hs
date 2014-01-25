{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        klb

[@Native name@]     Koléew Ñaja'

[@English name@]    Kiliwa
-}
module Text.Numeral.Language.KLB.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-kiliwa/en/lkb/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "msig")
      , (2, "juwak")
      , (3, "jmi’k")
      , (4, "mnak")
      , (5, "salchipam")
      , (6, "msigl paayp")
      , (7, "juwakl paayp")
      , (8, "jmi’kl paayp")
      , (9, "msigl tmat")
      , (10, "chipam msig")
      , (11, "chipam msig, msig tmaljaa")
      , (12, "chipam msig, juwak tmaljaa")
      , (13, "chipam msig, jmi’k tmaljaa")
      , (14, "chipam msig, mnak tmaljaa")
      , (15, "chipam msig, salchipam tmaljaa")
      , (16, "chipam msig, msigl paayp tmaljaa")
      , (17, "chipam msig, juwakl paayp tmaljaa")
      , (18, "chipam msig, jmi’kl paayp tmaljaa")
      , (19, "chipam msig, msigl tmat tmaljaa")
      , (20, "chipam juwak")
      , (21, "chipam juwak, msig tmaljaa")
      , (22, "chipam juwak, juwak tmaljaa")
      , (23, "chipam juwak, jmi’k tmaljaa")
      , (24, "chipam juwak, mnak tmaljaa")
      , (25, "chipam juwak, salchipam tmaljaa")
      , (26, "chipam juwak, msigl paayp tmaljaa")
      , (27, "chipam juwak, juwakl paayp tmaljaa")
      , (28, "chipam juwak, jmi’kl paayp tmaljaa")
      , (29, "chipam juwak, msigl tmat tmaljaa")
      , (30, "chipam jmi’k")
      , (31, "chipam jmi’k, msig tmaljaa")
      , (32, "chipam jmi’k, juwak tmaljaa")
      , (33, "chipam jmi’k, jmi’k tmaljaa")
      , (34, "chipam jmi’k, mnak tmaljaa")
      , (35, "chipam jmi’k, salchipam tmaljaa")
      , (36, "chipam jmi’k, msigl paayp tmaljaa")
      , (37, "chipam jmi’k, juwakl paayp tmaljaa")
      , (38, "chipam jmi’k, jmi’kl paayp tmaljaa")
      , (39, "chipam jmi’k, msigl tmat tmaljaa")
      , (40, "chipam mnak")
      , (41, "chipam mnak, msig tmaljaa")
      , (42, "chipam mnak, juwak tmaljaa")
      , (43, "chipam mnak, jmi’k tmaljaa")
      , (44, "chipam mnak, mnak tmaljaa")
      , (45, "chipam mnak, salchipam tmaljaa")
      , (46, "chipam mnak, msigl paayp tmaljaa")
      , (47, "chipam mnak, juwakl paayp tmaljaa")
      , (48, "chipam mnak, jmi’kl paayp tmaljaa")
      , (49, "chipam mnak, msigl tmat tmaljaa")
      , (50, "chipam salchipam")
      , (51, "chipam salchipam, msig tmaljaa")
      , (52, "chipam salchipam, juwak tmaljaa")
      , (53, "chipam salchipam, jmi’k tmaljaa")
      , (54, "chipam salchipam, mnak tmaljaa")
      , (55, "chipam salchipam, salchipam tmaljaa")
      , (56, "chipam salchipam, msigl paayp tmaljaa")
      , (57, "chipam salchipam, juwakl paayp tmaljaa")
      , (58, "chipam salchipam, jmi’kl paayp tmaljaa")
      , (59, "chipam salchipam, msigl tmat tmaljaa")
      , (60, "chipam msigl paayp")
      , (61, "chipam msigl paayp, msig tmaljaa")
      , (62, "chipam msigl paayp, juwak tmaljaa")
      , (63, "chipam msigl paayp, jmi’k tmaljaa")
      , (64, "chipam msigl paayp, mnak tmaljaa")
      , (65, "chipam msigl paayp, salchipam tmaljaa")
      , (66, "chipam msigl paayp, msigl paayp tmaljaa")
      , (67, "chipam msigl paayp, juwakl paayp tmaljaa")
      , (68, "chipam msigl paayp, jmi’kl paayp tmaljaa")
      , (69, "chipam msigl paayp, msigl tmat tmaljaa")
      , (70, "chipam juwakl paayb")
      , (71, "chipam juwakl paayb, msig tmaljaa")
      , (72, "chipam juwakl paayb, juwak tmaljaa")
      , (73, "chipam juwakl paayb, jmi’k tmaljaa")
      , (74, "chipam juwakl paayb, mnak tmaljaa")
      , (75, "chipam juwakl paayb, salchipam tmaljaa")
      , (76, "chipam juwakl paayb, msigl paayp tmaljaa")
      , (77, "chipam juwakl paayb, juwakl paayp tmaljaa")
      , (78, "chipam juwakl paayb, jmi’kl paayp tmaljaa")
      , (79, "chipam juwakl paayb, msigl tmat tmaljaa")
      , (80, "chipam jmi’kl paayb")
      , (81, "chipam jmi’kl paayb, msig tmaljaa")
      , (82, "chipam jmi’kl paayb, juwak tmaljaa")
      , (83, "chipam jmi’kl paayb, jmi’k tmaljaa")
      , (84, "chipam jmi’kl paayb, mnak tmaljaa")
      , (85, "chipam jmi’kl paayb, salchipam tmaljaa")
      , (86, "chipam jmi’kl paayb, msigl paayp tmaljaa")
      , (87, "chipam jmi’kl paayb, juwakl paayp tmaljaa")
      , (88, "chipam jmi’kl paayb, jmi’kl paayp tmaljaa")
      , (89, "chipam jmi’kl paayb, msigl tmat tmaljaa")
      , (90, "chipam msigl tmat")
      , (91, "chipam msigl tmat, msig tmaljaa")
      , (92, "chipam msigl tmat, juwak tmaljaa")
      , (93, "chipam msigl tmat, jmi’k tmaljaa")
      , (94, "chipam msigl tmat, mnak tmaljaa")
      , (95, "chipam msigl tmat, salchipam tmaljaa")
      , (96, "chipam msigl tmat, msigl paayp tmaljaa")
      , (97, "chipam msigl tmat, juwakl paayp tmaljaa")
      , (98, "chipam msigl tmat, jmi’kl paayp tmaljaa")
      , (99, "chipam msigl tmat, msigl tmat tmaljaa")
      , (100, "chipam msig u’ kun yuu chipam msig")
      , (101, "chipam msig u’ kun yuu chipam msig, msig tmaljaa")
      , (102, "chipam msig u’ kun yuu chipam msig, juwak tmaljaa")
      , (103, "chipam msig u’ kun yuu chipam msig, jmi’k tmaljaa")
      , (104, "chipam msig u’ kun yuu chipam msig, mnak tmaljaa")
      , (105, "chipam msig u’ kun yuu chipam msig, salchipam tmaljaa")
      , (106, "chipam msig u’ kun yuu chipam msig, msigl paayp tmaljaa")
      , (107, "chipam msig u’ kun yuu chipam msig, juwakl paayp tmaljaa")
      , (108, "chipam msig u’ kun yuu chipam msig, jmi’kl paayp tmaljaa")
      , (109, "chipam msig u’ kun yuu chipam msig, msigl tmat tmaljaa")
      , (110, "chipam msig u’ kun yuu chipam msig, chipam msig")
      , (123, "chipam msig u’ kun yuu chipam msig, chipam juwak, jmi’k tmaljaa")
      , (200, "chipam msig u’ kun yuu chipam juwak")
      , (300, "chipam msig u’ kun yuu chipam jmi’k")
      , (321, "chipam msig u’ kun yuu chipam jmi’k, chipam juwak, msig tmaljaa")
      , (400, "chipam msig u’ kun yuu chipam mnak")
      , (500, "chipam msig u’ kun yuu chipam salchipam")
      , (600, "chipam msig u’ kun yuu chipam msigl paayp")
      , (700, "chipam msig u’ kun yuu chipam juwakl paayp")
      , (800, "chipam msig u’ kun yuu chipam jmi’kl paayp")
      , (900, "chipam msig u’ kun yuu chipam msigl tmat")
      , (909, "chipam msig u’ kun yuu chipam msigl tmat, msigl tmat tmaljaa")
      , (990, "chipam msig u’ kun yuu chipam msigl tmat, chipam msigl tmat")
      , (999, "chipam msig u’ kun yuu chipam msigl tmat, chipam msigl tmat, msigl tmat tmaljaa")
      , (1000, "chipam msig u’ kuetet msig")
      , (1001, "chipam msig u’ kuetet msig, msig tmaljaa")
      , (1008, "chipam msig u’ kuetet msig, jmi’kl paayp tmaljaa")
      , (1234, "chipam msig u’ kuetet msig, chipam msig u’ kun yuu chipam juwak, chipam jmi’k, mnak tmaljaa")
      , (2000, "chipam msig u’ kuetet juwak")
      , (3000, "chipam msig u’ kuetet jmi’k")
      , (4000, "chipam msig u’ kuetet mnak")
      , (4321, "chipam msig u’ kuetet mnak, chipam msig u’ kun yuu chipam jmi’k, chipam juwak, msig tmaljaa")
      , (5000, "chipam msig u’ kuetet salchipam")
      , (6000, "chipam msig u’ kuetet msigl paayp")
      , (7000, "chipam msig u’ kuetet juwakl paayp")
      , (8000, "chipam msig u’ kuetet jmi’kl paayp")
      , (9000, "chipam msig u’ kuetet msigl tmat")
      , (10000, "chipam msig u’ kuetet chipam msig")
      ]
    )
  ]
