{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        zpc

[@Native name@]     -

[@English name@]    Choapan Zapotec
-}
module Text.Numeral.Language.ZPC.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-choapan-zapotec/en/zpc/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "tu")
      , (2, "chopa")
      , (3, "tzona")
      , (4, "tapa")
      , (5, "ga’yo’")
      , (6, "xopa")
      , (7, "gadyi")
      , (8, "xuna’")
      , (9, "ga")
      , (10, "chi")
      , (11, "chiptu")
      , (12, "chipchopa")
      , (13, "chi’nnu")
      , (14, "chi’da’")
      , (15, "chi’no’")
      , (16, "chi’no’ be tu")
      , (17, "chi’no’ be chopa")
      , (18, "chi’no be tzona")
      , (19, "tu galo")
      , (20, "galo")
      , (21, "galo be tu")
      , (22, "galo be chopa")
      , (23, "galo be tzona")
      , (24, "galo be tapa")
      , (25, "galo be ga’yo’")
      , (26, "galo be xopa")
      , (27, "galo be gadyi")
      , (28, "galo be xuna’")
      , (29, "galo be ga")
      , (30, "galo be chi")
      , (31, "galo be chi tu")
      , (32, "galo be chi chopa")
      , (33, "galo be chi tzona")
      , (34, "galo be chi tapa")
      , (35, "galo be chi ga’yo’")
      , (36, "galo be chi xopa")
      , (37, "galo be chi gadyi")
      , (38, "galo be chi xuna’")
      , (39, "galo be chi ga")
      , (40, "chopa galo")
      , (41, "chopa galo tu")
      , (42, "chopa galo chopa")
      , (43, "chopa galo tzona")
      , (44, "chopa galo tapa")
      , (45, "chopa galo ga’yo’")
      , (46, "chopa galo xopa")
      , (47, "chopa galo gadyi")
      , (48, "chopa galo xuna’")
      , (49, "chopa galo ga")
      , (50, "chopa galo chi")
      , (51, "chopa galo chi tu")
      , (52, "chopa galo chi chopa")
      , (53, "chopa galo chi tzona")
      , (54, "chopa galo chi tapa")
      , (55, "chopa galo chi ga’yo’")
      , (56, "chopa galo chi xopa")
      , (57, "chopa galo chi gadyi")
      , (58, "chopa galo chi xuna’")
      , (59, "chopa galo chi ga")
      , (60, "tzona galo")
      , (61, "tzona galo tu")
      , (62, "tzona galo chopa")
      , (63, "tzona galo tzona")
      , (64, "tzona galo tapa")
      , (65, "tzona galo ga’yo’")
      , (66, "tzona galo xopa")
      , (67, "tzona galo gadyi")
      , (68, "tzona galo xuna’")
      , (69, "tzona galo ga")
      , (70, "tzona galo chi")
      , (71, "tzona galo chi tu")
      , (72, "tzona galo chi chopa")
      , (73, "tzona galo chi tzona")
      , (74, "tzona galo chi tapa")
      , (75, "tzona galo chi ga’yo’")
      , (76, "tzona galo chi xopa")
      , (77, "tzona galo chi gadyi")
      , (78, "tzona galo chi xuna’")
      , (79, "tzona galo chi ga")
      , (80, "tapa galo")
      , (81, "tapa galo tu")
      , (82, "tapa galo chopa")
      , (83, "tapa galo tzona")
      , (84, "tapa galo tapa")
      , (85, "tapa galo ga’yo’")
      , (86, "tapa galo xopa")
      , (87, "tapa galo gadyi")
      , (88, "tapa galo xuna’")
      , (89, "tapa galo ga")
      , (90, "tapa galo chi")
      , (91, "tapa galo chi tu")
      , (92, "tapa galo chi chopa")
      , (93, "tapa galo chi tzona")
      , (94, "tapa galo chi tapa")
      , (95, "tapa galo chi ga’yo’")
      , (96, "tapa galo chi xopa")
      , (97, "tapa galo chi gadyi")
      , (98, "tapa galo chi xuna’")
      , (99, "tapa galo chi ga")
      , (100, "tu gayua")
      , (101, "tu gayua tu")
      , (102, "tu gayua chopa")
      , (103, "tu gayua tzona")
      , (104, "tu gayua tapa")
      , (105, "tu gayua ga’yo’")
      , (106, "tu gayua xopa")
      , (107, "tu gayua gadyi")
      , (108, "tu gayua xuna’")
      , (109, "tu gayua ga")
      , (110, "tu gayua chi")
      , (123, "tu gayua galo be tzona")
      , (200, "chopa gayua")
      , (300, "tzona gayua")
      , (321, "tzona gayua galo be tu")
      , (400, "tapa gayua")
      , (500, "ga’yo’ gayua")
      , (600, "xopa gayua")
      , (700, "gadyi gayua")
      , (800, "xuna’ gayua")
      , (900, "ga gayua")
      , (909, "ga gayua ga")
      , (990, "ga gayua tapa galo chi")
      , (999, "ga gayua tapa galo chi ga")
      ]
    )
  ]
