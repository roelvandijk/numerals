{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        zpl

[@Native name@]     East Sola de Vega Zapotec

[@English name@]    Lachixío Zapotec
-}
module Text.Numeral.Language.ZPL.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-lachixio-zapotec/en/zpl/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "tucu")
      , (2, "chiucu")
      , (3, "chuna")
      , (4, "tacu")
      , (5, "ayu’")
      , (6, "xu’cu")
      , (7, "achi")
      , (8, "xunu")
      , (9, "quie’")
      , (10, "chi’i")
      , (11, "chi’i tucu")
      , (12, "chi’i chiucu")
      , (13, "chi’i chuna")
      , (14, "chi’i tacu")
      , (15, "chi’i ayu’")
      , (16, "chi’i xu’cu")
      , (17, "chi’i achi")
      , (18, "chi’i xunu")
      , (19, "chi’i quie’")
      , (20, "ala")
      , (21, "ala tucu")
      , (22, "ala chiucu")
      , (23, "ala chuna")
      , (24, "ala tacu")
      , (25, "ala ayu’")
      , (26, "ala xu’cu")
      , (27, "ala achi")
      , (28, "ala xunu")
      , (29, "ala quie’")
      , (30, "ala llichi’i")
      , (31, "ala llichi’i tucu")
      , (32, "ala llichi’i chiucu")
      , (33, "ala llichi’i chuna")
      , (34, "ala llichi’i tacu")
      , (35, "ala llichi’i ayu’")
      , (36, "ala llichi’i xu’cu")
      , (37, "ala llichi’i achi")
      , (38, "ala llichi’i xunu")
      , (39, "ala llichi’i quie’")
      , (40, "chiu’a")
      , (41, "chiu’a tucu")
      , (42, "chiu’a chiucu")
      , (43, "chiu’a chuna")
      , (44, "chiu’a tacu")
      , (45, "chiu’a ayu’")
      , (46, "chiu’a xu’cu")
      , (47, "chiu’a achi")
      , (48, "chiu’a xunu")
      , (49, "chiu’a quie’")
      , (50, "chiu’a nu’ chi’i")
      , (51, "chiu’a nu’ chi’i tucu")
      , (52, "chiu’a nu’ chi’i chiucu")
      , (53, "chiu’a nu’ chi’i chuna")
      , (54, "chiu’a nu’ chi’i tacu")
      , (55, "chiu’a nu’ chi’i ayu’")
      , (56, "chiu’a nu’ chi’i xu’cu")
      , (57, "chiu’a nu’ chi’i achi")
      , (58, "chiu’a nu’ chi’i xunu")
      , (59, "chiu’a nu’ chi’i quie’")
      , (60, "ayuna")
      , (61, "ayuna tucu")
      , (62, "ayuna chiucu")
      , (63, "ayuna chuna")
      , (64, "ayuna tacu")
      , (65, "ayuna ayu’")
      , (66, "ayuna xu’cu")
      , (67, "ayuna achi")
      , (68, "ayuna xunu")
      , (69, "ayuna quie’")
      , (70, "ayuna nu’ chi’i")
      , (71, "ayuna nu’ chi’i tucu")
      , (72, "ayuna nu’ chi’i chiucu")
      , (73, "ayuna nu’ chi’i chuna")
      , (74, "ayuna nu’ chi’i tacu")
      , (75, "ayuna nu’ chi’i ayu’")
      , (76, "ayuna nu’ chi’i xu’cu")
      , (77, "ayuna nu’ chi’i achi")
      , (78, "ayuna nu’ chi’i xunu")
      , (79, "ayuna nu’ chi’i quie’")
      , (80, "tacu nu’ ala")
      , (81, "tacu nu’ ala tucu")
      , (82, "tacu nu’ ala chiucu")
      , (83, "tacu nu’ ala chuna")
      , (84, "tacu nu’ ala tacu")
      , (85, "tacu nu’ ala ayu’")
      , (86, "tacu nu’ ala xu’cu")
      , (87, "tacu nu’ ala achi")
      , (88, "tacu nu’ ala xunu")
      , (89, "tacu nu’ ala quie’")
      , (90, "chuna ala lli chi’i")
      , (91, "chuna ala lli chi’i tucu")
      , (92, "chuna ala lli chi’i chiucu")
      , (93, "chuna ala lli chi’i chuna")
      , (94, "chuna ala lli chi’i tacu")
      , (95, "chuna ala lli chi’i ayu’")
      , (96, "chuna ala lli chi’i xu’cu")
      , (97, "chuna ala lli chi’i achi")
      , (98, "chuna ala lli chi’i xunu")
      , (99, "chuna ala lli chi’i quie’")
      , (100, "tucu ayu’u")
      , (101, "tucu ayu’u tucu")
      , (102, "tucu ayu’u chiucu")
      , (103, "tucu ayu’u chuna")
      , (104, "tucu ayu’u tacu")
      , (105, "tucu ayu’u ayu’")
      , (106, "tucu ayu’u xu’cu")
      , (107, "tucu ayu’u achi")
      , (108, "tucu ayu’u xunu")
      , (109, "tucu ayu’u quie’")
      , (110, "tucu ayu’u chi’i")
      , (123, "tucu ayu’u ala chuna")
      , (200, "chiucu ayu’u")
      , (300, "chuna ayu’u")
      , (321, "chuna ayu’u ala tucu")
      , (400, "tacu ayu’u")
      , (500, "ayu’ ayu’u")
      , (600, "xu’cu ayu’u")
      , (700, "achi ayu’u")
      , (800, "xunu ayu’u")
      , (900, "quie’ ayu’u")
      , (909, "quie’ ayu’u quie’")
      , (990, "quie’ ayu’u chuna ala lli chi’i")
      , (999, "quie’ ayu’u chuna ala lli chi’i quie’")
      ]
    )
  ]
