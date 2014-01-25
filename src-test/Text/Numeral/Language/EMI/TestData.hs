{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        emi

[@Native name@]     -

[@English name@]    Mussau-Emira
-}
module Text.Numeral.Language.EMI.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-mussau-emira/en/emi/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "sesa")
      , (2, "lua")
      , (3, "tolu")
      , (4, "ata")
      , (5, "lima")
      , (6, "nomo")
      , (7, "itu")
      , (8, "oalu")
      , (9, "sio")
      , (10, "sangaulu")
      , (11, "sangaulu sesa")
      , (12, "sangaulu lua")
      , (13, "sangaulu tolu")
      , (14, "sangaulu ata")
      , (15, "sangaulu lima")
      , (16, "sangaulu nomo")
      , (17, "sangaulu itu")
      , (18, "sangaulu oalu")
      , (19, "sangaulu sio")
      , (20, "luengaulu")
      , (21, "luengaulu sesa")
      , (22, "luengaulu lua")
      , (23, "luengaulu tolu")
      , (24, "luengaulu ata")
      , (25, "luengaulu lima")
      , (26, "luengaulu nomo")
      , (27, "luengaulu itu")
      , (28, "luengaulu oalu")
      , (29, "luengaulu sio")
      , (30, "tolungaulu")
      , (31, "tolungaulu sesa")
      , (32, "tolungaulu lua")
      , (33, "tolungaulu tolu")
      , (34, "tolungaulu ata")
      , (35, "tolungaulu lima")
      , (36, "tolungaulu nomo")
      , (37, "tolungaulu itu")
      , (38, "tolungaulu oalu")
      , (39, "tolungaulu sio")
      , (40, "atingaulu")
      , (41, "atingaulu sesa")
      , (42, "atingaulu lua")
      , (43, "atingaulu tolu")
      , (44, "atingaulu ata")
      , (45, "atingaulu lima")
      , (46, "atingaulu nomo")
      , (47, "atingaulu itu")
      , (48, "atingaulu oalu")
      , (49, "atingaulu sio")
      , (50, "limangaulu")
      , (51, "limangaulu sesa")
      , (52, "limangaulu lua")
      , (53, "limangaulu tolu")
      , (54, "limangaulu ata")
      , (55, "limangaulu lima")
      , (56, "limangaulu nomo")
      , (57, "limangaulu itu")
      , (58, "limangaulu oalu")
      , (59, "limangaulu sio")
      , (60, "nomongaulu")
      , (61, "nomongaulu sesa")
      , (62, "nomongaulu lua")
      , (63, "nomongaulu tolu")
      , (64, "nomongaulu ata")
      , (65, "nomongaulu lima")
      , (66, "nomongaulu nomo")
      , (67, "nomongaulu itu")
      , (68, "nomongaulu oalu")
      , (69, "nomongaulu sio")
      , (70, "itungaulu")
      , (71, "itungaulu sesa")
      , (72, "itungaulu lua")
      , (73, "itungaulu tolu")
      , (74, "itungaulu ata")
      , (75, "itungaulu lima")
      , (76, "itungaulu nomo")
      , (77, "itungaulu itu")
      , (78, "itungaulu oalu")
      , (79, "itungaulu sio")
      , (80, "oalungaulu")
      , (81, "oalungaulu sesa")
      , (82, "oalungaulu lua")
      , (83, "oalungaulu tolu")
      , (84, "oalungaulu ata")
      , (85, "oalungaulu lima")
      , (86, "oalungaulu nomo")
      , (87, "oalungaulu itu")
      , (88, "oalungaulu oalu")
      , (89, "oalungaulu sio")
      , (90, "siongaulu")
      , (91, "siongaulu sesa")
      , (92, "siongaulu lua")
      , (93, "siongaulu tolu")
      , (94, "siongaulu ata")
      , (95, "siongaulu lima")
      , (96, "siongaulu nomo")
      , (97, "siongaulu itu")
      , (98, "siongaulu oalu")
      , (99, "siongaulu sio")
      , (100, "ai")
      , (101, "ai sesa")
      , (102, "ai lua")
      , (103, "ai tolu")
      , (104, "ai ata")
      , (105, "ai lima")
      , (106, "ai nomo")
      , (107, "ai itu")
      , (108, "ai oalu")
      , (109, "ai sio")
      , (110, "ai sangaulu")
      , (123, "ai luengaulu tolu")
      , (200, "lua ai")
      , (300, "tolu ai")
      , (321, "tolu ai luengaulu sesa")
      , (400, "ata ai")
      , (500, "lima ai")
      , (600, "nomo ai")
      , (700, "itu ai")
      , (800, "oalu ai")
      , (900, "sio ai")
      , (909, "sio ai sio")
      , (990, "sio ai siongaulu")
      , (999, "sio ai siongaulu sio")
      , (1000, "airari")
      , (1001, "airari sesa")
      , (1008, "airari oalu")
      , (1234, "airari lua ai tolungaulu ata")
      , (2000, "lua airari")
      , (3000, "tolu airari")
      , (4000, "ata airari")
      , (4321, "ata airari tolu ai luengaulu sesa")
      , (5000, "lima airari")
      , (6000, "nomo airari")
      , (7000, "itu airari")
      , (8000, "oalu airari")
      , (9000, "sio airari")
      ]
    )
  ]
