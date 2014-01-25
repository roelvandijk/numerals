{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        ln

[@ISO639-2@]        lin

[@ISO639-3@]        lin

[@Native name@]     Ngala

[@English name@]    Lingala
-}
module Text.Numeral.Language.LN.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-lingala/en/lin/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "libungutulu")
      , (1, "mókó")
      , (2, "míbalé")
      , (3, "mísáto")
      , (4, "mínei")
      , (5, "mítáno")
      , (6, "motóba")
      , (7, "sámbó")
      , (8, "mwámbe")
      , (9, "libwá")
      , (10, "zómi")
      , (11, "zómi na mókó")
      , (12, "zómi na míbalé")
      , (13, "zómi na mísáto")
      , (14, "zómi na mínei")
      , (15, "zómi na mítáno")
      , (16, "zómi na motóba")
      , (17, "zómi na sámbó")
      , (18, "zómi na mwámbe")
      , (19, "zómi na libwá")
      , (20, "ntúkú míbalé")
      , (21, "ntúkú míbalé na mókó")
      , (22, "ntúkú míbalé na míbalé")
      , (23, "ntúkú míbalé na mísáto")
      , (24, "ntúkú míbalé na mínei")
      , (25, "ntúkú míbalé na mítáno")
      , (26, "ntúkú míbalé na motóba")
      , (27, "ntúkú míbalé na sámbó")
      , (28, "ntúkú míbalé na mwámbe")
      , (29, "ntúkú míbalé na libwá")
      , (30, "ntúkú mísáto")
      , (31, "ntúkú mísáto na mókó")
      , (32, "ntúkú mísáto na míbalé")
      , (33, "ntúkú mísáto na mísáto")
      , (34, "ntúkú mísáto na mínei")
      , (35, "ntúkú mísáto na mítáno")
      , (36, "ntúkú mísáto na motóba")
      , (37, "ntúkú mísáto na sámbó")
      , (38, "ntúkú mísáto na mwámbe")
      , (39, "ntúkú mísáto na libwá")
      , (40, "ntúkú mínei")
      , (41, "ntúkú mínei na mókó")
      , (42, "ntúkú mínei na míbalé")
      , (43, "ntúkú mínei na mísáto")
      , (44, "ntúkú mínei na mínei")
      , (45, "ntúkú mínei na mítáno")
      , (46, "ntúkú mínei na motóba")
      , (47, "ntúkú mínei na sámbó")
      , (48, "ntúkú mínei na mwámbe")
      , (49, "ntúkú mínei na libwá")
      , (50, "ntúkú mítáno")
      , (51, "ntúkú mítáno na mókó")
      , (52, "ntúkú mítáno na míbalé")
      , (53, "ntúkú mítáno na mísáto")
      , (54, "ntúkú mítáno na mínei")
      , (55, "ntúkú mítáno na mítáno")
      , (56, "ntúkú mítáno na motóba")
      , (57, "ntúkú mítáno na sámbó")
      , (58, "ntúkú mítáno na mwámbe")
      , (59, "ntúkú mítáno na libwá")
      , (60, "ntúkú motóba")
      , (61, "ntúkú motóba na mókó")
      , (62, "ntúkú motóba na míbalé")
      , (63, "ntúkú motóba na mísáto")
      , (64, "ntúkú motóba na mínei")
      , (65, "ntúkú motóba na mítáno")
      , (66, "ntúkú motóba na motóba")
      , (67, "ntúkú motóba na sámbó")
      , (68, "ntúkú motóba na mwámbe")
      , (69, "ntúkú motóba na libwá")
      , (70, "ntúkú sámbó")
      , (71, "ntúkú sámbó na mókó")
      , (72, "ntúkú sámbó na míbalé")
      , (73, "ntúkú sámbó na mísáto")
      , (74, "ntúkú sámbó na mínei")
      , (75, "ntúkú sámbó na mítáno")
      , (76, "ntúkú sámbó na motóba")
      , (77, "ntúkú sámbó na sámbó")
      , (78, "ntúkú sámbó na mwámbe")
      , (79, "ntúkú sámbó na libwá")
      , (80, "ntúkú mwámbe")
      , (81, "ntúkú mwámbe na mókó")
      , (82, "ntúkú mwámbe na míbalé")
      , (83, "ntúkú mwámbe na mísáto")
      , (84, "ntúkú mwámbe na mínei")
      , (85, "ntúkú mwámbe na mítáno")
      , (86, "ntúkú mwámbe na motóba")
      , (87, "ntúkú mwámbe na sámbó")
      , (88, "ntúkú mwámbe na mwámbe")
      , (89, "ntúkú mwámbe na libwá")
      , (90, "ntúkú libwá")
      , (91, "ntúkú libwá na mókó")
      , (92, "ntúkú libwá na míbalé")
      , (93, "ntúkú libwá na mísáto")
      , (94, "ntúkú libwá na mínei")
      , (95, "ntúkú libwá na mítáno")
      , (96, "ntúkú libwá na motóba")
      , (97, "ntúkú libwá na sámbó")
      , (98, "ntúkú libwá na mwámbe")
      , (99, "ntúkú libwá na libwá")
      , (100, "nkámá")
      , (101, "nkámá mókó na mókó")
      , (102, "nkámá mókó na míbalé")
      , (103, "nkámá mókó na mísáto")
      , (104, "nkámá mókó na mínei")
      , (105, "nkámá mókó na mítáno")
      , (106, "nkámá mókó na motóba")
      , (107, "nkámá mókó na sámbó")
      , (108, "nkámá mókó na mwámbe")
      , (109, "nkámá mókó na libwá")
      , (110, "nkámá mókó na zómi")
      , (123, "nkámá mókó na ntúkú míbalé na mísáto")
      , (200, "nkámá míbalé")
      , (300, "nkámá mísáto")
      , (321, "nkámá mísáto na ntúkú míbalé na mókó")
      , (400, "nkámá mínei")
      , (500, "nkámá mítáno")
      , (600, "nkámá motóba")
      , (700, "nkámá sámbó")
      , (800, "nkámá mwámbe")
      , (900, "nkámá libwá")
      , (909, "nkámá libwá na libwá")
      , (990, "nkámá libwá na ntúkú libwá")
      , (999, "nkámá libwá na ntúkú libwá na libwá")
      , (1000, "nkóto")
      , (1001, "nkóto mókó na mókó")
      , (1008, "nkóto mókó na mwámbe")
      , (1234, "nkóto mókó na nkámá míbalé na ntúkú mísáto na mínei")
      , (2000, "nkóto míbalé")
      , (3000, "nkóto mísáto")
      , (4000, "nkóto mínei")
      , (4321, "nkóto mínei na nkámá mísáto na ntúkú míbalé na mókó")
      , (5000, "nkóto mítáno")
      , (6000, "nkóto motóba")
      , (7000, "nkóto sámbó")
      , (8000, "nkóto mwámbe")
      , (9000, "nkóto libwá")
      ]
    )
  ]
