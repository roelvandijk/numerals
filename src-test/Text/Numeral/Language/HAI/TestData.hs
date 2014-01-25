{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        hai

[@ISO639-3@]        hai

[@Native name@]     X̲aat Kíl

[@English name@]    Haida
-}
module Text.Numeral.Language.HAI.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-haida/en/hai/
  http://www.haidalanguage.org/phrases/numbers.html

'guu' means 'less'
'waak' means 'add on'
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "sgwáansang")
      , (2, "sdáng")
      , (3, "hlgúnahl")
      , (4, "stánsang")
      , (5, "tléihl")
      , (6, "tla’únhl")
      , (7, "jagwaa")
      , (8, "sdáansaangaa")
      , (9, "tláahlsgwáansangguu") -- ten minus one
      , (10, "tláahl")
      , (11, "tláahl wáak sgwáansang")
      , (12, "tláahl wáak sdáng")
      , (13, "tláahl wáak hlgúnahl")
      , (14, "tláahl wáak stánsang")
      , (15, "tláahl wáak tléihl")
      , (16, "tláahl wáak tla’únhl")
      , (17, "tláahl wáak jagwaa")
      , (18, "tláahl wáak sdáansaangaa")
      , (19, "tláahl wáak tláahlsgwáansangguu")
      , (20, "lagwa uu sgwáansang")
      , (21, "lagwa uu sgwáansang wáak sgwáansang")
      , (22, "lagwa uu sgwáansang wáak sdáng")
      , (23, "lagwa uu sgwáansang wáak hlgúnahl")
      , (24, "lagwa uu sgwáansang wáak stánsang")
      , (25, "lagwa uu sgwáansang wáak tléihl")
      , (26, "lagwa uu sgwáansang wáak tla’únhl")
      , (27, "lagwa uu sgwáansang wáak jagwaa")
      , (28, "lagwa uu sgwáansang wáak sdáansaangaa")
      , (29, "lagwa uu sgwáansang wáak tláahlsgwáansangguu")
      , (30, "tlaalee hlgúnahl")
      , (31, "tlaalee hlgúnahl wáak sgwáansang")
      , (32, "tlaalee hlgúnahl wáak sdáng")
      , (33, "tlaalee hlgúnahl wáak hlgúnahl")
      , (34, "tlaalee hlgúnahl wáak stánsang")
      , (35, "tlaalee hlgúnahl wáak tléihl")
      , (36, "tlaalee hlgúnahl wáak tla’únhl")
      , (37, "tlaalee hlgúnahl wáak jagwaa")
      , (38, "tlaalee hlgúnahl wáak sdáansaangaa")
      , (39, "tlaalee hlgúnahl wáak tláahlsgwáansangguu")
      , (40, "lagwa uu sdáng")
      , (41, "lagwa uu sdáng wáak sgwáansang")
      , (42, "lagwa uu sdáng wáak sdáng")
      , (43, "lagwa uu sdáng wáak hlgúnahl")
      , (44, "lagwa uu sdáng wáak stánsang")
      , (45, "lagwa uu sdáng wáak tléihl")
      , (46, "lagwa uu sdáng wáak tla’únhl")
      , (47, "lagwa uu sdáng wáak jagwaa")
      , (48, "lagwa uu sdáng wáak sdáansaangaa")
      , (49, "lagwa uu sdáng wáak tláahlsgwáansangguu")
      , (50, "tlaalee tléihl")
      , (51, "tlaalee tléihl wáak sgwáansang")
      , (52, "tlaalee tléihl wáak sdáng")
      , (53, "tlaalee tléihl wáak hlgúnahl")
      , (54, "tlaalee tléihl wáak stánsang")
      , (55, "tlaalee tléihl wáak tléihl")
      , (56, "tlaalee tléihl wáak tla’únhl")
      , (57, "tlaalee tléihl wáak jagwaa")
      , (58, "tlaalee tléihl wáak sdáansaangaa")
      , (59, "tlaalee tléihl wáak tláahlsgwáansangguu")
      , (60, "lagwa uu hlgúnahl")
      , (61, "lagwa uu hlgúnahl wáak sgwáansang")
      , (62, "lagwa uu hlgúnahl wáak sdáng")
      , (63, "lagwa uu hlgúnahl wáak hlgúnahl")
      , (64, "lagwa uu hlgúnahl wáak stánsang")
      , (65, "lagwa uu hlgúnahl wáak tléihl")
      , (66, "lagwa uu hlgúnahl wáak tla’únhl")
      , (67, "lagwa uu hlgúnahl wáak jagwaa")
      , (68, "lagwa uu hlgúnahl wáak sdáansaangaa")
      , (69, "lagwa uu hlgúnahl wáak tláahlsgwáansangguu")
      , (70, "tlaalee jagwaa")
      , (71, "tlaalee jagwaa wáak sgwáansang")
      , (72, "tlaalee jagwaa wáak sdáng")
      , (73, "tlaalee jagwaa wáak hlgúnahl")
      , (74, "tlaalee jagwaa wáak stánsang")
      , (75, "tlaalee jagwaa wáak tléihl")
      , (76, "tlaalee jagwaa wáak tla’únhl")
      , (77, "tlaalee jagwaa wáak jagwaa")
      , (78, "tlaalee jagwaa wáak sdáansaangaa")
      , (79, "tlaalee jagwaa wáak tláahlsgwáansangguu")
      , (80, "lagwa uu sdáansaangaa")
      , (81, "lagwa uu sdáansaangaa wáak sgwáansang")
      , (82, "lagwa uu sdáansaangaa wáak sdáng")
      , (83, "lagwa uu sdáansaangaa wáak hlgúnahl")
      , (84, "lagwa uu sdáansaangaa wáak stánsang")
      , (85, "lagwa uu sdáansaangaa wáak tléihl")
      , (86, "lagwa uu sdáansaangaa wáak tla’únhl")
      , (87, "lagwa uu sdáansaangaa wáak jagwaa")
      , (88, "lagwa uu sdáansaangaa wáak sdáansaangaa")
      , (89, "lagwa uu sdáansaangaa wáak tláahlsgwáansangguu")
      , (90, "tlaalee tláahlsgwáansangguu")
      , (91, "tlaalee tláahlsgwáansangguu wáak sgwáansang")
      , (92, "tlaalee tláahlsgwáansangguu wáak sdáng")
      , (93, "tlaalee tláahlsgwáansangguu wáak hlgúnahl")
      , (94, "tlaalee tláahlsgwáansangguu wáak stánsang")
      , (95, "tlaalee tláahlsgwáansangguu wáak tléihl")
      , (96, "tlaalee tláahlsgwáansangguu wáak tla’únhl")
      , (97, "tlaalee tláahlsgwáansangguu wáak jagwaa")
      , (98, "tlaalee tláahlsgwáansangguu wáak sdáansaangaa")
      , (99, "tlaalee tláahlsgwáansangguu wáak tláahlsgwáansangguu")
      , (100, "lagwa uu tléihl")
      , (101, "lagwa uu tléihl sgwáansang")
      , (102, "lagwa uu tléihl sdáng")
      , (103, "lagwa uu tléihl hlgúnahl")
      , (104, "lagwa uu tléihl stánsang")
      , (105, "lagwa uu tléihl tléihl")
      , (106, "lagwa uu tléihl tla’únhl")
      , (107, "lagwa uu tléihl jagwaa")
      , (108, "lagwa uu tléihl sdáansaangaa")
      , (109, "lagwa uu tléihl tláahlsgwáansangguu")
      , (110, "lagwa uu tléihl tláahl")
      , (123, "lagwa uu tléihl lagwa uu sgwáansang wáak hlgúnahl")
      , (200, "lagwa uu tláahl")
      , (300, "lagwa uu tláahl wáak lagwa uu tléihl")
      , (321, "lagwa uu tláahl wáak lagwa uu tléihl lagwa uu sgwáansang wáak sgwáansang")
      , (400, "lagwa uu tlaalee sdáng")
      , (500, "lagwa uu tlaalee sdáng wáak lagwa uu tléihl")
      , (600, "lagwa uu tlaalee hlgúnahl")
      , (700, "lagwa uu tlaalee wáak lagwa uu tléihl")
      , (800, "lagwa uu tlaalee sdáansaangaa")
      , (900, "lagwa uu tlaalee sdáansaangaa wáak lagwa uu tléihl")
      , (909, "lagwa uu tlaalee sdáansaangaa wáak lagwa uu tléihl tláahlsgwáansangguu")
      , (990, "lagwa uu tlaalee sdáansaangaa wáak lagwa uu tléihl tlaalee tláahlsgwáansangguu")
      , (999, "lagwa uu tlaalee sdáansaangaa wáak lagwa uu tléihl tlaalee tláahlsgwáansangguu wáak tláahlsgwáansangguu")
      , (1000, "lagwa uu tlaalee tléihl")
      , (1001, "lagwa uu tlaalee tléihl sgwáansang")
      , (1008, "lagwa uu tlaalee tléihl sdáansaangaa")
      , (1234, "lagwa uu tlaalee tléihl lagwa uu tláahl tlaalee hlgúnahl wáak stánsang")
      , (2000, "lagwa uu tlaalee tléihl wáak sdáng")
      , (3000, "lagwa uu tlaalee tléihl wáak hlgúnahl")
      , (4000, "lagwa uu tlaalee tléihl wáak stánsang")
      , (4321, "lagwa uu tlaalee tléihl wáak stánsang lagwa uu tláahl wáak lagwa uu tléihl lagwa uu sgwáansang wáak sgwáansang")
      , (5000, "lagwa uu tlaalee tléihl wáak tléihl")
      , (6000, "lagwa uu tlaalee tléihl wáak tla’únhl")
      , (7000, "lagwa uu tlaalee tléihl wáak jagwaa")
      , (8000, "lagwa uu tlaalee tléihl wáak sdáansaangaa")
      , (9000, "lagwa uu tlaalee tléihl wáak tláahlsgwáansangguu")
      , (10000, "lagwa uu tlaalee tléihl wáak tláahl")
      , (12345, "tláahl wáak lagwa uu tlaalee tléihl wáak sdáng lagwa uu tláahl wáak lagwa uu tléihl lagwa uu sdáng wáak tléihl")
      , (20000, "lagwa uu lagwa uu tlaalee tléihl")
      , (30000, "tlaalee lagwa uu tlaalee tléihl wáak hlgúnahl")
      , (40000, "lagwa uu lagwa uu tlaalee tléihl wáak sdáng")
      , (50000, "tlaalee lagwa uu tlaalee tléihl wáak tléihl")
      , (54321, "tlaalee tléihl wáak lagwa uu tlaalee tléihl wáak stánsang lagwa uu tláahl wáak lagwa uu tléihl lagwa uu sgwáansang wáak sgwáansang")
      , (60000, "lagwa uu lagwa uu tlaalee tléihl wáak hlgúnahl")
      , (70000, "tlaalee lagwa uu tlaalee tléihl wáak jagwaa")
      , (80000, "lagwa uu lagwa uu tlaalee tléihl wáak sdáansaangaa")
      , (90000, "tlaalee lagwa uu tlaalee tléihl wáak tláahlsgwáansangguu")
      , (100000, "lagwa uu taalee kwan tláahl wáak lagwa uu tléihl")
      , (123456, "lagwa uu tléihl lagwa uu sgwáansang wáak lagwa uu tlaalee tléihl wáak hlgúnahl lagwa uu tlaalee sdáng tlaalee tléihl wáak tla’únhl")
      , (200000, "lagwa uu lagwa uu tlaalee tléihl wáak tláahl")
      , (300000, "lagwa uu tláahl wáak lagwa uu taalee kwan tláahl wáak lagwa uu tléihl")
      , (400000, "lagwa uu tlaalee lagwa uu tlaalee tléihl wáak sdáng")
      , (500000, "lagwa uu tlaalee sdáng wáak lagwa uu taalee kwan tláahl wáak lagwa uu tléihl")
      , (600000, "lagwa uu tlaalee lagwa uu tlaalee tléihl wáak tléihl")
      , (654321, "lagwa uu tlaalee tléihl tlaalee tléihl wáak lagwa uu tlaalee tléihl wáak stánsang lagwa uu tláahl wáak lagwa uu tléihl lagwa uu sgwáansang wáak sgwáansang")
      , (700000, "lagwa uu tlaalee wáak lagwa uu taalee kwan tláahl wáak lagwa uu tléihl")
      , (800000, "lagwa uu tlaalee lagwa uu tlaalee tléihl wáak sdáansaangaa")
      , (900000, "lagwa uu tlaalee sdáansaangaa wáak lagwa uu taalee kwan tláahl wáak lagwa uu tléihl")
      , (1000000, "lagwa uu tlaalee kwan")
      , (1000001, "lagwa uu tlaalee kwan sgwáansang")
      , (1234567, "lagwa uu tlaalee kwan lagwa uu tláahl tlaalee hlgúnahl wáak lagwa uu tlaalee tléihl wáak stánsang lagwa uu tlaalee sdáng wáak lagwa uu tléihl lagwa uu hlgúnahl wáak jagwaa")
      , (2000000, "sdáng lagwa uu tlaalee kwan")
      , (3000000, "hlgúnahl lagwa uu tlaalee kwan")
      , (4000000, "stánsang lagwa uu tlaalee kwan")
      , (5000000, "tléihl lagwa uu tlaalee kwan")
      , (6000000, "tla’únhl lagwa uu tlaalee kwan")
      , (7000000, "jagwaa lagwa uu tlaalee kwan")
      , (7654321, "jagwaa lagwa uu tlaalee kwan lagwa uu tlaalee tléihl tlaalee tléihl wáak lagwa uu tlaalee tléihl wáak stánsang lagwa uu tláahl wáak lagwa uu tléihl lagwa uu sgwáansang wáak sgwáansang")
      , (8000000, "sdáansaangaa lagwa uu tlaalee kwan")
      , (9000000, "tláahlsgwáansangguu lagwa uu tlaalee kwan")
      , (1000000000, "lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl")
      , (1000000001, "lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl sgwáansang")
      , (2000000000, "sdáng lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl")
      , (3000000000, "hlgúnahl lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl")
      , (4000000000, "stánsang lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl")
      , (5000000000, "tléihl lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl")
      , (6000000000, "tla’únhl lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl")
      , (7000000000, "jagwaa lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl")
      , (8000000000, "sdáansaangaa lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl")
      , (9000000000, "tláahlsgwáansangguu lagwa uu tlaalee kwan wáak lagwa uu tlaalee tléihl")
      ]
    )
  ]
