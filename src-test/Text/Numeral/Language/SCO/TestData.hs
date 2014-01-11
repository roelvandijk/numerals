{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        sco

[@ISO639-3@]        sco

[@Native name@]     Scots

[@English name@]    Scots
-}

module Text.Numeral.Language.SCO.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Num )
import "this" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/scots.html
-}

cardinals ∷ (Num i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "ane")
      , (2, "twa")
      , (3, "three")
      , (4, "fower")
      , (5, "five")
      , (6, "sax")
      , (7, "seeven")
      , (8, "echt")
      , (9, "nine")
      , (10, "ten")
      , (11, "aleeven")
      , (12, "twal")
      , (13, "therteen")
      , (14, "fowerteen")
      , (15, "feifteen")
      , (16, "saxteen")
      , (17, "seiventeen")
      , (18, "echteen")
      , (19, "ninteen")
      , (20, "twintie")
      , (21, "twintie ane")
      , (22, "twintie twa")
      , (23, "twintie three")
      , (24, "twintie fower")
      , (25, "twintie five")
      , (26, "twintie sax")
      , (27, "twintie seeven")
      , (28, "twintie echt")
      , (29, "twintie nine")
      , (30, "thertie")
      , (31, "thertie ane")
      , (32, "thertie twa")
      , (33, "thertie three")
      , (34, "thertie fower")
      , (35, "thertie five")
      , (36, "thertie sax")
      , (37, "thertie seeven")
      , (38, "thertie echt")
      , (39, "thertie nine")
      , (40, "fowertie")
      , (41, "fowertie ane")
      , (42, "fowertie twa")
      , (43, "fowertie three")
      , (44, "fowertie fower")
      , (45, "fowertie five")
      , (46, "fowertie sax")
      , (47, "fowertie seeven")
      , (48, "fowertie echt")
      , (49, "fowertie nine")
      , (50, "fuftie")
      , (51, "fuftie ane")
      , (52, "fuftie twa")
      , (53, "fuftie three")
      , (54, "fuftie fower")
      , (55, "fuftie five")
      , (56, "fuftie sax")
      , (57, "fuftie seeven")
      , (58, "fuftie echt")
      , (59, "fuftie nine")
      , (60, "saxtie")
      , (61, "saxtie ane")
      , (62, "saxtie twa")
      , (63, "saxtie three")
      , (64, "saxtie fower")
      , (65, "saxtie five")
      , (66, "saxtie sax")
      , (67, "saxtie seeven")
      , (68, "saxtie echt")
      , (69, "saxtie nine")
      , (70, "seeventie")
      , (71, "seeventie ane")
      , (72, "seeventie twa")
      , (73, "seeventie three")
      , (74, "seeventie fower")
      , (75, "seeventie five")
      , (76, "seeventie sax")
      , (77, "seeventie seeven")
      , (78, "seeventie echt")
      , (79, "seeventie nine")
      , (80, "echtie")
      , (81, "echtie ane")
      , (82, "echtie twa")
      , (83, "echtie three")
      , (84, "echtie fower")
      , (85, "echtie five")
      , (86, "echtie sax")
      , (87, "echtie seeven")
      , (88, "echtie echt")
      , (89, "echtie nine")
      , (90, "nintie")
      , (91, "nintie ane")
      , (92, "nintie twa")
      , (93, "nintie three")
      , (94, "nintie fower")
      , (95, "nintie five")
      , (96, "nintie sax")
      , (97, "nintie seeven")
      , (98, "nintie echt")
      , (99, "nintie nine")
      , (100, "hunner")
      ]
    )
  ]
