{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        gv

[@ISO639-2@]        glv

[@ISO639-3@]        glv

[@Native name@]     Gaelg

[@English name@]    Manx
-}

module Text.Numeral.Language.GV.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude     ( Integral )
import "this" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/manx.html
  http://www.gaelg.iofm.net/LESSONS/P/P19.html
  http://www.gaelg.iofm.net/LESSONS/mona/Lessons.pdf
-}

cardinals ∷ (Integral i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "nane")
      , (2, "jees")
      , (3, "tree")
      , (4, "kiare")
      , (5, "queig")
      , (6, "shey")
      , (7, "shiaght")
      , (8, "hoght")
      , (9, "nuy")
      , (10, "jeih")
      , (11, "nane-jeig")
      , (12, "daa-yeig")
      , (13, "tree-jeig")
      , (14, "kiare-jeig")
      , (15, "queig-jeig")
      , (16, "shey-jeig")
      , (17, "shiaght-jeig")
      , (18, "hoght-jeig")
      , (19, "nuy-jeig")
      , (20, "feed")
      , (21, "nane as feed")
      , (22, "jees as feed")
      , (23, "tree as feed")
      , (24, "kiare as feed")
      , (25, "queig as feed")
      , (26, "shey as feed")
      , (27, "shiaght as feed")
      , (28, "hoght as feed")
      , (29, "nuy as feed")
      , (30, "jeih as feed")
      , (31, "nane-jeig as feed")
      , (32, "daa-yeig as feed")
      , (33, "tree-jeig as feed")
      , (34, "kiare-jeig as feed")
      , (35, "queig-jeig as feed")
      , (36, "shey-jeig as feed")
      , (37, "shiaght-jeig as feed")
      , (38, "hoght-jeig as feed")
      , (39, "nuy-jeig as feed")
      , (40, "daeed")
      , (41, "nane as daeed")
      , (42, "jees as daeed")
      , (43, "tree as daeed")
      , (44, "kiare as daeed")
      , (45, "queig as daeed")
      , (46, "shey as daeed")
      , (47, "shiaght as daeed")
      , (48, "hoght as daeed")
      , (49, "nuy as daeed")
      , (50, "jeih as daeed")
      , (51, "nane-jeig as daeed")
      , (52, "daa-yeig as daeed")
      , (53, "tree-jeig as daeed")
      , (54, "kiare-jeig as daeed")
      , (55, "queig-jeig as daeed")
      , (56, "shey-jeig as daeed")
      , (57, "shiaght-jeig as daeed")
      , (58, "hoght-jeig as daeed")
      , (59, "nuy-jeig as daeed")
      , (60, "tree feed")
      , (61, "tree feed as nane")
      , (62, "tree feed as jees")
      , (63, "tree feed as tree")
      , (64, "tree feed as kiare")
      , (65, "tree feed as queig")
      , (66, "tree feed as shey")
      , (67, "tree feed as shiaght")
      , (68, "tree feed as hoght")
      , (69, "tree feed as nuy")
      , (70, "tree feed as jeih")
      , (71, "tree feed as nane-jeig")
      , (72, "tree feed as daa-yeig")
      , (73, "tree feed as tree-jeig")
      , (74, "tree feed as kiare-jeig")
      , (75, "tree feed as queig-jeig")
      , (76, "tree feed as shey-jeig")
      , (77, "tree feed as shiaght-jeig")
      , (78, "tree feed as hoght-jeig")
      , (79, "tree feed as nuy-jeig")
      , (80, "kiare feed")
      , (81, "kiare feed as nane")
      , (82, "kiare feed as jees")
      , (83, "kiare feed as tree")
      , (84, "kiare feed as kiare")
      , (85, "kiare feed as queig")
      , (86, "kiare feed as shey")
      , (87, "kiare feed as shiaght")
      , (88, "kiare feed as hoght")
      , (89, "kiare feed as nuy")
      , (90, "kiare feed as jeih")
      , (91, "kiare feed as nane-jeig")
      , (92, "kiare feed as daa-yeig")
      , (93, "kiare feed as tree-jeig")
      , (94, "kiare feed as kiare-jeig")
      , (95, "kiare feed as queig-jeig")
      , (96, "kiare feed as shey-jeig")
      , (97, "kiare feed as shiaght-jeig")
      , (98, "kiare feed as hoght-jeig")
      , (99, "kiare feed as nuy-jeig")
      , (100, "keead")
      , (200, "daa cheead")
      , (300, "tree cheead")
      , (400, "kiare cheead")
      , (500, "queig cheead")
      , (600, "shey cheead")
      , (700, "shiaght cheead")
      , (800, "hoght cheead")
      , (900, "nuy cheead")
      , (1000, "thousane")
      -- , (dec 6, "jeih cheead thousane")
      ]
    )
  ]
