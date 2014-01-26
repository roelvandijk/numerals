{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        bm

[@ISO639-2@]        bam

[@ISO639-3@]        bam

[@Native name@]     -

[@English name@]    Bambara
-}
module Text.Numeral.Language.BAM.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-bambara/en/bam/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "kélen")
      , (2, "fila")
      , (3, "sàba")
      , (4, "náani")
      , (5, "dúuru")
      , (6, "wɔɔrɔ")
      , (7, "wólonwula")
      , (8, "séegin")
      , (9, "k̀ɔnɔntɔn")
      , (10, "tán")
      , (11, "tán ní kélen")
      , (12, "tán ní fila")
      , (13, "tán ní sàba")
      , (14, "tán ní náani")
      , (15, "tán ní dúuru")
      , (16, "tán ní wɔɔrɔ")
      , (17, "tán ní wólonwula")
      , (18, "tán ní séegin")
      , (19, "tán ní k̀ɔnɔntɔn")
      , (20, "mùgan")
      , (21, "mùgan ní kélen")
      , (22, "mùgan ní fila")
      , (23, "mùgan ní sàba")
      , (24, "mùgan ní náani")
      , (25, "mùgan ní dúuru")
      , (26, "mùgan ní wɔɔrɔ")
      , (27, "mùgan ní wólonwula")
      , (28, "mùgan ní séegin")
      , (29, "mùgan ní k̀ɔnɔntɔn")
      , (30, "bísàba")
      , (31, "bísàba ní kélen")
      , (32, "bísàba ní fila")
      , (33, "bísàba ní sàba")
      , (34, "bísàba ní náani")
      , (35, "bísàba ní dúuru")
      , (36, "bísàba ní wɔɔrɔ")
      , (37, "bísàba ní wólonwula")
      , (38, "bísàba ní séegin")
      , (39, "bísàba ní k̀ɔnɔntɔn")
      , (40, "bínaani")
      , (41, "bínaani ní kélen")
      , (42, "bínaani ní fila")
      , (43, "bínaani ní sàba")
      , (44, "bínaani ní náani")
      , (45, "bínaani ní dúuru")
      , (46, "bínaani ní wɔɔrɔ")
      , (47, "bínaani ní wólonwula")
      , (48, "bínaani ní séegin")
      , (49, "bínaani ní k̀ɔnɔntɔn")
      , (50, "bídúuru")
      , (51, "bídúuru ní kélen")
      , (52, "bídúuru ní fila")
      , (53, "bídúuru ní sàba")
      , (54, "bídúuru ní náani")
      , (55, "bídúuru ní dúuru")
      , (56, "bídúuru ní wɔɔrɔ")
      , (57, "bídúuru ní wólonwula")
      , (58, "bídúuru ní séegin")
      , (59, "bídúuru ní k̀ɔnɔntɔn")
      , (60, "bíwɔɔrɔ")
      , (61, "bíwɔɔrɔ ní kélen")
      , (62, "bíwɔɔrɔ ní fila")
      , (63, "bíwɔɔrɔ ní sàba")
      , (64, "bíwɔɔrɔ ní náani")
      , (65, "bíwɔɔrɔ ní dúuru")
      , (66, "bíwɔɔrɔ ní wɔɔrɔ")
      , (67, "bíwɔɔrɔ ní wólonwula")
      , (68, "bíwɔɔrɔ ní séegin")
      , (69, "bíwɔɔrɔ ní k̀ɔnɔntɔn")
      , (70, "bíwolonfila")
      , (71, "bíwolonfila ní kélen")
      , (72, "bíwolonfila ní fila")
      , (73, "bíwolonfila ní sàba")
      , (74, "bíwolonfila ní náani")
      , (75, "bíwolonfila ní dúuru")
      , (76, "bíwolonfila ní wɔɔrɔ")
      , (77, "bíwolonfila ní wólonwula")
      , (78, "bíwolonfila ní séegin")
      , (79, "bíwolonfila ní k̀ɔnɔntɔn")
      , (80, "bíséegin")
      , (81, "bíséegin ní kélen")
      , (82, "bíséegin ní fila")
      , (83, "bíséegin ní sàba")
      , (84, "bíséegin ní náani")
      , (85, "bíséegin ní dúuru")
      , (86, "bíséegin ní wɔɔrɔ")
      , (87, "bíséegin ní wólonwula")
      , (88, "bíséegin ní séegin")
      , (89, "bíséegin ní k̀ɔnɔntɔn")
      , (90, "bík̀ɔnɔntɔn")
      , (91, "bík̀ɔnɔntɔn ní kélen")
      , (92, "bík̀ɔnɔntɔn ní fila")
      , (93, "bík̀ɔnɔntɔn ní sàba")
      , (94, "bík̀ɔnɔntɔn ní náani")
      , (95, "bík̀ɔnɔntɔn ní dúuru")
      , (96, "bík̀ɔnɔntɔn ní wɔɔrɔ")
      , (97, "bík̀ɔnɔntɔn ní wólonwula")
      , (98, "bík̀ɔnɔntɔn ní séegin")
      , (99, "bík̀ɔnɔntɔn ní k̀ɔnɔntɔn")
      , (100, "k̀ɛmɛ")
      , (101, "k̀ɛmɛ ní kélen")
      , (102, "k̀ɛmɛ ní fila")
      , (103, "k̀ɛmɛ ní sàba")
      , (104, "k̀ɛmɛ ní náani")
      , (105, "k̀ɛmɛ ní dúuru")
      , (106, "k̀ɛmɛ ní wɔɔrɔ")
      , (107, "k̀ɛmɛ ní wólonwula")
      , (108, "k̀ɛmɛ ní séegin")
      , (109, "k̀ɛmɛ ní k̀ɔnɔntɔn")
      , (110, "k̀ɛmɛ ní tán")
      , (123, "k̀ɛmɛ ní mùgan ní sàba")
      , (200, "k̀ɛmɛ fila")
      , (300, "k̀ɛmɛ sàba")
      , (321, "k̀ɛmɛ sàba ní mùgan ní kélen")
      , (400, "k̀ɛmɛ náani")
      , (500, "k̀ɛmɛ dúuru")
      , (600, "k̀ɛmɛ wɔɔrɔ")
      , (700, "wólonwula k̀ɛmɛ")
      , (800, "séegin k̀ɛmɛ")
      , (900, "k̀ɔnɔntɔn k̀ɛmɛ")
      , (909, "k̀ɔnɔntɔn k̀ɛmɛ ní k̀ɔnɔntɔn")
      , (990, "k̀ɔnɔntɔn k̀ɛmɛ ní bík̀ɔnɔntɔn")
      , (999, "k̀ɔnɔntɔn k̀ɛmɛ ní bík̀ɔnɔntɔn ní k̀ɔnɔntɔn")
      , (1000, "wa kélen")
      , (1001, "wa kélen ní kélen")
      , (1008, "wa kélen ní séegin")
      , (1234, "wa kélen ní k̀ɛmɛ fila ní bísàba ní náani")
      , (2000, "wa fila")
      , (3000, "wa sàba")
      , (4000, "wa náani")
      , (4321, "wa náani ní k̀ɛmɛ sàba ní mùgan ní kélen")
      , (5000, "wa dúuru")
      , (6000, "wa wɔɔrɔ")
      , (7000, "wólonwula wa")
      , (8000, "séegin wa")
      , (9000, "k̀ɔnɔntɔn wa")
      , (10000, "tán wa")
      , (12345, "tán ní wa fila ní k̀ɛmɛ sàba ní bínaani ní dúuru")
      , (20000, "mùgan wa")
      , (30000, "bíwa sàba")
      , (40000, "bínaani wa")
      , (50000, "bíwa dúuru")
      , (54321, "bídúuru ní wa náani ní k̀ɛmɛ sàba ní mùgan ní kélen")
      , (60000, "bíwa wɔɔrɔ")
      , (70000, "bíwa wolonfila")
      , (80000, "bíséegin wa")
      , (90000, "bík̀ɔnɔntɔn wa")
      , (100000, "k̀ɛmɛ wa")
      , (123456, "k̀ɛmɛ ní mùgan ní wa sàba ní k̀ɛmɛ náani ní bídúuru ní wɔɔrɔ")
      , (200000, "k̀ɛmɛ fila wa")
      , (300000, "k̀ɛmɛ sàba wa")
      , (400000, "k̀ɛmɛ náani wa")
      , (500000, "k̀ɛmɛ dúuru wa")
      , (600000, "k̀ɛmɛ wɔɔrɔ wa")
      , (654321, "k̀ɛmɛ wɔɔrɔ ní bídúuru ní wa náani ní k̀ɛmɛ sàba ní mùgan ní kélen")
      , (700000, "wólonwula k̀ɛmɛ wa")
      , (800000, "séegin k̀ɛmɛ wa")
      , (900000, "k̀ɔnɔntɔn k̀ɛmɛ wa")
      , (1000000, "mílyɔn kélen")
      ]
    )
  ]
