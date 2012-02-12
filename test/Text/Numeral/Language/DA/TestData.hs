{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        da

[@ISO639-2@]        dan

[@ISO639-3@]        dan

[@Native name@]     dansk

[@English name@]    Danish
-}

module Text.Numeral.Language.DA.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "numerals-base" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "nul")
      , (1, "en")
      , (2, "to")
      , (3, "tre")
      , (4, "fire")
      , (5, "fem")
      , (6, "seks")
      , (7, "syv")
      , (8, "otte")
      , (9, "ni")
      , (10, "ti")
      , (11, "elleve")
      , (12, "tolv")
      , (13, "tretten")
      , (14, "fjorten")
      , (15, "femten")
      , (16, "seksten")
      , (17, "sytten")
      , (18, "atten")
      , (19, "nitten")
      , (20, "tyve")
      , (21, "énogtyve")
      , (22, "toogtyve")
      , (23, "treogtyve")
      , (24, "fireogtyve")
      , (25, "femogtyve")
      , (26, "seksogtyve")
      , (27, "syvogtyve")
      , (28, "otteogtyve")
      , (29, "niogtyve")
      , (30, "tredive")
      , (31, "énogtredive")
      , (32, "toogtredive")
      , (33, "treogtredive")
      , (34, "fireogtredive")
      , (35, "femogtredive")
      , (36, "seksogtredive")
      , (37, "syvogtredive")
      , (38, "otteogtredive")
      , (39, "niogtredive")
      , (40, "fyrre")
      , (41, "énogfyrre")
      , (42, "toogfyrre")
      , (43, "treogfyrre")
      , (44, "fireogfyrre")
      , (45, "femogfyrre")
      , (46, "seksogfyrre")
      , (47, "syvogfyrre")
      , (48, "otteogfyrre")
      , (49, "niogfyrre")
      , (50, "halvtreds")
      , (51, "énoghalvtreds")
      , (52, "tooghalvtreds")
      , (53, "treoghalvtreds")
      , (54, "fireoghalvtreds")
      , (55, "femoghalvtreds")
      , (56, "seksoghalvtreds")
      , (57, "syvoghalvtreds")
      , (58, "otteoghalvtreds")
      , (59, "nioghalvtreds")
      , (60, "tres")
      , (61, "énogtres")
      , (62, "toogtres")
      , (63, "treogtres")
      , (64, "fireogtres")
      , (65, "femogtres")
      , (66, "seksogtres")
      , (67, "syvogtres")
      , (68, "otteogtres")
      , (69, "niogtres")
      , (70, "halvfjerds")
      , (71, "énoghalvfjerds")
      , (72, "tooghalvfjerds")
      , (73, "treoghalvfjerds")
      , (74, "fireoghalvfjerds")
      , (75, "femoghalvfjerds")
      , (76, "seksoghalvfjerds")
      , (77, "syvoghalvfjerds")
      , (78, "otteoghalvfjerds")
      , (79, "nioghalvfjerds")
      , (80, "firs")
      , (81, "énogfirs")
      , (82, "toogfirs")
      , (83, "treogfirs")
      , (84, "fireogfirs")
      , (85, "femogfirs")
      , (86, "seksogfirs")
      , (87, "syvogfirs")
      , (88, "otteogfirs")
      , (89, "niogfirs")
      , (90, "halvfems")
      , (91, "énoghalvfems")
      , (92, "tooghalvfems")
      , (93, "treoghalvfems")
      , (94, "fireoghalvfems")
      , (95, "femoghalvfems")
      , (96, "seksoghalvfems")
      , (97, "syvoghalvfems")
      , (98, "otteoghalvfems")
      , (99, "nioghalvfems")
      , (100, "hundrede")
      ]
    )
  ]
