{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        gse

[@ISO639-3@]        gse

[@Native name@]     Schwyzerdütsch

[@English name@]    Swiss German
-}

module Text.Numeral.Language.GSW.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "this" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-swiss-german/en/gsw-che/
-}

cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "eis")
      , (2, "zwöi")
      , (3, "drü")
      , (4, "vier")
      , (5, "füf")
      , (6, "sächs")
      , (7, "sibe")
      , (8, "acht")
      , (9, "nüün")
      , (10, "zäh")
      , (11, "euf")
      , (12, "zwüof")
      , (13, "dryzäh")
      , (14, "vierzäh")
      , (15, "füfzäh")
      , (16, "sächszäh")
      , (17, "sibezäh")
      , (18, "achtzäh")
      , (19, "nüünzäh")
      , (20, "zwänzg")
      , (21, "einezwänzg")
      , (22, "zwöiezwänzg")
      , (23, "drüezwänzg")
      , (24, "vierezwänzg")
      , (25, "füfezwänzg")
      , (26, "sächsezwänzg")
      , (27, "sibenezwänzg")
      , (28, "achtezwänzg")
      , (29, "nünezwänzg")
      , (30, "dryssg")
      -- , (31, "")
      -- , (32, "")
      -- , (33, "")
      -- , (34, "")
      , (35, "füfedryssg")
      -- , (36, "")
      -- , (37, "")
      -- , (38, "")
      -- , (39, "")
      , (40, "vierzg")
      -- , (41, "")
      -- , (42, "")
      -- , (43, "")
      -- , (44, "")
      -- , (45, "")
      -- , (46, "")
      -- , (47, "")
      -- , (48, "")
      -- , (49, "")
      , (50, "füfzg")
      , (51, "einefüfzg")
      -- , (52, "")
      -- , (53, "")
      -- , (54, "")
      -- , (55, "")
      -- , (56, "")
      -- , (57, "")
      -- , (58, "")
      -- , (59, "")
      , (60, "sëchzg")
      -- , (61, "")
      -- , (62, "")
      -- , (63, "")
      -- , (64, "")
      -- , (65, "")
      -- , (66, "")
      -- , (67, "")
      -- , (68, "")
      -- , (69, "")
      , (70, "sibezg")
      -- , (71, "")
      , (72, "zwöiesibezg")
      -- , (73, "")
      -- , (74, "")
      -- , (75, "")
      -- , (76, "")
      -- , (77, "")
      -- , (78, "")
      -- , (79, "")
      , (80, "achzg")
      -- , (81, "")
      -- , (82, "")
      -- , (83, "")
      -- , (84, "")
      -- , (85, "")
      -- , (86, "")
      -- , (87, "")
      -- , (88, "")
      -- , (89, "")
      , (90, "nüünzg")
      -- , (91, "")
      -- , (92, "")
      -- , (93, "")
      -- , (94, "")
      -- , (95, "")
      -- , (96, "")
      , (97, "sibenenüünzg")
      -- , (98, "")
      -- , (99, "")
      , (100, "hundert")
      , (101, "hundertundäis")
      , (102, "hundertzwöi")
      ]
    )
  ]
