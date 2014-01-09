{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-3@]        pdc

[@Native name@]     Pennsilfaanisch Deitsch

[@English name@]    Pennsylvania German
-}

module Text.Numeral.Language.PDC.TestData (cardinals) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "base-unicode-symbols" Prelude.Unicode ( (⋅) )
import "numerals-base" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals-base" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )

--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-pennsylvania-german/en/pdc/
-}

cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "null")
      , (1, "eens")
      , (2, "zwee")
      , (3, "drei")
      , (4, "vier")
      , (5, "fimf")
      , (6, "sex")
      , (7, "siwwe")
      , (8, "acht")
      , (9, "nein")
      , (10, "zehe")
      , (11, "elf")
      , (12, "zwelf")
      , (13, "dreizeh")
      , (14, "vazeh")
      , (15, "fuffzeh")
      , (16, "sechzeh")
      , (17, "siwwezeh")
      , (18, "achtzeh")
      , (19, "neinzeh")
      , (20, "zwansich")
      , (21, "eenunzwansich")
      , (22, "zweeunzwansich")
      , (23, "dreiunzwansich")
      , (24, "vierunzwansich")
      , (25, "fimfunzwansich")
      , (26, "sexunzwansich")
      , (27, "siwweunzwansich")
      , (28, "achtunzwansich")
      , (29, "neinunzwansich")
      , (30, "dreissich")
      , (31, "eenundreissich")
      , (32, "zweeundreissich")
      , (33, "dreiundreissich")
      , (34, "vierundreissich")
      , (35, "fimfundreissich")
      , (36, "sexundreissich")
      , (37, "siwweundreissich")
      , (38, "achtundreissich")
      , (39, "neinundreissich")
      , (40, "vazich")
      , (41, "eenunvazich")
      , (42, "zweeunvazich")
      , (43, "dreiunvazich")
      , (44, "vierunvazich")
      , (45, "fimfunvazich")
      , (46, "sexunvazich")
      , (47, "siwweunvazich")
      , (48, "achtunvazich")
      , (49, "neinunvazich")
      , (50, "fuffzich")
      , (51, "eenunfuffzich")
      , (52, "zweeunfuffzich")
      , (53, "dreiunfuffzich")
      , (54, "vierunfuffzich")
      , (55, "fimfunfuffzich")
      , (56, "sexunfuffzich")
      , (57, "siwweunfuffzich")
      , (58, "achtunfuffzich")
      , (59, "neinunfuffzich")
      , (60, "sechzich")
      , (61, "eenunsechzich")
      , (62, "zweeunsechzich")
      , (63, "dreiunsechzich")
      , (64, "vierunsechzich")
      , (65, "fimfunsechzich")
      , (66, "sexunsechzich")
      , (67, "siwweunsechzich")
      , (68, "achtunsechzich")
      , (69, "neinunsechzich")
      , (70, "siwwezich")
      , (71, "eenunsiwwezich")
      , (72, "zweeunsiwwezich")
      , (73, "dreiunsiwwezich")
      , (74, "vierunsiwwezich")
      , (75, "fimfunsiwwezich")
      , (76, "sexunsiwwezich")
      , (77, "siwweunsiwwezich")
      , (78, "achtunsiwwezich")
      , (79, "neinunsiwwezich")
      , (80, "achtzich")
      , (81, "eenunachtzich")
      , (82, "zweeunachtzich")
      , (83, "dreiunachtzich")
      , (84, "vierunachtzich")
      , (85, "fimfunachtzich")
      , (86, "sexunachtzich")
      , (87, "siwweunachtzich")
      , (88, "achtunachtzich")
      , (89, "neinunachtzich")
      , (90, "neinzich")
      , (91, "eenunneinzich")
      , (92, "zweeunneinzich")
      , (93, "dreiunneinzich")
      , (94, "vierunneinzich")
      , (95, "fimfunneinzich")
      , (96, "sexunneinzich")
      , (97, "siwweunneinzich")
      , (98, "achtunneinzich")
      , (99, "neinunneinzich")
      , (100, "en hunnert")
      , (101, "en hunnert un eens")
      , (107, "en hunnert un siwwe")
      , (199, "en hunnert neinunneinzich")
      , (200, "zwee hunnert")
      , (201, "zwee hunnert un eens")
      , (207, "zwee hunnert un siwwe")
      , (909, "nein hunnert un nein")
      , (990, "nein hunnert neinzich")
      , (999, "nein hunnert neinunneinzich")
      , (1000, "en dausend")
      , (1001, "en dausend eens")
      , (1024, "en dausend vierunzwansich")
      , (1979, "en dausend nein hunnert neinunsiwwezich")
      , (2048, "zwee dausend achtunvazich")
      , (3000, "drei dausend")
      , (4000, "vier dausend")
      , (4096, "vier dausend sexunneinzich")
      , (8192, "acht dausend en hunnert zweeunneinzich")
      , (16384, "sechzeh dausend drei hunnert vierunachtzich")
      , (32768, "zweeundreissich dausend siwwe hunnert achtunsechzich")
      , (65536, "fimfunsechzich dausend fimf hunnert sexundreissich")
      , (131072, "en hunnert eenundreissich dausend zweeunsiwwezich")
      , (262144, "zwee hunnert zweeunsechzich dausend en hunnert vierunvazich")
      , (524288, "fimf hunnert vierunzwansich dausend zwee hunnert achtunachtzich")
      , (dec 6, "en millyon")
      , (1048576, "en millyon achtunvazich dausend fimf hunnert sexunsiwwezich")
      , (5 ⋅ dec 6, "fimf millyon")
      , (123456789, "en hunnert dreiunzwansich millyon vier hunnert sexunfuffzich dausend siwwe hunnert neinunachtzich")
      ]
    )
  ]
