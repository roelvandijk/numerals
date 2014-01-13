{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        af

[@ISO639-2@]        afr

[@ISO639-3@]        afr

[@Native name@]     Afrikaans

[@English name@]    Afrikaans
-}

module Text.Numeral.Language.AF.TestData (cardinals, ordinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude       ( Integral )
import "numerals" Text.Numeral.Misc ( dec )
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-afrikaans/en/afr/
  http://mylanguages.org/afrikaans_numbers.php
-}

cardinals ∷ (Integral i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "nul")
      , (1, "een")
      , (2, "twee")
      , (3, "drie")
      , (4, "vier")
      , (5, "vyf")
      , (6, "ses")
      , (7, "sewe")
      , (8, "ag")
      , (9, "nege")
      , (10, "tien")
      , (11, "elf")
      , (12, "twaalf")
      , (13, "dertien")
      , (14, "veertien")
      , (15, "vyftien")
      , (16, "sestien")
      , (17, "sewentien")
      , (18, "agtien")
      , (19, "negentien")
      , (20, "twintig")
      , (21, "een-en-twintig")
      , (22, "twee-en-twintig")
      , (23, "drie-en-twintig")
      , (24, "vier-en-twintig")
      , (25, "vyf-en-twintig")
      , (26, "ses-en-twintig")
      , (27, "sewe-en-twintig")
      , (28, "ag-en-twintig")
      , (29, "nege-en-twintig")
      , (30, "dertig")
      , (31, "een-en-dertig")
      , (32, "twee-en-dertig")
      , (33, "drie-en-dertig")
      , (34, "vier-en-dertig")
      , (35, "vyf-en-dertig")
      , (36, "ses-en-dertig")
      , (37, "sewe-en-dertig")
      , (38, "ag-en-dertig")
      , (39, "nege-en-dertig")
      , (40, "veertig")
      , (50, "vyftig")
      , (60, "sestig")
      , (70, "sewentig")
      , (80, "tagtig")
      , (90, "neëntig")
      , (100, "eenhonderd")
      , (121, "eenhonderd een-en-twintig")
      , (1000, "eenduisend")
      , (1219, "eenduisend tweehonderd negentien")
      , (dec 6, "een miljoen")
      , (dec 9, "een miljard")
      , (dec 12, "een biljoen")
      ]
    )
  ]

ordinals ∷ (Integral i) ⇒ TestData i
ordinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "eerste")
      , (2, "tweede")
      , (3, "derde")
      , (4, "vierde")
      , (5, "vyfde")
      , (6, "sesde")
      , (7, "sewende")
      , (8, "agtste")
      , (9, "negende")
      , (10, "tiende")
      , (11, "elfde")
      , (12, "twaalfde")
      , (13, "dertiende")
      , (14, "veertiende")
      , (15, "vyftiende")
      , (16, "sestiende")
      , (17, "sewentiende")
      , (18, "agtiende")
      , (19, "negentiende")
      , (20, "twintigste")
      ]
    )
  ]
