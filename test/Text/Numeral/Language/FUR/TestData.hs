{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        fur

[@ISO639-3@]        fur

[@Native name@]     Furlan

[@English name@]    Friulan
-}

module Text.Numeral.Language.FUR.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "base-unicode-symbols" Prelude.Unicode ( (⋅) )
import "numerals-base" Text.Numeral.Grammar
import "numerals-base" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals-base" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

-- Sources:
--   http://www.languagesandnumbers.com/how-to-count-in-friulian/en/fur/
cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "masculine"
    , masculine defaultInflection
    , [ (0, "zero")
      , (1, "un")
      , (2, "doi")
      , (3, "trê")
      , (4, "cuatri")
      , (5, "cinc")
      , (6, "sîs")
      , (7, "siet")
      , (8, "vot")
      , (9, "nûf")
      , (10, "dîs")
      , (11, "undis")
      , (12, "dodis")
      , (13, "tredis")
      , (14, "cutuardis")
      , (15, "cuindis")
      , (16, "sedis")
      , (17, "disesiet")
      , (18, "disevot")
      , (19, "disenûf")
      , (20, "vincj")
      , (21, "vincjeun")
      , (30, "trente")
      , (32, "trentedoi")
      , (40, "cuarante")
      , (48, "cuarantevot")
      , (50, "cincuante")
      , (60, "sessante")
      , (70, "setante")
      , (80, "otante")
      , (90, "novante")
      , (100, "cent")
      , (109, "cent e nûf")
      , (200, "dusinte")
      , (230, "dusinte e trente")
      , (300, "tresinte")
      , (400, "cuatricent")
      , (500, "cinccent")
      , (600, "sîscent")
      , (700, "sietcent")
      , (800, "votcent")
      , (900, "nûfcent")
      , (999, "nûfcent e novantenûf")
      , (1000, "mil")
      , (2000, "doi mil")
      , (3000, "trê mil")
      , (4000, "cuatri mil")
      , (dec 6, "un milion")
      , (2 ⋅ dec 6, "doi milions")
      , (3 ⋅ dec 6, "trê milions")
      , (dec 9, "un miliart")
      , (2 ⋅ dec 9, "doi miliarts")
      , (3 ⋅ dec 9, "trê miliarts")
      ]
    )
  , ( "feminine"
    , feminine defaultInflection
    , [ (1, "une")
      , (2, "dôs")
      , (2 ⋅ dec 6, "dôs milions")
      , (2 ⋅ dec 9, "dôs miliarts")
      ]
    )
  ]
