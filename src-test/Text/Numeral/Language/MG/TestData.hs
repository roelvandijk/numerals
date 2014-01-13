{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        mg

[@ISO639-2@]        mlg

[@ISO639-3@]        mlg

[@Native name@]     -

[@English name@]    Malagasy
-}

module Text.Numeral.Language.MG.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude     ( Integral )
import "numerals" Text.Numeral.Misc ( dec )
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://en.wikipedia.org/wiki/Malagasy_language
  http://www.sf.airnet.ne.jp/~ts/language/number/malagasy.html
-}

cardinals ∷ (Integral i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "haotra")
      , (1, "iray")
      , (2, "roa")
      , (3, "telo")
      , (4, "efatra")
      , (5, "dimy")
      , (6, "enina")
      , (7, "fito")
      , (8, "valo")
      , (9, "sivy")
      , (10, "folo")
      , (11, "iraika ambin'ny folo")
      , (12, "roa ambin'ny folo")
      , (13, "telo ambin'ny folo")
      , (14, "efatra ambin'ny folo")
      , (15, "dimy ambin'ny folo")
      , (16, "enina ambin'ny folo")
      , (17, "fito ambin'ny folo")
      , (18, "valo ambin'ny folo")
      , (19, "sivy ambin'ny folo")
      , (20, "roapolo")
      , (21, "iraika amby roapolo")
      , (22, "roa amby roapolo")
      , (23, "telo amby roapolo")
      , (24, "efatra amby roapolo")
      , (25, "dimy amby roapolo")
      , (26, "enina amby roapolo")
      , (27, "fito amby roapolo")
      , (28, "valo amby roapolo")
      , (29, "sivy amby roapolo")
      , (30, "telopolo")
      , (31, "iraika amby telopolo")
      , (32, "roa amby telopolo")
      , (33, "telo amby telopolo")
      , (34, "efatra amby telopolo")
      , (35, "dimy amby telopolo")
      , (36, "enina amby telopolo")
      , (37, "fito amby telopolo")
      , (38, "valo amby telopolo")
      , (39, "sivy amby telopolo")
      , (40, "efapolo")
      , (41, "iraika amby efapolo")
      , (42, "roa amby efapolo")
      , (43, "telo amby efapolo")
      , (44, "efatra amby efapolo")
      , (45, "dimy amby efapolo")
      , (46, "enina amby efapolo")
      , (47, "fito amby efapolo")
      , (48, "valo amby efapolo")
      , (49, "sivy amby efapolo")
      , (50, "dimampolo")
      , (51, "iraika amby dimampolo")
      , (52, "roa amby dimampolo")
      , (53, "telo amby dimampolo")
      , (54, "efatra amby dimampolo")
      , (55, "dimy amby dimampolo")
      , (56, "enina amby dimampolo")
      , (57, "fito amby dimampolo")
      , (58, "valo amby dimampolo")
      , (59, "sivy amby dimampolo")
      , (60, "enimpolo")
      , (61, "iraika amby enimpolo")
      , (62, "roa amby enimpolo")
      , (63, "telo amby enimpolo")
      , (64, "efatra amby enimpolo")
      , (65, "dimy amby enimpolo")
      , (66, "enina amby enimpolo")
      , (67, "fito amby enimpolo")
      , (68, "valo amby enimpolo")
      , (69, "sivy amby enimpolo")
      , (70, "fitopolo")
      , (71, "iraika amby fitopolo")
      , (72, "roa amby fitopolo")
      , (73, "telo amby fitopolo")
      , (74, "efatra amby fitopolo")
      , (75, "dimy amby fitopolo")
      , (76, "enina amby fitopolo")
      , (77, "fito amby fitopolo")
      , (78, "valo amby fitopolo")
      , (79, "sivy amby fitopolo")
      , (80, "valopolo")
      , (81, "iraika amby valopolo")
      , (82, "roa amby valopolo")
      , (83, "telo amby valopolo")
      , (84, "efatra amby valopolo")
      , (85, "dimy amby valopolo")
      , (86, "enina amby valopolo")
      , (87, "fito amby valopolo")
      , (88, "valo amby valopolo")
      , (89, "sivy amby valopolo")
      , (90, "sivifolo")
      , (91, "iraika amby sivifolo")
      , (92, "roa amby sivifolo")
      , (93, "telo amby sivifolo")
      , (94, "efatra amby sivifolo")
      , (95, "dimy amby sivifolo")
      , (96, "enina amby sivifolo")
      , (97, "fito amby sivifolo")
      , (98, "valo amby sivifolo")
      , (99, "sivy amby sivifolo")
      , (100, "zato")
      , (200, "roanjato")
      , (300, "telonjato")
      , (400, "efajato")
      , (500, "dimanjato")
      , (600, "eninjato")
      , (700, "fitonjato")
      , (800, "valonjato")
      , (900, "sivinjato")
      , (1000, "arivo")
      , (dec 4, "alina")
      , (dec 5, "hetsy")
      , (123456, "enina amby dimampolo sy efajato sy telo arivo sy roa alina sy hetsy")
      , (dec 6, "tapitrisa")
      ]
    )
  ]
