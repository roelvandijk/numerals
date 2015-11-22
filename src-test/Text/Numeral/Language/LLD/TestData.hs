{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        lld

[@Native name@]     Ladin

[@English name@]    Ladin
-}

module Text.Numeral.Language.LLD.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude     ( Integral, (+) )
import "numerals" Text.Numeral.Grammar
import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "numerals" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

-- Sources:
--   http://www.languagesandnumbers.com/how-to-count-in-ladin/en/lld/
cardinals :: (Integral i) => TestData i
cardinals =
  [ ( "masculine"
    , masculine defaultInflection
    , [ (0, "zero")
      , (1, "un")
      , (2, "doi")
      , (3, "trei")
      , (4, "cater")
      , (5, "cinch")
      , (6, "sies")
      , (7, "set")
      , (8, "ot")
      , (9, "nuef")
      , (10, "diesc")
      , (11, "undesc")
      , (12, "dodesc")
      , (13, "tredesc")
      , (14, "catordesc")
      , (15, "chindesc")
      , (16, "seidesc")
      , (17, "dejesset")
      , (18, "dejedot")
      , (19, "dejenuef")
      , (20, "vint")
      , (21, "vintun")
      , (22, "vintedoi")
      , (23, "vintetrei")
      , (24, "vintecater")
      , (25, "vintecinch")
      , (26, "vintesies")
      , (27, "vinteset")
      , (28, "vintot")
      , (29, "vintenuef")
      , (30, "trenta")
      , (31, "trentun")
      , (32, "trentedoi")
      , (33, "trentetrei")
      , (34, "trentecater")
      , (35, "trentecinch")
      , (36, "trentesies")
      , (37, "trenteset")
      , (38, "trenteot")
      , (39, "trentenuef")
      , (40, "caranta")
      , (41, "carantun")
      , (42, "carantedoi")
      , (43, "carantetrei")
      , (44, "carantecater")
      , (45, "carantecinch")
      , (46, "carantesies")
      , (47, "caranteset")
      , (48, "carantot")
      , (49, "carantenuef")
      , (50, "cincanta")
      , (51, "cincantun")
      , (52, "cincantedoi")
      , (53, "cincantetrei")
      , (54, "cincantecater")
      , (55, "cincantecinch")
      , (56, "cincantesies")
      , (57, "cincanteset")
      , (58, "cincantot")
      , (59, "cincantenuef")
      , (60, "sessanta")
      , (70, "setanta")
      , (80, "otanta")
      , (90, "nonanta")
      , (100, "cent")
      , (101, "centeun")
      , (102, "centedoi")
      , (103, "centetrei")
      , (200, "doicent")
      , (300, "treicent")
      , (400, "catercent")
      , (401, "catercenteun")
      , (402, "catercentedoi")
      , (403, "catercentetrei")
      , (500, "cinchcent")
      , (600, "siesçent")
      , (700, "setcent")
      , (800, "otcent")
      , (900, "nuefcent")
      , (999, "nuefcentenonantenuef")
      , (1000, "mile")
      , (1001, "mileun")
      , (1100, "milecent")
      , (1110, "milecentediesc")
      , (1234, "miledoicentetrentecater")
      , (2000, "doimile")
      , (3000, "treimile")
      , (4000, "catermile")
      , (5000, "cinchmile")
      , (5678, "cinchmilesiesçentesetantot")
      , (6000, "siesmile")
      , (7000, "setmile")
      , (8000, "otmile")
      , (9000, "nuefmile")
      , (12345, "dodescmiletreicentecarantecinch")
      , (dec 6, "un milion")
      , (dec 6 + 1, "un milioneun")
      , (2 * dec 6, "doi milion")
      , (123456789, "centevintetrei milionecatercentecincantesiesmilesetcenteotantenuef")
      , (dec 9, "un miliard")
      , (2 * dec 9, "doi miliard")
      , (9876543210, "nuef miliardeotcentesetantesies milionecinchcentecarantetreimiledoicentediesc")
      , (9999999999, "nuef miliardenuefcentenonantenuef milionenuefcentenonantenuefmilenuefcentenonantenuef")
      ]
    )
  , ( "feminine"
    , feminine defaultInflection
    , [ (1, "una")
      , (2, "does")
      ]
    )
  ]
