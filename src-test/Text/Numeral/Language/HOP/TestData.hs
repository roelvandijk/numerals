{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        hop

[@Native name@]     Hopilàvayi

[@English name@]    Hopi
-}

module Text.Numeral.Language.HOP.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude ( Integral )
import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

-- Sources:
--   http://www.languagesandnumbers.com/how-to-count-in-hopi/en/hop/
cardinals :: (Integral i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "suukya’")
      , (2, "lööyöm")
      , (3, "pàayom")
      , (4, "naalöyöm")
      , (5, "tsivot")
      , (6, "navay")
      , (7, "tsange’")
      , (8, "nanalt")
      , (9, "pevt")
      , (10, "pakwt")
      , (11, "pakwt niikyang suk siikya’ta")
      , (12, "pakwt niikyang löqmuy siikya’ta")
      , (13, "pakwt niikyang paykomuy siikya’ta")
      , (14, "pakwt niikyang naalöqmuy siikya’ta")
      , (15, "pakwt niikyang tsivot siikya’ta")
      , (16, "pakwt niikyang navay siikya’ta")
      , (17, "pakwt niikyang tsange’ siikya’ta")
      , (18, "pakwt niikyang nanalt siikya’ta")
      , (19, "pakwt niikyang pevt siikya’ta")
      , (20, "sunat")
      , (21, "sunat niikyang suk siikya’ta")
      , (22, "sunat niikyang löqmuy siikya’ta")
      , (23, "sunat niikyang paykomuy siikya’ta")
      , (24, "sunat niikyang naalöqmuy siikya’ta")
      , (25, "sunat niikyang tsivot siikya’ta")
      , (26, "sunat niikyang navay siikya’ta")
      , (27, "sunat niikyang tsange’ siikya’ta")
      , (28, "sunat niikyang nanalt siikya’ta")
      , (29, "sunat niikyang pevt siikya’ta")
      , (30, "payiv pakwt")
      , (31, "payiv pakwt niikyang suk siikya’ta")
      , (32, "payiv pakwt niikyang löqmuy siikya’ta")
      , (33, "payiv pakwt niikyang paykomuy siikya’ta")
      , (34, "payiv pakwt niikyang naalöqmuy siikya’ta")
      , (35, "payiv pakwt niikyang tsivot siikya’ta")
      , (36, "payiv pakwt niikyang navay siikya’ta")
      , (37, "payiv pakwt niikyang tsange’ siikya’ta")
      , (38, "payiv pakwt niikyang nanalt siikya’ta")
      , (39, "payiv pakwt niikyang pevt siikya’ta")
      , (40, "naalöv pakwt")
      , (41, "naalöv pakwt niikyang suk siikya’ta")
      , (42, "naalöv pakwt niikyang löqmuy siikya’ta")
      , (43, "naalöv pakwt niikyang paykomuy siikya’ta")
      , (44, "naalöv pakwt niikyang naalöqmuy siikya’ta")
      , (45, "naalöv pakwt niikyang tsivot siikya’ta")
      , (46, "naalöv pakwt niikyang navay siikya’ta")
      , (47, "naalöv pakwt niikyang tsange’ siikya’ta")
      , (48, "naalöv pakwt niikyang nanalt siikya’ta")
      , (49, "naalöv pakwt niikyang pevt siikya’ta")
      , (50, "tsivotsikiv pakwt")
      , (51, "tsivotsikiv pakwt niikyang suk siikya’ta")
      , (52, "tsivotsikiv pakwt niikyang löqmuy siikya’ta")
      , (53, "tsivotsikiv pakwt niikyang paykomuy siikya’ta")
      , (54, "tsivotsikiv pakwt niikyang naalöqmuy siikya’ta")
      , (55, "tsivotsikiv pakwt niikyang tsivot siikya’ta")
      , (56, "tsivotsikiv pakwt niikyang navay siikya’ta")
      , (57, "tsivotsikiv pakwt niikyang tsange’ siikya’ta")
      , (58, "tsivotsikiv pakwt niikyang nanalt siikya’ta")
      , (59, "tsivotsikiv pakwt niikyang pevt siikya’ta")
      , (60, "navaysikiv pakwt")
      , (61, "navaysikiv pakwt niikyang suk siikya’ta")
      , (62, "navaysikiv pakwt niikyang löqmuy siikya’ta")
      , (63, "navaysikiv pakwt niikyang paykomuy siikya’ta")
      , (64, "navaysikiv pakwt niikyang naalöqmuy siikya’ta")
      , (65, "navaysikiv pakwt niikyang tsivot siikya’ta")
      , (66, "navaysikiv pakwt niikyang navay siikya’ta")
      , (67, "navaysikiv pakwt niikyang tsange’ siikya’ta")
      , (68, "navaysikiv pakwt niikyang nanalt siikya’ta")
      , (69, "navaysikiv pakwt niikyang pevt siikya’ta")
      , (70, "tsange’sikiv pakwt")
      , (71, "tsange’sikiv pakwt niikyang suk siikya’ta")
      , (72, "tsange’sikiv pakwt niikyang löqmuy siikya’ta")
      , (73, "tsange’sikiv pakwt niikyang paykomuy siikya’ta")
      , (74, "tsange’sikiv pakwt niikyang naalöqmuy siikya’ta")
      , (75, "tsange’sikiv pakwt niikyang tsivot siikya’ta")
      , (76, "tsange’sikiv pakwt niikyang navay siikya’ta")
      , (77, "tsange’sikiv pakwt niikyang tsange’ siikya’ta")
      , (78, "tsange’sikiv pakwt niikyang nanalt siikya’ta")
      , (79, "tsange’sikiv pakwt niikyang pevt siikya’ta")
      , (80, "nanalsikiv pakwt")
      , (81, "nanalsikiv pakwt niikyang suk siikya’ta")
      , (82, "nanalsikiv pakwt niikyang löqmuy siikya’ta")
      , (83, "nanalsikiv pakwt niikyang paykomuy siikya’ta")
      , (84, "nanalsikiv pakwt niikyang naalöqmuy siikya’ta")
      , (85, "nanalsikiv pakwt niikyang tsivot siikya’ta")
      , (86, "nanalsikiv pakwt niikyang navay siikya’ta")
      , (87, "nanalsikiv pakwt niikyang tsange’ siikya’ta")
      , (88, "nanalsikiv pakwt niikyang nanalt siikya’ta")
      , (89, "nanalsikiv pakwt niikyang pevt siikya’ta")
      , (90, "peve’sikiv pakwt")
      , (91, "peve’sikiv pakwt niikyang suk siikya’ta")
      , (92, "peve’sikiv pakwt niikyang löqmuy siikya’ta")
      , (93, "peve’sikiv pakwt niikyang paykomuy siikya’ta")
      , (94, "peve’sikiv pakwt niikyang naalöqmuy siikya’ta")
      , (95, "peve’sikiv pakwt niikyang tsivot siikya’ta")
      , (96, "peve’sikiv pakwt niikyang navay siikya’ta")
      , (97, "peve’sikiv pakwt niikyang tsange’ siikya’ta")
      , (98, "peve’sikiv pakwt niikyang nanalt siikya’ta")
      , (99, "peve’sikiv pakwt niikyang pevt siikya’ta")
      , (100, "palotsikiv pakwt")
      ]
    )
  ]
