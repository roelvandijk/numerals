{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        nee

[@Native name@]     Nêlêmwa-Nixumwak

[@English name@]    Kumak
-}
module Text.Numeral.Language.NEE.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "numerals" Text.Numeral.Grammar ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-nelemwa/en/nee/

Note: This data uses only one Nêlêmwa numerical classifier. There are
22 categories.
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "pwa-giik")
      , (2, "pwa-du")
      , (3, "pwa-gan")
      , (4, "pwa-baak")
      , (5, "pwa-nem")
      , (6, "pwa-nem-giik")
      , (7, "pwa-nem-du")
      , (8, "pwa-nem-gan")
      , (9, "pwa-nem-baak")
      , (10, "tujic")
      , (11, "tujic xa bwaat pwagiik")
      , (12, "tujic xa bwaat pwadu")
      , (13, "tujic xa bwaat pwagan")
      , (14, "tujic xa bwaat pwabaak")
      , (15, "tujic xa bwaat pwanem")
      , (16, "tujic xa bwaat pwanemgiik")
      , (17, "tujic xa bwaat pwanemdu")
      , (18, "tujic xa bwaat pwanemgan")
      , (19, "tujic xa bwaat pwanembaak")
      , (20, "aaxi ak")
      , (21, "aaxi ak xa bwaat pwagiik")
      , (22, "aaxi ak xa bwaat pwadu")
      , (23, "aaxi ak xa bwaat pwagan")
      , (24, "aaxi ak xa bwaat pwabaak")
      , (25, "aaxi ak xa bwaat pwanem")
      , (26, "aaxi ak xa bwaat pwanemgiik")
      , (27, "aaxi ak xa bwaat pwanemdu")
      , (28, "aaxi ak xa bwaat pwanemgan")
      , (29, "aaxi ak xa bwaat pwanembaak")
      , (30, "aaxi ak xa bwaat tujic")
      , (31, "aaxi ak xa bwaat tujic xa bwaat pwagiik")
      , (32, "aaxi ak xa bwaat tujic xa bwaat pwadu")
      , (33, "aaxi ak xa bwaat tujic xa bwaat pwagan")
      , (34, "aaxi ak xa bwaat tujic xa bwaat pwabaak")
      , (35, "aaxi ak xa bwaat tujic xa bwaat pwanem")
      , (36, "aaxi ak xa bwaat tujic xa bwaat pwanemgiik")
      , (37, "aaxi ak xa bwaat tujic xa bwaat pwanemdu")
      , (38, "aaxi ak xa bwaat tujic xa bwaat pwanemgan")
      , (39, "aaxi ak xa bwaat tujic xa bwaat pwanembaak")
      , (40, "aaru ak")
      , (41, "aaru ak xa bwaat pwagiik")
      , (42, "aaru ak xa bwaat pwadu")
      , (43, "aaru ak xa bwaat pwagan")
      , (44, "aaru ak xa bwaat pwabaak")
      , (45, "aaru ak xa bwaat pwanem")
      , (46, "aaru ak xa bwaat pwanemgiik")
      , (47, "aaru ak xa bwaat pwanemdu")
      , (48, "aaru ak xa bwaat pwanemgan")
      , (49, "aaru ak xa bwaat pwanembaak")
      , (50, "aaru ak xa bwaat tujic")
      , (51, "aaru ak xa bwaat tujic xa bwaat pwagiik")
      , (52, "aaru ak xa bwaat tujic xa bwaat pwadu")
      , (53, "aaru ak xa bwaat tujic xa bwaat pwagan")
      , (54, "aaru ak xa bwaat tujic xa bwaat pwabaak")
      , (55, "aaru ak xa bwaat tujic xa bwaat pwanem")
      , (56, "aaru ak xa bwaat tujic xa bwaat pwanemgiik")
      , (57, "aaru ak xa bwaat tujic xa bwaat pwanemdu")
      , (58, "aaru ak xa bwaat tujic xa bwaat pwanemgan")
      , (59, "aaru ak xa bwaat tujic xa bwaat pwanembaak")
      , (60, "aaxan ak")
      , (61, "aaxan ak xa bwaat pwagiik")
      , (62, "aaxan ak xa bwaat pwadu")
      , (63, "aaxan ak xa bwaat pwagan")
      , (64, "aaxan ak xa bwaat pwabaak")
      , (65, "aaxan ak xa bwaat pwanem")
      , (66, "aaxan ak xa bwaat pwanemgiik")
      , (67, "aaxan ak xa bwaat pwanemdu")
      , (68, "aaxan ak xa bwaat pwanemgan")
      , (69, "aaxan ak xa bwaat pwanembaak")
      , (70, "aaxan ak xa bwaat tujic")
      , (71, "aaxan ak xa bwaat tujic xa bwaat pwagiik")
      , (72, "aaxan ak xa bwaat tujic xa bwaat pwadu")
      , (73, "aaxan ak xa bwaat tujic xa bwaat pwagan")
      , (74, "aaxan ak xa bwaat tujic xa bwaat pwabaak")
      , (75, "aaxan ak xa bwaat tujic xa bwaat pwanem")
      , (76, "aaxan ak xa bwaat tujic xa bwaat pwanemgiik")
      , (77, "aaxan ak xa bwaat tujic xa bwaat pwanemdu")
      , (78, "aaxan ak xa bwaat tujic xa bwaat pwanemgan")
      , (79, "aaxan ak xa bwaat tujic xa bwaat pwanembaak")
      , (80, "aavaak ak")
      , (81, "aavaak ak xa bwaat pwagiik")
      , (82, "aavaak ak xa bwaat pwadu")
      , (83, "aavaak ak xa bwaat pwagan")
      , (84, "aavaak ak xa bwaat pwabaak")
      , (85, "aavaak ak xa bwaat pwanem")
      , (86, "aavaak ak xa bwaat pwanemgiik")
      , (87, "aavaak ak xa bwaat pwanemdu")
      , (88, "aavaak ak xa bwaat pwanemgan")
      , (89, "aavaak ak xa bwaat pwanembaak")
      , (90, "aavaak ak xa bwaat tujic")
      , (91, "aavaak ak xa bwaat tujic xa bwaat pwagiik")
      , (92, "aavaak ak xa bwaat tujic xa bwaat pwadu")
      , (93, "aavaak ak xa bwaat tujic xa bwaat pwagan")
      , (94, "aavaak ak xa bwaat tujic xa bwaat pwabaak")
      , (95, "aavaak ak xa bwaat tujic xa bwaat pwanem")
      , (96, "aavaak ak xa bwaat tujic xa bwaat pwanemgiik")
      , (97, "aavaak ak xa bwaat tujic xa bwaat pwanemdu")
      , (98, "aavaak ak xa bwaat tujic xa bwaat pwanemgan")
      , (99, "aavaak ak xa bwaat tujic xa bwaat pwanembaak")
      , (100, "aanem ak")
      ]
    )
  ]
