{-|
[@ISO639-1@]        om

[@ISO639-2@]        orm

[@ISO639-3@]        orm

[@Native name@]     Afaan Oromo

[@English name@]    Oromo
-}
module Text.Numeral.Language.ORM.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-oromo/en/orm/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "duwwa")
      , (1, "tokko")
      , (2, "lama")
      , (3, "sadi")
      , (4, "afur")
      , (5, "shan")
      , (6, "jaya")
      , (7, "torba")
      , (8, "saddeet")
      , (9, "sagal")
      , (10, "khudan")
      , (11, "khuda-tokko")
      , (12, "khuda-lama")
      , (13, "khuda-sadi")
      , (14, "khuda-afur")
      , (15, "khuda-shan")
      , (16, "khuda-jaya")
      , (17, "khuda-torba")
      , (18, "khuda-saddeet")
      , (19, "khuda-sagal")
      , (20, "digdama")
      , (21, "digdamii-tokko")
      , (22, "digdamii-lama")
      , (23, "digdamii-sadi")
      , (24, "digdamii-afur")
      , (25, "digdamii-shan")
      , (26, "digdamii-jaya")
      , (27, "digdamii-torba")
      , (28, "digdamii-saddeet")
      , (29, "digdamii-sagal")
      , (30, "soddoma")
      , (31, "soddomii-tokko")
      , (32, "soddomii-lama")
      , (33, "soddomii-sadi")
      , (34, "soddomii-afur")
      , (35, "soddomii-shan")
      , (36, "soddomii-jaya")
      , (37, "soddomii-torba")
      , (38, "soddomii-saddeet")
      , (39, "soddomii-sagal")
      , (40, "afurtama")
      , (41, "afurtamii-tokko")
      , (42, "afurtamii-lama")
      , (43, "afurtamii-sadi")
      , (44, "afurtamii-afur")
      , (45, "afurtamii-shan")
      , (46, "afurtamii-jaya")
      , (47, "afurtamii-torba")
      , (48, "afurtamii-saddeet")
      , (49, "afurtamii-sagal")
      , (50, "shantama")
      , (51, "shantamii-tokko")
      , (52, "shantamii-lama")
      , (53, "shantamii-sadi")
      , (54, "shantamii-afur")
      , (55, "shantamii-shan")
      , (56, "shantamii-jaya")
      , (57, "shantamii-torba")
      , (58, "shantamii-saddeet")
      , (59, "shantamii-sagal")
      , (60, "jaatama")
      , (61, "jaatamii-tokko")
      , (62, "jaatamii-lama")
      , (63, "jaatamii-sadi")
      , (64, "jaatamii-afur")
      , (65, "jaatamii-shan")
      , (66, "jaatamii-jaya")
      , (67, "jaatamii-torba")
      , (68, "jaatamii-saddeet")
      , (69, "jaatamii-sagal")
      , (70, "torbatama")
      , (71, "torbatamii-tokko")
      , (72, "torbatamii-lama")
      , (73, "torbatamii-sadi")
      , (74, "torbatamii-afur")
      , (75, "torbatamii-shan")
      , (76, "torbatamii-jaya")
      , (77, "torbatamii-torba")
      , (78, "torbatamii-saddeet")
      , (79, "torbatamii-sagal")
      , (80, "saddeetama")
      , (81, "saddeetamii-tokko")
      , (82, "saddeetamii-lama")
      , (83, "saddeetamii-sadi")
      , (84, "saddeetamii-afur")
      , (85, "saddeetamii-shan")
      , (86, "saddeetamii-jaya")
      , (87, "saddeetamii-torba")
      , (88, "saddeetamii-saddeet")
      , (89, "saddeetamii-sagal")
      , (90, "sagaltama")
      , (91, "sagaltamii-tokko")
      , (92, "sagaltamii-lama")
      , (93, "sagaltamii-sadi")
      , (94, "sagaltamii-afur")
      , (95, "sagaltamii-shan")
      , (96, "sagaltamii-jaya")
      , (97, "sagaltamii-torba")
      , (98, "sagaltamii-saddeet")
      , (99, "sagaltamii-sagal")
      , (100, "dhibba")
      , (101, "dhibba tokko")
      , (102, "dhibba lama")
      , (103, "dhibba sadi")
      , (104, "dhibba afur")
      , (105, "dhibba shan")
      , (106, "dhibba jaya")
      , (107, "dhibba torba")
      , (108, "dhibba saddeet")
      , (109, "dhibba sagal")
      , (110, "dhibba khudan")
      , (123, "dhibba digdamii-sadi")
      , (200, "lama dhibba")
      , (300, "sadi dhibba")
      , (321, "sadi dhibba digdamii-tokko")
      , (400, "afur dhibba")
      , (500, "shan dhibba")
      , (600, "jaya dhibba")
      , (700, "torba dhibba")
      , (800, "saddeet dhibba")
      , (900, "sagal dhibba")
      , (909, "sagal dhibba sagal")
      , (990, "sagal dhibba sagaltama")
      , (999, "sagal dhibba sagaltamii-sagal")
      , (1000, "kuma")
      , (1001, "kuma tokko")
      , (1008, "kuma saddeet")
      , (1234, "kuma lama dhibba soddomii-afur")
      , (2000, "lama kuma")
      , (3000, "sadi kuma")
      , (4000, "afur kuma")
      , (4321, "afur kuma sadi dhibba digdamii-tokko")
      , (5000, "shan kuma")
      , (6000, "jaya kuma")
      , (7000, "torba kuma")
      , (8000, "saddeet kuma")
      , (9000, "sagal kuma")
      ]
    )
  ]
