{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        tgs

[@Native name@]     -

[@English name@]    Nume
-}
module Text.Numeral.Language.TGS.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-nume/en/tgs/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "tuwal")
      , (2, "i-ru")
      , (3, "i-tol")
      , (4, "i-βet")
      , (5, "tiβi-lim")
      , (6, "leβe-te")
      , (7, "leβe-ru")
      , (8, "leβe-tol")
      , (9, "leβe-βet")
      , (10, "sanuβul tuwal")
      , (11, "sanuβul tuwal ndi win tuwal")
      , (12, "sanuβul tuwal ndi win ti-ru")
      , (13, "sanuβul tuwal ndi win ti-tol")
      , (14, "sanuβul tuwal ndi win ti-βet")
      , (15, "sanuβul tuwal ndi win tiβi-lim")
      , (16, "sanuβul tuwal ndi win ti-leβe-te")
      , (17, "sanuβul tuwal ndi win ti-leβe-ru")
      , (18, "sanuβul tuwal ndi win ti-leβe-tol")
      , (19, "sanuβul tuwal ndi win ti-leβe-βet")
      , (20, "sanuβul ru")
      , (21, "sanuβul ru ndi win tuwal")
      , (22, "sanuβul ru ndi win ti-ru")
      , (23, "sanuβul ru ndi win ti-tol")
      , (24, "sanuβul ru ndi win ti-βet")
      , (25, "sanuβul ru ndi win tiβi-lim")
      , (26, "sanuβul ru ndi win ti-leβe-te")
      , (27, "sanuβul ru ndi win ti-leβe-ru")
      , (28, "sanuβul ru ndi win ti-leβe-tol")
      , (29, "sanuβul ru ndi win ti-leβe-βet")
      , (30, "sanuβul tol")
      , (31, "sanuβul tol ndi win tuwal")
      , (32, "sanuβul tol ndi win ti-ru")
      , (33, "sanuβul tol ndi win ti-tol")
      , (34, "sanuβul tol ndi win ti-βet")
      , (35, "sanuβul tol ndi win tiβi-lim")
      , (36, "sanuβul tol ndi win ti-leβe-te")
      , (37, "sanuβul tol ndi win ti-leβe-ru")
      , (38, "sanuβul tol ndi win ti-leβe-tol")
      , (39, "sanuβul tol ndi win ti-leβe-βet")
      , (40, "sanuβul βet")
      , (41, "sanuβul βet ndi win tuwal")
      , (42, "sanuβul βet ndi win ti-ru")
      , (43, "sanuβul βet ndi win ti-tol")
      , (44, "sanuβul βet ndi win ti-βet")
      , (45, "sanuβul βet ndi win tiβi-lim")
      , (46, "sanuβul βet ndi win ti-leβe-te")
      , (47, "sanuβul βet ndi win ti-leβe-ru")
      , (48, "sanuβul βet ndi win ti-leβe-tol")
      , (49, "sanuβul βet ndi win ti-leβe-βet")
      , (50, "sanuβul tiβi-lim")
      , (51, "sanuβul tiβi-lim ndi win tuwal")
      , (52, "sanuβul tiβi-lim ndi win ti-ru")
      , (53, "sanuβul tiβi-lim ndi win ti-tol")
      , (54, "sanuβul tiβi-lim ndi win ti-βet")
      , (55, "sanuβul tiβi-lim ndi win tiβi-lim")
      , (56, "sanuβul tiβi-lim ndi win ti-leβe-te")
      , (57, "sanuβul tiβi-lim ndi win ti-leβe-ru")
      , (58, "sanuβul tiβi-lim ndi win ti-leβe-tol")
      , (59, "sanuβul tiβi-lim ndi win ti-leβe-βet")
      , (60, "sanuβul leβe-te")
      , (61, "sanuβul leβe-te ndi win tuwal")
      , (62, "sanuβul leβe-te ndi win ti-ru")
      , (63, "sanuβul leβe-te ndi win ti-tol")
      , (64, "sanuβul leβe-te ndi win ti-βet")
      , (65, "sanuβul leβe-te ndi win tiβi-lim")
      , (66, "sanuβul leβe-te ndi win ti-leβe-te")
      , (67, "sanuβul leβe-te ndi win ti-leβe-ru")
      , (68, "sanuβul leβe-te ndi win ti-leβe-tol")
      , (69, "sanuβul leβe-te ndi win ti-leβe-βet")
      , (70, "sanuβul leβe-ru")
      , (71, "sanuβul leβe-ru ndi win tuwal")
      , (72, "sanuβul leβe-ru ndi win ti-ru")
      , (73, "sanuβul leβe-ru ndi win ti-tol")
      , (74, "sanuβul leβe-ru ndi win ti-βet")
      , (75, "sanuβul leβe-ru ndi win tiβi-lim")
      , (76, "sanuβul leβe-ru ndi win ti-leβe-te")
      , (77, "sanuβul leβe-ru ndi win ti-leβe-ru")
      , (78, "sanuβul leβe-ru ndi win ti-leβe-tol")
      , (79, "sanuβul leβe-ru ndi win ti-leβe-βet")
      , (80, "sanuβul leβe-tol")
      , (81, "sanuβul leβe-tol ndi win tuwal")
      , (82, "sanuβul leβe-tol ndi win ti-ru")
      , (83, "sanuβul leβe-tol ndi win ti-tol")
      , (84, "sanuβul leβe-tol ndi win ti-βet")
      , (85, "sanuβul leβe-tol ndi win tiβi-lim")
      , (86, "sanuβul leβe-tol ndi win ti-leβe-te")
      , (87, "sanuβul leβe-tol ndi win ti-leβe-ru")
      , (88, "sanuβul leβe-tol ndi win ti-leβe-tol")
      , (89, "sanuβul leβe-tol ndi win ti-leβe-βet")
      , (90, "sanuβul leβe-βet")
      , (91, "sanuβul leβe-βet ndi win tuwal")
      , (92, "sanuβul leβe-βet ndi win ti-ru")
      , (93, "sanuβul leβe-βet ndi win ti-tol")
      , (94, "sanuβul leβe-βet ndi win ti-βet")
      , (95, "sanuβul leβe-βet ndi win tiβi-lim")
      , (96, "sanuβul leβe-βet ndi win ti-leβe-te")
      , (97, "sanuβul leβe-βet ndi win ti-leβe-ru")
      , (98, "sanuβul leβe-βet ndi win ti-leβe-tol")
      , (99, "sanuβul leβe-βet ndi win ti-leβe-βet")
      , (100, "muweldul")
      , (101, "muweldul ndi win tuwal")
      , (102, "muweldul ndi win ti-ru")
      , (103, "muweldul ndi win ti-tol")
      , (104, "muweldul ndi win ti-βet")
      , (105, "muweldul ndi win tiβi-lim")
      , (106, "muweldul ndi win ti-leβe-te")
      , (107, "muweldul ndi win ti-leβe-ru")
      , (108, "muweldul ndi win ti-leβe-tol")
      , (109, "muweldul ndi win ti-leβe-βet")
      , (110, "muweldul sanuβul tuwal")
      , (123, "muweldul sanuβul ru ndi win ti-tol")
      , (200, "muweldul i-ru")
      , (300, "muweldul i-tol")
      , (321, "muweldul i-tol sanuβul ru ndi win tuwal")
      , (400, "muweldul i-βet")
      , (500, "muweldul tiβi-lim")
      , (600, "muweldul leβe-te")
      , (700, "muweldul leβe-ru")
      , (800, "muweldul leβe-tol")
      , (900, "muweldul leβe-βet")
      , (909, "muweldul leβe-βet ndi win ti-leβe-βet")
      , (990, "muweldul leβe-βet sanuβul leβe-βet")
      , (999, "muweldul leβe-βet sanuβul leβe-βet ndi win ti-leβe-βet")
      ]
    )
  ]
