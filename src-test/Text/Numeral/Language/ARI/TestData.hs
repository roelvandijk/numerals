{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        ari

[@Native name@]     -

[@English name@]    Arikara
-}
module Text.Numeral.Language.ARI.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude ( Num )
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-arikara/en/ari/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "áxkux")
      , (2, "pítkux")
      , (3, "táwit")
      , (4, "čiitíʾiš")
      , (5, "šíhux")
      , (6, "tšaápis")
      , (7, "tawišaapiswaána")
      , (8, "tawišaápis")
      , (9, "nooxiniiwaána")
      , (10, "nooxíniʾ")
      , (11, "nooxini na áxkux")
      , (12, "nooxini na pítkux")
      , (13, "nooxini na táwit")
      , (14, "nooxini na čiitíʾiš")
      , (15, "nooxini na šíhux")
      , (16, "nooxini na tšaápis")
      , (17, "nooxini na tawišaapiswaána")
      , (18, "nooxini na tawišaápis")
      , (19, "nooxini na nooxiniiwaána")
      , (20, "wiitáʾuʾ")
      , (21, "wiitáʾu na áxkux")
      , (22, "wiitáʾu na pítkux")
      , (23, "wiitáʾu na táwit")
      , (24, "wiitáʾu na čiitíʾiš")
      , (25, "wiitáʾu na šíhux")
      , (26, "wiitáʾu na tšaápis")
      , (27, "wiitáʾu na tawišaapiswaána")
      , (28, "wiitáʾu na tawišaápis")
      , (29, "wiitáʾu na nooxiniiwaána")
      , (30, "nasaawíʾuʾ")
      , (31, "nasaawiʾu na áxkux")
      , (32, "nasaawiʾu na pítkux")
      , (33, "nasaawiʾu na táwit")
      , (34, "nasaawiʾu na čiitíʾiš")
      , (35, "nasaawiʾu na šíhux")
      , (36, "nasaawiʾu na tšaápis")
      , (37, "nasaawiʾu na tawišaapiswaána")
      , (38, "nasaawiʾu na tawišaápis")
      , (39, "nasaawiʾu na nooxiniiwaána")
      , (40, "pitkuxunaánuʾ")
      , (41, "pitkuxunaanu na áxkux")
      , (42, "pitkuxunaanu na pítkux")
      , (43, "pitkuxunaanu na táwit")
      , (44, "pitkuxunaanu na čiitíʾiš")
      , (45, "pitkuxunaanu na šíhux")
      , (46, "pitkuxunaanu na tšaápis")
      , (47, "pitkuxunaanu na tawišaapiswaána")
      , (48, "pitkuxunaanu na tawišaápis")
      , (49, "pitkuxunaanu na nooxiniiwaána")
      , (50, "pitkuxunaánuʾ na nooxíniʾ")
      , (51, "pitkuxunaanu na nooxini na áxkux")
      , (52, "pitkuxunaanu na nooxini na pítkux")
      , (53, "pitkuxunaanu na nooxini na táwit")
      , (54, "pitkuxunaanu na nooxini na čiitíʾiš")
      , (55, "pitkuxunaanu na nooxini na šíhux")
      , (56, "pitkuxunaanu na nooxini na tšaápis")
      , (57, "pitkuxunaanu na nooxini na tawišaapiswaána")
      , (58, "pitkuxunaanu na nooxini na tawišaápis")
      , (59, "pitkuxunaanu na nooxini na nooxiniiwaána")
      , (60, "tawihkunaánuʾ")
      , (61, "tawihkunaanu na áxkux")
      , (62, "tawihkunaanu na pítkux")
      , (63, "tawihkunaanu na táwit")
      , (64, "tawihkunaanu na čiitíʾiš")
      , (65, "tawihkunaanu na šíhux")
      , (66, "tawihkunaanu na tšaápis")
      , (67, "tawihkunaanu na tawišaapiswaána")
      , (68, "tawihkunaanu na tawišaápis")
      , (69, "tawihkunaanu na nooxiniiwaána")
      , (70, "tawihkunaánuʾ na nooxíniʾ")
      , (71, "tawihkunaánu nooxíni na áxkux")
      , (72, "tawihkunaánu nooxíni na pítkux")
      , (73, "tawihkunaánu nooxíni na táwit")
      , (74, "tawihkunaánu nooxíni na čiitíʾiš")
      , (75, "tawihkunaánu nooxíni na šíhux")
      , (76, "tawihkunaánu nooxíni na tšaápis")
      , (77, "tawihkunaánu nooxíni na tawišaapiswaána")
      , (78, "tawihkunaánu nooxíni na tawišaápis")
      , (79, "tawihkunaánu nooxíni na nooxiniiwaána")
      , (80, "čiitiʾištaánuʾ")
      , (81, "čiitiʾištaanu na áxkux")
      , (82, "čiitiʾištaanu na pítkux")
      , (83, "čiitiʾištaanu na táwit")
      , (84, "čiitiʾištaanu na čiitíʾiš")
      , (85, "čiitiʾištaanu na šíhux")
      , (86, "čiitiʾištaanu na tšaápis")
      , (87, "čiitiʾištaanu na tawišaapiswaána")
      , (88, "čiitiʾištaanu na tawišaápis")
      , (89, "čiitiʾištaanu na nooxiniiwaána")
      , (90, "čiitiʾištaanu na nooxíniʾ")
      , (91, "čiitiʾištaanu nooxíni na áxkux")
      , (92, "čiitiʾištaanu nooxíni na pítkux")
      , (93, "čiitiʾištaanu nooxíni na táwit")
      , (94, "čiitiʾištaanu nooxíni na čiitíʾiš")
      , (95, "čiitiʾištaanu nooxíni na šíhux")
      , (96, "čiitiʾištaanu nooxíni na tšaápis")
      , (97, "čiitiʾištaanu nooxíni na tawišaapiswaána")
      , (98, "čiitiʾištaanu nooxíni na tawišaápis")
      , (99, "čiitiʾištaanu nooxíni na nooxiniiwaána")
      , (100, "šihuxtaánuʾ")
      , (101, "šihuxtaánuʾ na áxkux")
      , (102, "šihuxtaánuʾ na pítkux")
      , (103, "šihuxtaánuʾ na táwit")
      , (104, "šihuxtaánuʾ na čiitíʾiš")
      , (105, "šihuxtaánuʾ na šíhux")
      , (106, "šihuxtaánuʾ na tšaápis")
      , (107, "šihuxtaánuʾ na tawišaapiswaána")
      , (108, "šihuxtaánuʾ na tawišaápis")
      , (109, "šihuxtaánuʾ na nooxiniiwaána")
      , (110, "šihuxtaánuʾ na nooxíniʾ")
      , (123, "šihuxtaánuʾ na wiitáʾu na táwit")
      , (200, "pitkux šihuxtaánuʾ")
      , (300, "tawit šihuxtaánuʾ")
      , (321, "tawit šihuxtaánuʾ na wiitáʾu na áxkux")
      , (400, "čiitiʾiš šihuxtaánuʾ")
      , (500, "šíhux šihuxtaánuʾ")
      , (600, "tšaápis šihuxtaánuʾ")
      , (700, "tawišaapiswaána šihuxtaánuʾ")
      , (800, "tawišaápis šihuxtaánuʾ")
      , (900, "nooxiniiwaána šihuxtaánuʾ")
      , (909, "nooxiniiwaána šihuxtaánuʾ na nooxiniiwaána")
      , (990, "nooxiniiwaána šihuxtaánuʾ na čiitiʾištaanu na nooxíniʾ")
      , (999, "nooxiniiwaána šihuxtaánuʾ na čiitiʾištaanu nooxíni na nooxiniiwaána")
      , (1000, "nooxininaánuʾ")
      , (1001, "nooxininaánuʾ na áxkux")
      , (1008, "nooxininaánuʾ na tawišaápis")
      , (1234, "nooxininaánuʾ na pitkux šihuxtaánuʾ na nasaawiʾu na čiitíʾiš")
      , (2000, "pitkux nooxininaánuʾ")
      , (3000, "tawit nooxininaánuʾ")
      , (4000, "čiitiʾiš nooxininaánuʾ")
      , (4321, "čiitiʾiš nooxininaánuʾ na tawit šihuxtaánuʾ na wiitáʾu na áxkux")
      , (5000, "šíhux nooxininaánuʾ")
      , (6000, "tšaápis nooxininaánuʾ")
      , (7000, "tawišaapiswaána nooxininaánuʾ")
      , (8000, "tawišaápis nooxininaánuʾ")
      , (9000, "nooxiniiwaána nooxininaánuʾ")
      , (10000, "nooxíniʾ nooxininaánuʾ")
      , (12345, "nooxini na pitkux nooxininaánuʾ na tawit šihuxtaánuʾ na pitkuxunaanu na šíhux")
      , (20000, "wiitáʾuʾ nooxininaánuʾ")
      , (30000, "nasaawíʾuʾ nooxininaánuʾ")
      , (40000, "pitkuxunaánuʾ nooxininaánuʾ")
      , (50000, "pitkuxunaánuʾ na nooxíniʾ nooxininaánuʾ")
      , (54321, "pitkuxunaanu na nooxini na čiitiʾiš nooxininaánuʾ na tawit šihuxtaánuʾ na wiitáʾu na áxkux")
      , (60000, "tawihkunaánuʾ nooxininaánuʾ")
      , (70000, "tawihkunaánuʾ na nooxíniʾ nooxininaánuʾ")
      , (80000, "čiitiʾištaánuʾ nooxininaánuʾ")
      , (90000, "čiitiʾištaanu na nooxíniʾ nooxininaánuʾ")
      , (100000, "šihuxtaánuʾ nooxininaánuʾ")
      , (123456, "šihuxtaánuʾ na wiitáʾu na tawit nooxininaánuʾ na čiitiʾiš šihuxtaánuʾ na pitkuxunaanu na nooxini na tšaápis")
      , (200000, "pitkux šihuxtaánuʾ nooxininaánuʾ")
      , (300000, "tawit šihuxtaánuʾ nooxininaánuʾ")
      , (400000, "čiitiʾiš šihuxtaánuʾ nooxininaánuʾ")
      , (500000, "šíhux šihuxtaánuʾ nooxininaánuʾ")
      , (600000, "tšaápis šihuxtaánuʾ nooxininaánuʾ")
      , (654321, "tšaápis šihuxtaánuʾ na pitkuxunaanu na nooxini na čiitiʾiš nooxininaánuʾ na tawit šihuxtaánuʾ na wiitáʾu na áxkux")
      , (700000, "tawišaapiswaána šihuxtaánuʾ nooxininaánuʾ")
      , (800000, "tawišaápis šihuxtaánuʾ nooxininaánuʾ")
      , (900000, "nooxiniiwaána šihuxtaánuʾ nooxininaánuʾ")
      , (1000000, "axku-hunaánuʾ")
      ]
    )
  ]
