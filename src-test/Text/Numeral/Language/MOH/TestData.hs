{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        moh

[@ISO639-3@]        moh

[@Native name@]     Kanien’kéha

[@English name@]    Mohawk
-}
module Text.Numeral.Language.MOH.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude     ( Num )
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-acholi/en/moh
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "énska")
      , (2, "tékeni")
      , (3, "áhsen")
      , (4, "kaié:ri")
      , (5, "wisk")
      , (6, "ià:ia’k")
      , (7, "tsá:ta")
      , (8, "sha’té:kon")
      , (9, "tióhton")
      , (10, "oié:ri")
      , (11, "énska iawén:re")
      , (12, "tékeni iawén:re")
      , (13, "áhsen iawén:re")
      , (14, "kaié:ri iawén:re")
      , (15, "wisk iawén:re")
      , (16, "ià:ia’k iawén:re")
      , (17, "tsá:ta iawén:re")
      , (18, "sha’té:kon iawén:re")
      , (19, "tióhton iawén:re")
      , (20, "tewáhsen")
      , (21, "tewáhsen énska")
      , (22, "tewáhsen tékeni")
      , (23, "tewáhsen áhsen")
      , (24, "tewáhsen kaié:ri")
      , (25, "tewáhsen wisk")
      , (26, "tewáhsen ià:ia’k")
      , (27, "tewáhsen tsá:ta")
      , (28, "tewáhsen sha’té:kon")
      , (29, "tewáhsen tióhton")
      , (30, "áhsen niwáhsen")
      , (31, "áhsen niwáhsen énska")
      , (32, "áhsen niwáhsen tékeni")
      , (33, "áhsen niwáhsen áhsen")
      , (34, "áhsen niwáhsen kaié:ri")
      , (35, "áhsen niwáhsen wisk")
      , (36, "áhsen niwáhsen ià:ia’k")
      , (37, "áhsen niwáhsen tsá:ta")
      , (38, "áhsen niwáhsen sha’té:kon")
      , (39, "áhsen niwáhsen tióhton")
      , (40, "kaié:ri niwáhsen")
      , (41, "kaié:ri niwáhsen énska")
      , (42, "kaié:ri niwáhsen tékeni")
      , (43, "kaié:ri niwáhsen áhsen")
      , (44, "kaié:ri niwáhsen kaié:ri")
      , (45, "kaié:ri niwáhsen wisk")
      , (46, "kaié:ri niwáhsen ià:ia’k")
      , (47, "kaié:ri niwáhsen tsá:ta")
      , (48, "kaié:ri niwáhsen sha’té:kon")
      , (49, "kaié:ri niwáhsen tióhton")
      , (51, "wisk niwáhsen énska")
      , (52, "wisk niwáhsen tékeni")
      , (53, "wisk niwáhsen áhsen")
      , (54, "wisk niwáhsen kaié:ri")
      , (55, "wisk niwáhsen wisk")
      , (56, "wisk niwáhsen ià:ia’k")
      , (57, "wisk niwáhsen tsá:ta")
      , (58, "wisk niwáhsen sha’té:kon")
      , (59, "wisk niwáhsen tióhton")
      , (60, "iá:ia’k niwáhsen")
      , (61, "iá:ia’k niwáhsen énska")
      , (62, "iá:ia’k niwáhsen tékeni")
      , (63, "iá:ia’k niwáhsen áhsen")
      , (64, "iá:ia’k niwáhsen kaié:ri")
      , (65, "iá:ia’k niwáhsen wisk")
      , (66, "iá:ia’k niwáhsen ià:ia’k")
      , (67, "iá:ia’k niwáhsen tsá:ta")
      , (68, "iá:ia’k niwáhsen sha’té:kon")
      , (69, "iá:ia’k niwáhsen tióhton")
      , (70, "tsá:ta niwáhsen")
      , (71, "tsá:ta niwáhsen énska")
      , (72, "tsá:ta niwáhsen tékeni")
      , (73, "tsá:ta niwáhsen áhsen")
      , (74, "tsá:ta niwáhsen kaié:ri")
      , (75, "tsá:ta niwáhsen wisk")
      , (76, "tsá:ta niwáhsen ià:ia’k")
      , (77, "tsá:ta niwáhsen tsá:ta")
      , (78, "tsá:ta niwáhsen sha’té:kon")
      , (79, "tsá:ta niwáhsen tióhton")
      , (80, "sha’té:kon niwáhsen")
      , (81, "sha’té:kon niwáhsen énska")
      , (82, "sha’té:kon niwáhsen tékeni")
      , (83, "sha’té:kon niwáhsen áhsen")
      , (84, "sha’té:kon niwáhsen kaié:ri")
      , (85, "sha’té:kon niwáhsen wisk")
      , (86, "sha’té:kon niwáhsen ià:ia’k")
      , (87, "sha’té:kon niwáhsen tsá:ta")
      , (88, "sha’té:kon niwáhsen sha’té:kon")
      , (89, "sha’té:kon niwáhsen tióhton")
      , (90, "tióhton niwáhsen")
      , (91, "tióhton niwáhsen énska")
      , (92, "tióhton niwáhsen tékeni")
      , (93, "tióhton niwáhsen áhsen")
      , (94, "tióhton niwáhsen kaié:ri")
      , (95, "tióhton niwáhsen wisk")
      , (96, "tióhton niwáhsen ià:ia’k")
      , (97, "tióhton niwáhsen tsá:ta")
      , (98, "tióhton niwáhsen sha’té:kon")
      , (99, "tióhton niwáhsen tióhton")
      , (100, "énska tewen’niáwe")
      , (101, "énska tewen’niáwe tánon énska")
      , (102, "énska tewen’niáwe tánon tékeni")
      , (103, "énska tewen’niáwe tánon áhsen")
      , (104, "énska tewen’niáwe tánon kaié:ri")
      , (105, "énska tewen’niáwe tánon wisk")
      , (106, "énska tewen’niáwe tánon ià:ia’k")
      , (107, "énska tewen’niáwe tánon tsá:ta")
      , (108, "énska tewen’niáwe tánon sha’té:kon")
      , (109, "énska tewen’niáwe tánon tióhton")
      , (110, "énska tewen’niáwe oié:ri")
      , (123, "énska tewen’niáwe tewáhsen áhsen")
      , (200, "tékeni tewen’niáwe")
      , (300, "áhsen tewen’niáwe")
      , (321, "áhsen tewen’niáwe tewáhsen énska")
      , (400, "kaié:ri tewen’niáwe")
      , (500, "wisk tewen’niáwe")
      , (600, "ià:ia’k tewen’niáwe")
      , (700, "tsá:ta tewen’niáwe")
      , (800, "sha’té:kon tewen’niáwe")
      , (900, "tióhton tewen’niáwe")
      , (909, "tióhton tewen’niáwe tánon tióhton")
      , (990, "tióhton tewen’niáwe tióhton niwáhsen")
      , (999, "tióhton tewen’niáwe tióhton niwáhsen tióhton")
      , (1000, "oié:ri tewen’niáwe")
      , (1001, "oié:ri tewen’niáwe tánon énska")
      , (1008, "oié:ri tewen’niáwe tánon sha’té:kon")
      , (1234, "oié:ri tewen’niáwe tékeni tewen’niáwe áhsen niwáhsen kaié:ri")
      , (2000, "tewáhsen tewen’niáwe")
      , (3000, "áhsen niwáhsen tewen’niáwe")
      , (4000, "kaié:ri niwáhsen tewen’niáwe")
      , (4321, "kaié:ri niwáhsen tewen’niáwe áhsen tewen’niáwe tewáhsen énska")
      , (5000, "wisk niwáhsen tewen’niáwe")
      , (6000, "iá:ia’k niwáhsen tewen’niáwe")
      , (7000, "tsá:ta niwáhsen tewen’niáwe")
      , (8000, "sha’té:kon niwáhsen tewen’niáwe")
      , (9000, "tióhton niwáhsen tewen’niáwe")
      , (10000, "oié:ri oié:ri tewen’niáwe")
      , (12345, "tékeni iawén:re oié:ri tewen’niáwe áhsen tewen’niáwe kaié:ri niwáhsen wisk")
      , (20000, "tewáhsen niwáhsen tewen’niáwe")
      , (30000, "áhsen niwáhsen niwáhsen tewen’niáwe")
      , (40000, "kaié:ri niwáhsen niwáhsen tewen’niáwe")
      , (50000, "wisk niwáhsen niwáhsen tewen’niáwe")
      , (54321, "wisk niwáhsen kaié:ri niwáhsen tewen’niáwe áhsen tewen’niáwe tewáhsen énska")
      , (60000, "iá:ia’k niwáhsen niwáhsen tewen’niáwe")
      , (70000, "tsá:ta niwáhsen niwáhsen tewen’niáwe")
      , (80000, "sha’té:kon niwáhsen niwáhsen tewen’niáwe")
      , (90000, "tióhton niwáhsen niwáhsen tewen’niáwe")
      , (100000, "énska tewen’niáwe oié:ri tewen’niáwe")
      , (123456, "énska tewen’niáwe tewáhsen áhsen niwáhsen tewen’niáwe kaié:ri tewen’niáwe wisk niwáhsen ià:ia’k")
      , (200000, "tékeni tewen’niáwe oié:ri tewen’niáwe")
      , (300000, "áhsen tewen’niáwe oié:ri tewen’niáwe")
      , (400000, "kaié:ri tewen’niáwe oié:ri tewen’niáwe")
      , (500000, "wisk tewen’niáwe oié:ri tewen’niáwe")
      , (600000, "ià:ia’k tewen’niáwe oié:ri tewen’niáwe")
      , (654321, "ià:ia’k tewen’niáwe wisk niwáhsen kaié:ri niwáhsen tewen’niáwe áhsen tewen’niáwe tewáhsen énska")
      , (700000, "tsá:ta tewen’niáwe oié:ri tewen’niáwe")
      , (800000, "sha’té:kon tewen’niáwe oié:ri tewen’niáwe")
      , (900000, "tióhton tewen’niáwe oié:ri tewen’niáwe")
      , (1000000, "énska million")
      ]
    )
  ]
