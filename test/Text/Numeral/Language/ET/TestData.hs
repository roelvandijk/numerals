{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        et

[@ISO639-2@]        est

[@ISO639-3@]        est

[@Native name@]     eesti keel

[@English name@]    Estonian
-}

module Text.Numeral.Language.ET.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "numerals-base" Text.Numeral.Misc ( dec )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

cardinals ∷ (Integral i, IsString s) ⇒ [(i, s)]
cardinals =
  [ (0, "null")
  , (1, "üks")
  , (2, "kaks")
  , (3, "kolm")
  , (4, "neli")
  , (5, "viis")
  , (6, "kuus")
  , (7, "seitse")
  , (8, "kaheksa")
  , (9, "üheksa")
  , (10, "kümme")
  , (11, "üksteist")
  , (12, "kaksteist")
  , (13, "kolmteist")
  , (14, "neliteist")
  , (15, "viisteist")
  , (16, "kuusteist")
  , (17, "seitseteist")
  , (18, "kaheksateist")
  , (19, "üheksateist")
  , (20, "kakskümmend")
  , (21, "kakskümmend üks")
  , (22, "kakskümmend kaks")
  , (23, "kakskümmend kolm")
  , (24, "kakskümmend neli")
  , (25, "kakskümmend viis")
  , (26, "kakskümmend kuus")
  , (27, "kakskümmend seitse")
  , (28, "kakskümmend kaheksa")
  , (29, "kakskümmend üheksa")
  , (30, "kolmkümmend")
  , (31, "kolmkümmend üks")
  , (32, "kolmkümmend kaks")
  , (33, "kolmkümmend kolm")
  , (34, "kolmkümmend neli")
  , (35, "kolmkümmend viis")
  , (36, "kolmkümmend kuus")
  , (37, "kolmkümmend seitse")
  , (38, "kolmkümmend kaheksa")
  , (39, "kolmkümmend üheksa")
  , (40, "nelikümmend")
  , (41, "nelikümmend üks")
  , (42, "nelikümmend kaks")
  , (43, "nelikümmend kolm")
  , (44, "nelikümmend neli")
  , (45, "nelikümmend viis")
  , (46, "nelikümmend kuus")
  , (47, "nelikümmend seitse")
  , (48, "nelikümmend kaheksa")
  , (49, "nelikümmend üheksa")
  , (50, "viiskümmend")
  , (51, "viiskümmend üks")
  , (52, "viiskümmend kaks")
  , (53, "viiskümmend kolm")
  , (54, "viiskümmend neli")
  , (55, "viiskümmend viis")
  , (56, "viiskümmend kuus")
  , (57, "viiskümmend seitse")
  , (58, "viiskümmend kaheksa")
  , (59, "viiskümmend üheksa")
  , (61, "kuuskümmend üks")
  , (62, "kuuskümmend kaks")
  , (63, "kuuskümmend kolm")
  , (64, "kuuskümmend neli")
  , (65, "kuuskümmend viis")
  , (66, "kuuskümmend kuus")
  , (67, "kuuskümmend seitse")
  , (68, "kuuskümmend kaheksa")
  , (69, "kuuskümmend üheksa")
  , (71, "seitsekümmend üks")
  , (72, "seitsekümmend kaks")
  , (73, "seitsekümmend kolm")
  , (74, "seitsekümmend neli")
  , (75, "seitsekümmend viis")
  , (76, "seitsekümmend kuus")
  , (77, "seitsekümmend seitse")
  , (78, "seitsekümmend kaheksa")
  , (79, "seitsekümmend üheksa")
  , (81, "kaheksakümmend üks")
  , (82, "kaheksakümmend kaks")
  , (83, "kaheksakümmend kolm")
  , (84, "kaheksakümmend neli")
  , (85, "kaheksakümmend viis")
  , (86, "kaheksakümmend kuus")
  , (87, "kaheksakümmend seitse")
  , (88, "kaheksakümmend kaheksa")
  , (89, "kaheksakümmend üheksa")
  , (91, "üheksakümmend üks")
  , (92, "üheksakümmend kaks")
  , (93, "üheksakümmend kolm")
  , (94, "üheksakümmend neli")
  , (95, "üheksakümmend viis")
  , (96, "üheksakümmend kuus")
  , (97, "üheksakümmend seitse")
  , (98, "üheksakümmend kaheksa")
  , (99, "üheksakümmend üheksa")
  , (100, "sada")
  , (1000, "tuhat")
  , (dec 6, "miljon")
  , (dec 9, "miljard")
  , (dec 12, "triljon")
  ]
