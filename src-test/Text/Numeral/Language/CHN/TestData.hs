{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        -

[@ISO639-2B@]       chn

[@ISO639-3@]        chn

[@Native name@]     -

[@English name@]    Chinook Jargon
-}

module Text.Numeral.Language.CHN.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude     ( Num )
import "this" Text.Numeral.Grammar.Reified ( defaultInflection )
import "this" Text.Numeral.Test ( TestData )

--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "ikt")
      , (2, "mokst")
      , (3, "klone")
      , (4, "lakit")
      , (5, "kwinnum")
      , (6, "taghum")
      , (7, "sinamokst")
      , (8, "stotekin")
      , (9, "kwaist")
      , (10, "tahtlum")
      , (11, "tahtlum pe ikt")
      , (12, "tahtlum pe mokst")
      , (13, "tahtlum pe klone")
      , (14, "tahtlum pe lakit")
      , (15, "tahtlum pe kwinnum")
      , (16, "tahtlum pe taghum")
      , (17, "tahtlum pe sinamokst")
      , (18, "tahtlum pe stotekin")
      , (19, "tahtlum pe kwaist")
      , (20, "mokst tahtlum")
      , (21, "mokst tahtlum pe ikt")
      , (22, "mokst tahtlum pe mokst")
      , (23, "mokst tahtlum pe klone")
      , (24, "mokst tahtlum pe lakit")
      , (25, "mokst tahtlum pe kwinnum")
      , (26, "mokst tahtlum pe taghum")
      , (27, "mokst tahtlum pe sinamokst")
      , (28, "mokst tahtlum pe stotekin")
      , (29, "mokst tahtlum pe kwaist")
      , (30, "klone tahtlum")
      , (31, "klone tahtlum pe ikt")
      , (32, "klone tahtlum pe mokst")
      , (33, "klone tahtlum pe klone")
      , (34, "klone tahtlum pe lakit")
      , (35, "klone tahtlum pe kwinnum")
      , (36, "klone tahtlum pe taghum")
      , (37, "klone tahtlum pe sinamokst")
      , (38, "klone tahtlum pe stotekin")
      , (39, "klone tahtlum pe kwaist")
      , (40, "lakit tahtlum")
      , (41, "lakit tahtlum pe ikt")
      , (42, "lakit tahtlum pe mokst")
      , (43, "lakit tahtlum pe klone")
      , (44, "lakit tahtlum pe lakit")
      , (45, "lakit tahtlum pe kwinnum")
      , (46, "lakit tahtlum pe taghum")
      , (47, "lakit tahtlum pe sinamokst")
      , (48, "lakit tahtlum pe stotekin")
      , (49, "lakit tahtlum pe kwaist")
      , (50, "kwinnum tahtlum")
      , (51, "kwinnum tahtlum pe ikt")
      , (52, "kwinnum tahtlum pe mokst")
      , (53, "kwinnum tahtlum pe klone")
      , (54, "kwinnum tahtlum pe lakit")
      , (55, "kwinnum tahtlum pe kwinnum")
      , (56, "kwinnum tahtlum pe taghum")
      , (57, "kwinnum tahtlum pe sinamokst")
      , (58, "kwinnum tahtlum pe stotekin")
      , (59, "kwinnum tahtlum pe kwaist")
      , (60, "taghum tahtlum")
      , (61, "taghum tahtlum pe ikt")
      , (62, "taghum tahtlum pe mokst")
      , (63, "taghum tahtlum pe klone")
      , (64, "taghum tahtlum pe lakit")
      , (65, "taghum tahtlum pe kwinnum")
      , (66, "taghum tahtlum pe taghum")
      , (67, "taghum tahtlum pe sinamokst")
      , (68, "taghum tahtlum pe stotekin")
      , (69, "taghum tahtlum pe kwaist")
      , (70, "sinamokst tahtlum")
      , (71, "sinamokst tahtlum pe ikt")
      , (72, "sinamokst tahtlum pe mokst")
      , (73, "sinamokst tahtlum pe klone")
      , (74, "sinamokst tahtlum pe lakit")
      , (75, "sinamokst tahtlum pe kwinnum")
      , (76, "sinamokst tahtlum pe taghum")
      , (77, "sinamokst tahtlum pe sinamokst")
      , (78, "sinamokst tahtlum pe stotekin")
      , (79, "sinamokst tahtlum pe kwaist")
      , (80, "stotekin tahtlum")
      , (81, "stotekin tahtlum pe ikt")
      , (82, "stotekin tahtlum pe mokst")
      , (83, "stotekin tahtlum pe klone")
      , (84, "stotekin tahtlum pe lakit")
      , (85, "stotekin tahtlum pe kwinnum")
      , (86, "stotekin tahtlum pe taghum")
      , (87, "stotekin tahtlum pe sinamokst")
      , (88, "stotekin tahtlum pe stotekin")
      , (89, "stotekin tahtlum pe kwaist")
      , (90, "kwaist tahtlum")
      , (91, "kwaist tahtlum pe ikt")
      , (92, "kwaist tahtlum pe mokst")
      , (93, "kwaist tahtlum pe klone")
      , (94, "kwaist tahtlum pe lakit")
      , (95, "kwaist tahtlum pe kwinnum")
      , (96, "kwaist tahtlum pe taghum")
      , (97, "kwaist tahtlum pe sinamokst")
      , (98, "kwaist tahtlum pe stotekin")
      , (99, "kwaist tahtlum pe kwaist")
      , (100, "tukamonuk")
      ]
    )
  ]
