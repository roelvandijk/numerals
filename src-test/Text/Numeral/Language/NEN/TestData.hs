{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        nen

[@Native name@]     -

[@English name@]    Nengone
-}
module Text.Numeral.Language.NEN.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-nengone/en/nen/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "sa")
      , (2, "rewe")
      , (3, "tini")
      , (4, "ece")
      , (5, "sedong")
      , (6, "sedong ne sa")
      , (7, "sedong ne rew")
      , (8, "sedong ne tin")
      , (9, "sedong ne ec")
      , (10, "ruenin")
      , (11, "ruenin ne sa")
      , (12, "ruenin ne rew")
      , (13, "ruenin ne tin")
      , (14, "ruenin ne ec")
      , (15, "adenin")
      , (16, "adenin ne sa")
      , (17, "adenin ne rew")
      , (18, "adenin ne tin")
      , (19, "adenin ne ec")
      , (20, "sarengom")
      , (21, "sarengom ne sa")
      , (22, "sarengom ne rew")
      , (23, "sarengom ne tin")
      , (24, "sarengom ne ec")
      , (25, "sarengom ne sedong")
      , (26, "sarengom ne sedosa")
      , (27, "sarengom ne sedorew")
      , (28, "sarengom ne sedotin")
      , (29, "sarengom ne sedoec")
      , (30, "sarengom ne ruenin")
      ]
    )
  ]
