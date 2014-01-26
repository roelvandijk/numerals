{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-|
[@ISO639-1@]        it

[@ISO639-2B@]       ita

[@ISO639-3@]        ita

[@Native name@]     Italiano

[@English name@]    Italian
-}

module Text.Numeral.Language.ITA.TestData (cardinals, ordinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude     ( Integral )
import "base-unicode-symbols" Prelude.Unicode ( (⋅) )
import "numerals" Text.Numeral.Grammar
import "numerals" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

-- Sources:
--   http://www.sf.airnet.ne.jp/~ts/language/number/italian.html
--   http://www.orbilat.com/Languages/Italian/Grammar/Italian-Numerals.html
--   http://italian.about.com/library/weekly/aa042600a.htm
--   http://www.suite101.com/content/how-to-count-in-italian-a146487

cardinals ∷ (Integral i) ⇒ TestData i
cardinals =
  [ ( "neuter"
    , neuter defaultInflection
    , [ (0, "zero")
      , (1, "uno")
      , (2, "due")
      , (3, "tre")
      , (4, "quattro")
      , (5, "cinque")
      , (6, "sei")
      , (7, "sette")
      , (8, "otto")
      , (9, "nove")
      , (10, "dieci")
      , (11, "undici")
      , (12, "dodici")
      , (13, "tredici")
      , (14, "quattordici")
      , (15, "quindici")
      , (16, "sedici")
      , (17, "diciassette")
      , (18, "diciotto")
      , (19, "diciannove")
      , (20, "venti")
      , (21, "ventuno")
      , (22, "ventidue")
      , (23, "ventitré")
      , (24, "ventiquattro")
      , (25, "venticinque")
      , (26, "ventisei")
      , (27, "ventisette")
      , (28, "ventotto")
      , (29, "ventinove")
      , (30, "trenta")
      , (31, "trentuno")
      , (32, "trentadue")
      , (33, "trentatré")
      , (34, "trentaquattro")
      , (35, "trentacinque")
      , (36, "trentasei")
      , (37, "trentasette")
      , (38, "trentotto")
      , (39, "trentanove")
      , (40, "quaranta")
      , (41, "quarantuno")
      , (42, "quarantadue")
      , (43, "quarantatré")
      , (44, "quarantaquattro")
      , (45, "quarantacinque")
      , (46, "quarantasei")
      , (47, "quarantasette")
      , (48, "quarantotto")
      , (49, "quarantanove")
      , (50, "cinquanta")
      , (51, "cinquantuno")
      , (52, "cinquantadue")
      , (53, "cinquantatré")
      , (54, "cinquantaquattro")
      , (55, "cinquantacinque")
      , (56, "cinquantasei")
      , (57, "cinquantasette")
      , (58, "cinquantotto")
      , (59, "cinquantanove")
      , (60, "sessanta")
      , (61, "sessantuno")
      , (62, "sessantadue")
      , (63, "sessantatré")
      , (64, "sessantaquattro")
      , (65, "sessantacinque")
      , (66, "sessantasei")
      , (67, "sessantasette")
      , (68, "sessantotto")
      , (69, "sessantanove")
      , (70, "settanta")
      , (71, "settantuno")
      , (72, "settantadue")
      , (73, "settantatré")
      , (74, "settantaquattro")
      , (75, "settantacinque")
      , (76, "settantasei")
      , (77, "settantasette")
      , (78, "settantotto")
      , (79, "settantanove")
      , (80, "ottanta")
      , (81, "ottantuno")
      , (82, "ottantadue")
      , (83, "ottantatré")
      , (84, "ottantaquattro")
      , (85, "ottantacinque")
      , (86, "ottantasei")
      , (87, "ottantasette")
      , (88, "ottantotto")
      , (89, "ottantanove")
      , (90, "novanta")
      , (91, "novantuno")
      , (92, "novantadue")
      , (93, "novantatré")
      , (94, "novantaquattro")
      , (95, "novantacinque")
      , (96, "novantasei")
      , (97, "novantasette")
      , (98, "novantotto")
      , (99, "novantanove")
      , (100, "cento")
      , (108, "centotto")
      , (101, "centouno")
      , (150, "centocinquanta")
      , (180, "centottanta")
      , (188, "centottantotto")
      , (200, "duecento")
      , (208, "duecentotto")
      , (280, "duecentottanta")
      , (288, "duecentottantotto")
      , (300, "trecento")
      , (400, "quattrocento")
      , (456, "quattrocentocinquantasei")
      , (500, "cinquecento")
      , (600, "seicento")
      , (700, "settecento")
      , (800, "ottocento")
      , (900, "novecento")
      , (1000, "mille")
      , (1001, "milleuno")
      , (1002, "milledue")
      , (1100, "millecento")
      , (1200, "milleduecento")
      , (2000, "duemila")
      , (3000, "tremila")
      , (6827, "seimilaottocentoventisette")
      , (10000, "diecimila")
      , (11000, "undicimila")
      , (100000, "centomila")
      , (1 ⋅ dec 6, "milione")
      , (2 ⋅ dec 6, "due milioni")
      , (125 ⋅ dec 5, "dodici milioni cinquecentomila")
      , (43152 ⋅ dec 3, "quarantatré milioni centocinquantaduemila")
      , (132 ⋅ dec 6, "centotrentadue milioni")
      , (28375 ⋅ dec 4, "duecentottantatré milioni settecentocinquantamila")
      , (680 ⋅ dec 6, "seicentottanta milioni")
      , (1 ⋅ dec 9, "miliardo")
      , (2 ⋅ dec 9, "due miliardi")
      , (dec 12, "bilione")
      , (dec 15, "biliardo")
      , (dec 18, "trilione")
      , (dec 21, "triliardo")
      , (dec 24, "quadrilione")
      , (dec 27, "quadriliardo")
      , (dec 30, "quintilione")
      , (dec 33, "quintiliardo")
      , (dec 36, "sestilione")
      , (dec 39, "sestiliardo")
      , (dec 42, "settilione")
      , (dec 45, "settiliardo")
      , (dec 48, "ottilione")
      , (dec 51, "ottiliardo")
      , (dec 54, "nonilione")
      , (dec 57, "noniliardo")
      , (dec 60, "decilione")
      , (dec 63, "deciliardo")
      ]
    )
  , ( "masculine"
    , masculine defaultInflection
    , [ (1, "un")
      ]
    )
  , ( "feminine"
    , feminine defaultInflection
    , [ (1, "una")
      ]
    )
  ]

ordinals ∷ (Integral i) ⇒ TestData i
ordinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "primo")
      , (2, "secondo")
      , (3, "terzo")
      , (4, "quarto")
      , (5, "quinto")
      , (6, "sesto")
      , (7, "settimo")
      , (8, "ottavo")
      , (9, "nono")
      , (10, "decimo")
      , (11, "undicesimo")
      , (12, "dodicesimo")
      , (13, "tredicesimo")
      , (14, "quattordicesimo")
      , (15, "quindicesimo")
      , (16, "sedicesimo")
      , (17, "diciassettesimo")
      , (18, "diciottesimo")
      , (19, "diciannovesimo")
      , (20, "ventesimo")
      , (21, "ventunesimo")
      , (22, "ventiduesimo")
      , (23, "ventitreesimo")
      , (24, "ventiquattresimo")
      , (25, "venticinquesimo")
      , (26, "ventiseiesimo")
      , (27, "ventisettesimo")
      , (28, "ventottesimo")
      , (29, "ventinovesimo")
      , (30, "trentesimo")
      , (31, "trentunesimo")
      , (32, "trentaduesimo")
      , (33, "trentatreesimo")
      , (34, "trentaquattresimo")
      , (35, "trentacinquesimo")
      , (36, "trentaseiesimo")
      , (37, "trentasettesimo")
      , (38, "trentottesimo")
      , (39, "trentanovesimo")
      , (40, "quarantesimo")
      , (43, "quarantatreesimo")
      , (50, "cinquantesimo")
      , (60, "sessantesimo")
      , (70, "settantesimo")
      , (80, "ottantesimo")
      , (89, "ottantanovesimo")
      , (90, "novantesimo")
      , (100, "centesimo")
      , (200, "duecentesimo")
      , (278, "duecentosettantottesimo")
      , (300, "trecentesimo")
      , (400, "quattrocentesimo")
      , (500, "cinquecentesimo")
      , (600, "seicentesimo")
      , (700, "settecentesimo")
      , (800, "ottocentesimo")
      , (900, "novecentesimo")
      , (1000, "millesimo")
      , (2000, "duemillesimo")
      , (dec 6, "milionesimo")
      , (dec 12, "bilionesimo")
      ]
    )
  , ( "feminine"
    , feminine defaultInflection
    , [ (1, "prima")
      , (2, "seconda")
      , (3, "terza")
      , (4, "quarta")
      , (5, "quinta")
      , (6, "sesta")
      ]
    )
  ]
