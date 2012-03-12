{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        tr

[@ISO639-2@]        tur

[@ISO639-3@]        tur

[@Native name@]     Türkçe

[@English name@]    Turkish
-}

module Text.Numeral.Language.TR.TestData (cardinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.String ( IsString )
import "base" Prelude     ( Integral )
import "numerals-base" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals-base" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

{-
Sources:
  http://www.languagesandnumbers.com/how-to-count-in-turkish/en/tur/
  http://www.sf.airnet.ne.jp/~ts/language/number/turkish.html
  http://www.turkishlanguage.co.uk/seasons.htm#article_15
  http://www.turkeytravelplanner.com/details/LanguageGuide/100words_lessons/100Words_10.html
  http://en.wikibooks.org/wiki/Turkish/Numbers
  http://tr.wikipedia.org/wiki/B%C3%BCy%C3%BCk_say%C4%B1lar%C4%B1n_adlar%C4%B1
-}

cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "sıfır")
      , (1, "bir")
      , (2, "iki")
      , (3, "üç")
      , (4, "dört")
      , (5, "beş")
      , (6, "altı")
      , (7, "yedi")
      , (8, "sekiz")
      , (9, "dokuz")
      , (10, "on")
      , (11, "on bir")
      , (12, "on iki")
      , (13, "on üç")
      , (14, "on dört")
      , (15, "on beş")
      , (16, "on altı")
      , (17, "on yedi")
      , (18, "on sekiz")
      , (19, "on dokuz")
      , (20, "yirmi")
      , (21, "yirmi bir")
      , (22, "yirmi iki")
      , (23, "yirmi üç")
      , (24, "yirmi dört")
      , (25, "yirmi beş")
      , (26, "yirmi altı")
      , (27, "yirmi yedi")
      , (28, "yirmi sekiz")
      , (29, "yirmi dokuz")
      , (30, "otuz")
      , (31, "otuz bir")
      , (32, "otuz iki")
      , (33, "otuz üç")
      , (34, "otuz dört")
      , (35, "otuz beş")
      , (36, "otuz altı")
      , (37, "otuz yedi")
      , (38, "otuz sekiz")
      , (39, "otuz dokuz")
      , (40, "kırk")
      , (41, "kırk bir")
      , (42, "kırk iki")
      , (43, "kırk üç")
      , (44, "kırk dört")
      , (45, "kırk beş")
      , (46, "kırk altı")
      , (47, "kırk yedi")
      , (48, "kırk sekiz")
      , (49, "kırk dokuz")
      , (50, "elli")
      , (51, "elli bir")
      , (52, "elli iki")
      , (53, "elli üç")
      , (54, "elli dört")
      , (55, "elli beş")
      , (56, "elli altı")
      , (57, "elli yedi")
      , (58, "elli sekiz")
      , (59, "elli dokuz")
      , (60, "altmış")
      , (61, "altmış bir")
      , (62, "altmış iki")
      , (63, "altmış üç")
      , (64, "altmış dört")
      , (65, "altmış beş")
      , (66, "altmış altı")
      , (67, "altmış yedi")
      , (68, "altmış sekiz")
      , (69, "altmış dokuz")
      , (70, "yetmiş")
      , (71, "yetmiş bir")
      , (72, "yetmiş iki")
      , (73, "yetmiş üç")
      , (74, "yetmiş dört")
      , (75, "yetmiş beş")
      , (76, "yetmiş altı")
      , (77, "yetmiş yedi")
      , (78, "yetmiş sekiz")
      , (79, "yetmiş dokuz")
      , (80, "seksen")
      , (81, "seksen bir")
      , (82, "seksen iki")
      , (83, "seksen üç")
      , (84, "seksen dört")
      , (85, "seksen beş")
      , (86, "seksen altı")
      , (87, "seksen yedi")
      , (88, "seksen sekiz")
      , (89, "seksen dokuz")
      , (90, "doksan")
      , (91, "doksan bir")
      , (92, "doksan iki")
      , (93, "doksan üç")
      , (94, "doksan dört")
      , (95, "doksan beş")
      , (96, "doksan altı")
      , (97, "doksan yedi")
      , (98, "doksan sekiz")
      , (99, "doksan dokuz")
      , (100, "yüz")
      , (200, "iki yüz")
      , (300, "üç yüz")
      , (400, "dört yüz")
      , (500, "beş yüz")
      , (600, "altı yüz")
      , (700, "yedi yüz")
      , (800, "sekiz yüz")
      , (900, "dokuz yüz")
      , (1000, "bin")
      , (2000, "iki bin")
      , (3000, "üç bin")
      , (4000, "dört bin")
      , (5000, "beş bin")
      , (6000, "altı bin")
      , (7000, "yedi bin")
      , (8000, "sekiz bin")
      , (9000, "dokuz bin")
      , (10000, "on bin")
      , (11000, "onbir bin")
      , (12000, "oniki bin")
      , (13000, "onüç bin")
      , (14000, "ondört bin")
      , (15000, "onbeş bin")
      , (16000, "onaltı bin")
      , (17000, "onyedi bin")
      , (18000, "onsekiz bin")
      , (19000, "ondokuz bin")
      , (20000, "yirmi bin")
      , (21000, "yirmi bir bin")
      , (22000, "yirmi iki bin")
      , (23000, "yirmi üç bin")
      , (24000, "yirmi dört bin")
      , (25000, "yirmi beş bin")
      , (26000, "yirmi altı bin")
      , (27000, "yirmi yedi bin")
      , (28000, "yirmi sekiz bin")
      , (30000, "otuz bin")
      , (50000, "elli bin")
      , (100000, "yüz bin")
      , (523758, "beş yüz yirmi üç bin yedi yüz elli sekiz")
      , (dec 6, "bir milyon")
      , (123456789, "yüz yirmi üç milyon dört yüz elli altı bin yedi yüz seksen dokuz")
      , (dec 9, "bir milyar")
      , (dec 12, "bir trilyon")
      , (dec 15, "bir katrilyon")
      , (dec 18, "bir kentilyon")
      , (dec 21, "bir sekstilyon")
      , (dec 24, "bir septilyon")
      , (dec 27, "bir oktilyon")
      , (dec 30, "bir nonilyon")
      , (dec 33, "bir desilyon")
      , (dec 36, "bir andesilyon")
      , (dec 39, "bir dodesilyon")
      , (dec 42, "bir tredesilyon")
      , (dec 45, "bir katordesilyon")
      , (dec 48, "bir kendesilyon")
      , (dec 51, "bir seksdesilyon")
      , (dec 54, "bir septendesilyon")
      , (dec 57, "bir oktodesilyon")
      , (dec 60, "bir novemdesilyon")
      , (dec 63, "bir vigintilyon")
      , (dec 66, "bir anvigintilyon")
      , (dec 69, "bir dovigintilyon")
      , (dec 72, "bir tresvigintilyon")
      , (dec 75, "bir katorvigintilyon")
      , (dec 78, "bir kenkavigintilyon")
      , (dec 81, "bir sesvigintilyon")
      , (dec 84, "bir septemvigintilyon")
      , (dec 87, "bir oktovigintilyon")
      , (dec 90, "bir novemvigintilyon")
      , (dec 93, "bir trigintilyon")
      , (dec 96, "bir antrigintilyon")
      , (dec 99, "bir dotrigintilyon")
      , (dec 102, "bir trestrigintilyon")
      , (dec 105, "bir katortrigintilyon")
      , (dec 108, "bir kenkatrigintilyon")
      , (dec 111, "bir sestrigintilyon")
      , (dec 114, "bir septemtrigintilyon")
      , (dec 117, "bir oktotrigintilyon")
      , (dec 120, "bir novemtrigintilyon")
      , (dec 123, "bir katragintilyon")
      , (dec 153, "bir kenkagintilyon")
      , (dec 183, "bir seksagintilyon")
      , (dec 213, "bir septegintilyon")
      , (dec 243, "bir oktogintilyon")
      , (dec 273, "bir nonagintilyon")
      , (dec 303, "bir sentilyon")
      , (dec 306, "bir ansentilyon")
      , (dec 309, "bir dosentilyon")
      , (dec 312, "bir tresentilyon")
      , (dec 333, "bir desisentilyon")
      , (dec 336, "bir andesisentilyon")
      , (dec 363, "bir vigintisentilyon")
      , (dec 366, "bir anvigintisentilyon")
      , (dec 393, "bir trigintasentilyon")
      , (dec 423, "bir katragintasentilyon")
      , (dec 453, "bir kenkagintasentilyon")
      , (dec 483, "bir seksagintasentilyon")
      , (dec 513, "bir septegintasentilyon")
      , (dec 543, "bir oktogintasentilyon")
      , (dec 573, "bir nonagintasentilyon")
      , (dec 603, "bir dusentilyon")
      , (dec 903, "bir tresentilyon")
      , (dec 1203, "bir katringentilyon")
      , (dec 1503, "bir kengentilyon")
      , (dec 1803, "bir sesentilyon")
      , (dec 2103, "bir septingentilyon")
      , (dec 2403, "bir oktingentilyon")
      , (dec 2703, "bir nongentilyon")
      ]
    )
  ]
