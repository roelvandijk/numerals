{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        pt

[@ISO639-2@]        por

[@ISO639-3@]        por

[@Native name@]     Português

[@English name@]    Portuguese
-}

module Text.Numeral.Language.PT.TestData (cardinals, ordinals) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Arrow ( second )
import "base" Data.List     ( map )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Monoid.Unicode   ( (⊕) )
import "base-unicode-symbols" Prelude.Unicode       ( (⋅) )
import "numerals-base" Text.Numeral.Grammar
import "numerals-base" Text.Numeral.Grammar.Reified ( defaultInflection )
import "numerals-base" Text.Numeral.Misc ( dec )
import "this"          Text.Numeral.Test ( TestData )


--------------------------------------------------------------------------------
-- Test data
--------------------------------------------------------------------------------

-- Sources:
--   http://www.sonia-portuguese.com/text/numerals.htm
--   http://www.smartphrase.com/Portuguese/po_numbers_voc.shtml

base_cardinals ∷ (Integral i, IsString s) ⇒ [(i, s)]
base_cardinals =
  [ (0, "zero")
  , (3, "três")
  , (4, "quatro")
  , (5, "cinco")
  , (6, "seis")
  , (7, "sete")
  , (8, "oito")
  , (9, "nove")
  , (10, "dez")
  , (11, "onze")
  , (12, "doze")
  , (13, "treze")
  , (14, "catorze")
  , (15, "quinze")
  , (16, "dezasseis")
  , (17, "dezassete")
  , (18, "dezoito")
  , (19, "dezanove")
  , (20, "vinte")
  , (23, "vinte e três")
  , (24, "vinte e quatro")
  , (25, "vinte e cinco")
  , (26, "vinte e seis")
  , (27, "vinte e sete")
  , (28, "vinte e oito")
  , (29, "vinte e nove")
  , (30, "trinta")
  , (33, "trinta e três")
  , (34, "trinta e quatro")
  , (35, "trinta e cinco")
  , (36, "trinta e seis")
  , (37, "trinta e sete")
  , (38, "trinta e oito")
  , (39, "trinta e nove")
  , (40, "quarenta")
  , (43, "quarenta e três")
  , (44, "quarenta e quatro")
  , (45, "quarenta e cinco")
  , (46, "quarenta e seis")
  , (47, "quarenta e sete")
  , (48, "quarenta e oito")
  , (49, "quarenta e nove")
  , (50, "cinquenta")
  , (53, "cinquenta e três")
  , (54, "cinquenta e quatro")
  , (55, "cinquenta e cinco")
  , (56, "cinquenta e seis")
  , (57, "cinquenta e sete")
  , (58, "cinquenta e oito")
  , (59, "cinquenta e nove")
  , (60, "sessenta")
  , (63, "sessenta e três")
  , (64, "sessenta e quatro")
  , (65, "sessenta e cinco")
  , (66, "sessenta e seis")
  , (67, "sessenta e sete")
  , (68, "sessenta e oito")
  , (69, "sessenta e nove")
  , (70, "setenta")
  , (73, "setenta e três")
  , (74, "setenta e quatro")
  , (75, "setenta e cinco")
  , (76, "setenta e seis")
  , (77, "setenta e sete")
  , (78, "setenta e oito")
  , (79, "setenta e nove")
  , (80, "oitenta")
  , (83, "oitenta e três")
  , (84, "oitenta e quatro")
  , (85, "oitenta e cinco")
  , (86, "oitenta e seis")
  , (87, "oitenta e sete")
  , (88, "oitenta e oito")
  , (89, "oitenta e nove")
  , (90, "noventa")
  , (93, "noventa e três")
  , (94, "noventa e quatro")
  , (95, "noventa e cinco")
  , (96, "noventa e seis")
  , (97, "noventa e sete")
  , (98, "noventa e oito")
  , (99, "noventa e nove")
  , (100, "cem")
  , (105, "cento e cinco")
  , (125, "cento e vinte e cinco")
  , (138, "cento e trinta e oito")
  , (199, "cento e noventa e nove")
  , (1000, "mil")
  , (1008, "mil e oito")
  , (1985, "mil novecentos e oitenta e cinco")
  , (3000, "três mil")
  , (10000, "dez mil")
  , (100000, "cem mil")
  , (125000, "cento e vinte e cinco mil")
  , (735346, "setecentos e trinta e cinco mil trezentos e quarenta e seis")
  , (dec 6, "um milhão")
  , (2 ⋅ dec 6, "dois milhões")
  , (dec 7, "dez milhões")
  , (dec 9, "um bilhão")
  , (dec 10, "dez bilhões")
  , (dec 12, "um trilhão")
  , (dec 13, "dez trilhões")
  , (dec 15, "um quatrilhão")
  , (dec 18, "um quintilhão")
  , (dec 21, "um sextilhão")
  , (dec 24, "um septilhão")
  , (dec 27, "um octilhão")
  , (dec 30, "um nonilhão")
  , (dec 33, "um decilhão")
  , (dec 36, "um undecilhão")
  , (dec 39, "um duodecilhão")
  , (dec 42, "um tredecilhão")
  , (dec 100, "dez duotrigintilhões")
  ]

cardinals ∷ (Integral i, IsString s) ⇒ TestData i s
cardinals =
  [ ( "masculine"
    , masculine defaultInflection
    , base_cardinals
      ⊕ [ (1, "um")
        , (2, "dois")
        , (21, "vinte e um")
        , (22, "vinte e dois")
        , (31, "trinta e um")
        , (32, "trinta e dois")
        , (41, "quarenta e um")
        , (42, "quarenta e dois")
        , (51, "cinquenta e um")
        , (52, "cinquenta e dois")
        , (61, "sessenta e um")
        , (62, "sessenta e dois")
        , (71, "setenta e um")
        , (72, "setenta e dois")
        , (81, "oitenta e um")
        , (82, "oitenta e dois")
        , (91, "noventa e um")
        , (92, "noventa e dois")
        , (101, "cento e um")
        , (200, "duzentos")
        , (234, "duzentos e trinta e quatro")
        , (250, "duzentos e cinquenta")
        , (300, "trezentos")
        , (330, "trezentos e trinta")
        , (375, "trezentos e setenta e cinco")
        , (400, "quatrocentos")
        , (467, "quatrocentos e sessenta e sete")
        , (500, "quinhentos")
        , (600, "seiscentos")
        , (700, "setecentos")
        , (800, "oitocentos")
        , (900, "novecentos")
        , (2000, "dois mil")
        , (200000, "duzentos mil")
        , (500000, "quinhentos mil")
        , (100001, "cem mil e um")
        , (101000, "cento e um mil")
        , (1537469, "um milhão quinhentos e trinta e sete mil quatrocentos e sessenta e nove")
        ]
    )
  , ( "feminine"
    , feminine defaultInflection
    , base_cardinals
      ⊕ [ (1, "uma")
        , (2, "duas")
        , (21, "vinte e uma")
        , (22, "vinte e duas")
        , (31, "trinta e uma")
        , (32, "trinta e duas")
        , (41, "quarenta e uma")
        , (42, "quarenta e duas")
        , (51, "cinquenta e uma")
        , (52, "cinquenta e duas")
        , (61, "sessenta e uma")
        , (62, "sessenta e duas")
        , (71, "setenta e uma")
        , (72, "setenta e duas")
        , (81, "oitenta e uma")
        , (82, "oitenta e duas")
        , (91, "noventa e uma")
        , (92, "noventa e duas")
        , (101, "cento e uma")
        , (200, "duzentas")
        , (234, "duzentas e trinta e quatro")
        , (250, "duzentas e cinquenta")
        , (300, "trezentas")
        , (330, "trezentas e trinta")
        , (375, "trezentas e setenta e cinco")
        , (400, "quatrocentas")
        , (467, "quatrocentas e sessenta e sete")
        , (500, "quinhentas")
        , (600, "seiscentas")
        , (700, "setecentas")
        , (800, "oitocentas")
        , (900, "novecentas")
        , (2000, "duas mil")
        , (200000, "duzentas mil")
        , (500000, "quinhentas mil")
        , (100001, "cem mil e uma")
        , (101000, "cento e uma mil")
        , (1537469, "um milhão quinhentas e trinta e sete mil quatrocentas e sessenta e nove")
        ]
    )
  ]

-- These are the base forms of the ordinals, stripped of their
-- ending. Append "o", "os", "a" or "as" to form combinations of
-- masculine, feminine, singular and plural ordinals.
base_ordinals ∷ (Integral i, IsString s) ⇒ [(i, s)]
base_ordinals =
  [ (1, "primeir")
  , (2, "segund")
  , (3, "terceir")
  , (4, "quart")
  , (5, "quint")
  , (6, "sext")
  , (7, "sétim")
  , (8, "oitav")
  , (9, "non")
  , (10, "décim")
  , (11, "décimo primeir")
  , (12, "décimo segund")
  , (13, "décimo terceir")
  , (20, "vigésim")
  , (21, "vigésimo primeir")
  , (30, "trigésim")
  , (40, "quadragésim")
  , (50, "qüinquagésim")
  , (60, "sexagésim")
  , (70, "septuagésim")
  , (80, "octogésim")
  , (90, "nonagésim")
  , (100, "centésim")
  , (200, "ducentésim")
  , (300, "trecentésim")
  , (400, "quadringentésim")
  , (500, "qüingentésim")
  , (600, "sexcentésim")
  , (700, "setingentésim")
  , (800, "octingentésim")
  , (900, "nongentésim")
  , (1000, "milésim")
  ]

ordinals ∷ (Integral i, IsString s, Monoid s) ⇒ TestData i s
ordinals = map (\(n, f, e) → ( n
                             , f defaultInflection
                             , map (second (⊕ e)) base_ordinals)
                             )
               [ ("masculine singular", masculine ∘ singular, "o")
               , ("masculine plural",   masculine ∘ plural,   "os")
               , ("feminine singular",  feminine  ∘ singular, "a")
               , ("feminine plural",    feminine  ∘ plural,   "as")
               ]
