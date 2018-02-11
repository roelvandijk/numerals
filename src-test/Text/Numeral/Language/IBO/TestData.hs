{-|
[@ISO639-1@]        ig

[@ISO639-2@]        ibo

[@ISO639-3@]        ibo

[@Native name@]     Asụsụ Igbo

[@English name@]    Igbo
-}
module Text.Numeral.Language.IBO.TestData
    ( cardinals
    , ordinals
    , multiplicatives
    ) where


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
  http://www.languagesandnumbers.com/how-to-count-in-igbo/en/ibo/
    Note (RvD): appears to be incorrect for numbers >= 10000
    10,000 = "puku iri" (1000 * 10) not "iri puku" (10 * 1000).
  http://www.sf.airnet.ne.jp/~ts/language/number/igbo.html
  http://www.teachyourselfigbo.com/counting-of-numbers.php
  https://ezinaulo.com/igbo-lessons/vocabulary/numbers/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (0, "adigi")
      , (1, "otu")
      , (2, "abụọ")
      , (3, "atọ")
      , (4, "anọ")
      , (5, "ise")
      , (6, "isii")
      , (7, "asaa")
      , (8, "asato")
      , (9, "itoolu")
      , (10, "iri")
      , (11, "iri na otu")
      , (12, "iri na abụọ")
      , (13, "iri na atọ")
      , (14, "iri na anọ")
      , (15, "iri na ise")
      , (16, "iri na isii")
      , (17, "iri na asaa")
      , (18, "iri na asato")
      , (19, "iri na itoolu")
      , (20, "iri abụọ")
      , (21, "iri abụọ na otu")
      , (22, "iri abụọ na abụọ")
      , (23, "iri abụọ na atọ")
      , (24, "iri abụọ na anọ")
      , (25, "iri abụọ na ise")
      , (26, "iri abụọ na isii")
      , (27, "iri abụọ na asaa")
      , (28, "iri abụọ na asato")
      , (29, "iri abụọ na itoolu")
      , (30, "iri atọ")
      , (31, "iri atọ na otu")
      , (32, "iri atọ na abụọ")
      , (33, "iri atọ na atọ")
      , (34, "iri atọ na anọ")
      , (35, "iri atọ na ise")
      , (36, "iri atọ na isii")
      , (37, "iri atọ na asaa")
      , (38, "iri atọ na asato")
      , (39, "iri atọ na itoolu")
      , (40, "iri anọ")
      , (41, "iri anọ na otu")
      , (42, "iri anọ na abụọ")
      , (43, "iri anọ na atọ")
      , (44, "iri anọ na anọ")
      , (45, "iri anọ na ise")
      , (46, "iri anọ na isii")
      , (47, "iri anọ na asaa")
      , (48, "iri anọ na asato")
      , (49, "iri anọ na itoolu")
      , (50, "iri ise")
      , (51, "iri ise na otu")
      , (52, "iri ise na abụọ")
      , (53, "iri ise na atọ")
      , (54, "iri ise na anọ")
      , (55, "iri ise na ise")
      , (56, "iri ise na isii")
      , (57, "iri ise na asaa")
      , (58, "iri ise na asato")
      , (59, "iri ise na itoolu")
      , (60, "iri isii")
      , (61, "iri isii na otu")
      , (62, "iri isii na abụọ")
      , (63, "iri isii na atọ")
      , (64, "iri isii na anọ")
      , (65, "iri isii na ise")
      , (66, "iri isii na isii")
      , (67, "iri isii na asaa")
      , (68, "iri isii na asato")
      , (69, "iri isii na itoolu")
      , (70, "iri asaa")
      , (71, "iri asaa na otu")
      , (72, "iri asaa na abụọ")
      , (73, "iri asaa na atọ")
      , (74, "iri asaa na anọ")
      , (75, "iri asaa na ise")
      , (76, "iri asaa na isii")
      , (77, "iri asaa na asaa")
      , (78, "iri asaa na asato")
      , (79, "iri asaa na itoolu")
      , (80, "iri asato")
      , (81, "iri asato na otu")
      , (82, "iri asato na abụọ")
      , (83, "iri asato na atọ")
      , (84, "iri asato na anọ")
      , (85, "iri asato na ise")
      , (86, "iri asato na isii")
      , (87, "iri asato na asaa")
      , (88, "iri asato na asato")
      , (89, "iri asato na itoolu")
      , (90, "iri itoolu")
      , (91, "iri itoolu na otu")
      , (92, "iri itoolu na abụọ")
      , (93, "iri itoolu na atọ")
      , (94, "iri itoolu na anọ")
      , (95, "iri itoolu na ise")
      , (96, "iri itoolu na isii")
      , (97, "iri itoolu na asaa")
      , (98, "iri itoolu na asato")
      , (99, "iri itoolu na itoolu")
      , (100, "nnari")
      , (101, "nnari na otu")
      , (102, "nnari na abụọ")
      , (103, "nnari na atọ")
      , (104, "nnari na anọ")
      , (105, "nnari na ise")
      , (106, "nnari na isii")
      , (107, "nnari na asaa")
      , (108, "nnari na asato")
      , (109, "nnari na itoolu")
      , (110, "nnari na iri")
      , (123, "nnari na iri abụọ na atọ")
      , (150, "nnari na iri ise")
      , (200, "nnari abụọ")
      , (300, "nnari atọ")
      , (321, "nnari atọ na iri abụọ na otu")
      , (400, "nnari anọ")
      , (500, "nnari ise")
      , (600, "nnari isii")
      , (700, "nnari asaa")
      , (750, "nnari asaa na iri ise")
      , (800, "nnari asato")
      , (900, "nnari itoolu")
      , (909, "nnari itoolu na itoolu")
      , (990, "nnari itoolu na iri itoolu")
      , (999, "nnari itoolu na iri itoolu na itoolu")
      , (1000, "puku")
      , (1001, "puku na otu")
      , (1008, "puku na asato")
      , (1234, "puku na nnari abụọ na iri atọ na anọ")
      , (2000, "puku abụọ")
      , (3000, "puku atọ")
      , (3452, "puku atọ na nnari anọ na iri ise na abụọ")
      , (4000, "puku anọ")
      , (4321, "puku anọ na nnari atọ na iri abụọ na otu")
      , (5000, "puku ise")
      , (6000, "puku isii")
      , (7000, "puku asaa")
      , (8000, "puku asato")
      , (9000, "puku itoolu")
      , (10000, "puku iri")
      , (10150, "puku iri na nnari na iri ise")
      , (20000, "puku iri abụọ")
      , (30000, "puku iri atọ")
      , (40000, "puku iri anọ")
      , (50000, "puku iri ise")
      , (60000, "puku iri isii")
      , (70000, "puku iri asaa")
      , (80000, "puku iri asato")
      , (90000, "puku iri itoolu")
      , (100000, "puku nnari")
      , (200000, "puku nnari abụọ")
      , (300000, "puku nnari atọ")
      , (400000, "puku nnari anọ")
      , (500000, "puku nnari ise")
      , (600000, "puku nnari isii")
      , (700000, "puku nnari asaa")
      , (800000, "puku nnari asato")
      , (900000, "puku nnari itoolu")
      , (1000000, "nde")
      , (1000001, "nde na otu")
      , (2000000, "nde abụọ")
      , (3000000, "nde atọ")
      , (4000000, "nde anọ")
      , (5000000, "nde ise")
      , (6000000, "nde isii")
      , (7000000, "nde asaa")
      , (8000000, "nde asato")
      , (9000000, "nde itoolu")
      , (1000000000, "ijeri")
      , (1000000001, "ijeri na otu")
      , (2000000000, "ijeri abụọ")
      , (3000000000, "ijeri atọ")
      , (4000000000, "ijeri anọ")
      , (5000000000, "ijeri ise")
      , (6000000000, "ijeri isii")
      , (7000000000, "ijeri asaa")
      , (8000000000, "ijeri asato")
      , (9000000000, "ijeri itoolu")
      ]
    )
  ]

ordinals :: (Num i) => TestData i
ordinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "nke mbụ")
      , (2, "nke abụọ")
      , (3, "nke atọ")
      , (4, "nke anọ")
      , (5, "nke ise")
      , (6, "nke isii")
      , (7, "nke asaa")
      , (8, "nke asato")
      , (9, "nke itoolu")
      , (10, "nke iri")
      , (13, "nke iri na atọ")
      , (30, "nke iri atọ")
      , (300, "nke nnari atọ")
      , (3000, "nke puku atọ")
      ]
    )
  ]

multiplicatives :: (Integral i) => TestData i
multiplicatives =
  [ ( "default"
    , defaultInflection
    , [ (1, "otu ugboro")
      , (2, "ugboro abụọ")
      , (3, "ugboro atọ")
      ]
    )
  ]
