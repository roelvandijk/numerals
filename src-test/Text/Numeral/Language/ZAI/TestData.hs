{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        zai

[@Native name@]     diidxazá

[@English name@]    Isthmus Zapotec
-}
module Text.Numeral.Language.ZAI.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-isthmus-zapotec/en/zai/
-}

cardinals :: (Num i) => TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "tobi")
      , (2, "chupa")
      , (3, "chonna")
      , (4, "tapa")
      , (5, "gaayu’")
      , (6, "xhoopa’")
      , (7, "gadxe")
      , (8, "xhono")
      , (9, "ga’")
      , (10, "chii")
      , (11, "chii ne tobi")
      , (12, "chii ne chupa")
      , (13, "chii ne chonna")
      , (14, "chii ne tapa")
      , (15, "chii ne gaayu’")
      , (16, "chii ne xhoopa’")
      , (17, "chii ne gadxe")
      , (18, "chii ne xhono")
      , (19, "chii ne ga’")
      , (20, "gande")
      , (21, "gande ne tobi")
      , (22, "gande ne chupa")
      , (23, "gande ne chonna")
      , (24, "gande ne tapa")
      , (25, "gande ne gaayu’")
      , (26, "gande ne xhoopa’")
      , (27, "gande ne gadxe")
      , (28, "gande ne xhono")
      , (29, "gande ne ga’")
      , (30, "gande chii")
      , (31, "gande chii ne tobi")
      , (32, "gande chii ne chupa")
      , (33, "gande chii ne chonna")
      , (34, "gande chii ne tapa")
      , (35, "gande chii ne gaayu’")
      , (36, "gande chii ne xhoopa’")
      , (37, "gande chii ne gadxe")
      , (38, "gande chii ne xhono")
      , (39, "gande chii ne ga’")
      , (40, "chupa late gande")
      , (41, "chupa late gande ne tobi")
      , (42, "chupa late gande ne chupa")
      , (43, "chupa late gande ne chonna")
      , (44, "chupa late gande ne tapa")
      , (45, "chupa late gande ne gaayu’")
      , (46, "chupa late gande ne xhoopa’")
      , (47, "chupa late gande ne gadxe")
      , (48, "chupa late gande ne xhono")
      , (49, "chupa late gande ne ga’")
      , (50, "chupa late gande chii")
      , (51, "chupa late gande chii ne tobi")
      , (52, "chupa late gande chii ne chupa")
      , (53, "chupa late gande chii ne chonna")
      , (54, "chupa late gande chii ne tapa")
      , (55, "chupa late gande chii ne gaayu’")
      , (56, "chupa late gande chii ne xhoopa’")
      , (57, "chupa late gande chii ne gadxe")
      , (58, "chupa late gande chii ne xhono")
      , (59, "chupa late gande chii ne ga’")
      , (60, "chonna late gande")
      , (61, "chonna late gande ne tobi")
      , (62, "chonna late gande ne chupa")
      , (63, "chonna late gande ne chonna")
      , (64, "chonna late gande ne tapa")
      , (65, "chonna late gande ne gaayu’")
      , (66, "chonna late gande ne xhoopa’")
      , (67, "chonna late gande ne gadxe")
      , (68, "chonna late gande ne xhono")
      , (69, "chonna late gande ne ga’")
      , (70, "chonna late gande chii")
      , (71, "chonna late gande chii ne tobi")
      , (72, "chonna late gande chii ne chupa")
      , (73, "chonna late gande chii ne chonna")
      , (74, "chonna late gande chii ne tapa")
      , (75, "chonna late gande chii ne gaayu’")
      , (76, "chonna late gande chii ne xhoopa’")
      , (77, "chonna late gande chii ne gadxe")
      , (78, "chonna late gande chii ne xhono")
      , (79, "chonna late gande chii ne ga’")
      , (80, "tapa late gande")
      , (81, "tapa late gande ne tobi")
      , (82, "tapa late gande ne chupa")
      , (83, "tapa late gande ne chonna")
      , (84, "tapa late gande ne tapa")
      , (85, "tapa late gande ne gaayu’")
      , (86, "tapa late gande ne xhoopa’")
      , (87, "tapa late gande ne gadxe")
      , (88, "tapa late gande ne xhono")
      , (89, "tapa late gande ne ga’")
      , (90, "tapa late gande chii")
      , (91, "tapa late gande chii ne tobi")
      , (92, "tapa late gande chii ne chupa")
      , (93, "tapa late gande chii ne chonna")
      , (94, "tapa late gande chii ne tapa")
      , (95, "tapa late gande chii ne gaayu’")
      , (96, "tapa late gande chii ne xhoopa’")
      , (97, "tapa late gande chii ne gadxe")
      , (98, "tapa late gande chii ne xhono")
      , (99, "tapa late gande chii ne ga’")
      , (100, "ti gayuaa")
      , (101, "ti gayuaa tobi")
      , (102, "ti gayuaa chupa")
      , (103, "ti gayuaa chonna")
      , (104, "ti gayuaa tapa")
      , (105, "ti gayuaa gaayu’")
      , (106, "ti gayuaa xhoopa’")
      , (107, "ti gayuaa gadxe")
      , (108, "ti gayuaa xhono")
      , (109, "ti gayuaa ga’")
      , (110, "ti gayuaa chii")
      , (123, "ti gayuaa gande ne chonna")
      , (200, "chupa gayuaa")
      , (300, "chonna gayuaa")
      , (321, "chonna gayuaa gande ne tobi")
      , (400, "tapa gayuaa")
      , (500, "gaayu’ gayuaa")
      , (600, "xhoopa’ gayuaa")
      , (700, "gadxe gayuaa")
      , (800, "xhono gayuaa")
      , (900, "ga’ gayuaa")
      , (909, "ga’ gayuaa ga’")
      , (990, "ga’ gayuaa tapa late gande chii")
      , (999, "ga’ gayuaa tapa late gande chii ne ga’")
      , (1000, "ti mil")
      , (1001, "ti mil ne tobi")
      , (1008, "ti mil ne xhono")
      , (1234, "ti mil ne chupa gayuaa gande chii ne tapa")
      , (2000, "chupa mil")
      , (3000, "chonna mil")
      , (4000, "tapa mil")
      , (4321, "tapa mil ne chonna gayuaa gande ne tobi")
      , (5000, "gaayu’ mil")
      , (6000, "xhoopa’ mil")
      , (7000, "gadxe mil")
      , (8000, "xhono mil")
      , (9000, "ga’ mil")
      ]
    )
  ]
