{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        -

[@ISO639-3@]        hur

[@Native name@]     Halq̓eméylem, Hul̓q̓umín̓um̓, Hun'qumi'num

[@English name@]    Halkomelem
-}
module Text.Numeral.Language.HUR.TestData (cardinals) where


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
  http://www.languagesandnumbers.com/how-to-count-in-halkomelem/en/hur/
-}

cardinals ∷ (Num i) ⇒ TestData i
cardinals =
  [ ( "default"
    , defaultInflection
    , [ (1, "nuts’a’")
      , (2, "yuse’lu")
      , (3, "lhihw")
      , (4, "xu’athun")
      , (5, "lhq’etsus")
      , (6, "t’xum")
      , (7, "tth’a’kwus")
      , (8, "te’tsus")
      , (9, "toohw")
      , (10, "’apun")
      , (11, "’apun ’i’ kw’ nuts’a’")
      , (12, "’apun ’i’ kw’ yuse’lu")
      , (13, "’apun ’i’ kw’ lhihw")
      , (14, "’apun ’i’ kw’ xu’athun")
      , (15, "’apun ’i’ kw’ lhq’etsus")
      , (16, "’apun ’i’ kw’ t’xum")
      , (17, "’apun ’i’ kw’ tth’a’kwus")
      , (18, "’apun ’i’ kw’ te’tsus")
      , (19, "’apun ’i’ kw’ toohw")
      , (20, "tskw’ush")
      , (21, "tskw’ush ’i’ kw’ nuts’a’")
      , (22, "tskw’ush ’i’ kw’ yuse’lu")
      , (23, "tskw’ush ’i’ kw’ lhihw")
      , (24, "tskw’ush ’i’ kw’ xu’athun")
      , (25, "tskw’ush ’i’ kw’ lhq’etsus")
      , (26, "tskw’ush ’i’ kw’ t’xum")
      , (27, "tskw’ush ’i’ kw’ tth’a’kwus")
      , (28, "tskw’ush ’i’ kw’ te’tsus")
      , (29, "tskw’ush ’i’ kw’ toohw")
      , (30, "lhuhwulhshe’")
      , (31, "lhuhwulhshe’ ’i’ kw’ nuts’a’")
      , (32, "lhuhwulhshe’ ’i’ kw’ yuse’lu")
      , (33, "lhuhwulhshe’ ’i’ kw’ lhihw")
      , (34, "lhuhwulhshe’ ’i’ kw’ xu’athun")
      , (35, "lhuhwulhshe’ ’i’ kw’ lhq’etsus")
      , (36, "lhuhwulhshe’ ’i’ kw’ t’xum")
      , (37, "lhuhwulhshe’ ’i’ kw’ tth’a’kwus")
      , (38, "lhuhwulhshe’ ’i’ kw’ te’tsus")
      , (39, "lhuhwulhshe’ ’i’ kw’ toohw")
      , (40, "xuthunlhshe’")
      , (41, "xuthunlhshe’ ’i’ kw’ nuts’a’")
      , (42, "xuthunlhshe’ ’i’ kw’ yuse’lu")
      , (43, "xuthunlhshe’ ’i’ kw’ lhihw")
      , (44, "xuthunlhshe’ ’i’ kw’ xu’athun")
      , (45, "xuthunlhshe’ ’i’ kw’ lhq’etsus")
      , (46, "xuthunlhshe’ ’i’ kw’ t’xum")
      , (47, "xuthunlhshe’ ’i’ kw’ tth’a’kwus")
      , (48, "xuthunlhshe’ ’i’ kw’ te’tsus")
      , (49, "xuthunlhshe’ ’i’ kw’ toohw")
      , (50, "lhq’utssulhshe’")
      , (51, "lhq’utssulhshe’ ’i’ kw’ nuts’a’")
      , (52, "lhq’utssulhshe’ ’i’ kw’ yuse’lu")
      , (53, "lhq’utssulhshe’ ’i’ kw’ lhihw")
      , (54, "lhq’utssulhshe’ ’i’ kw’ xu’athun")
      , (55, "lhq’utssulhshe’ ’i’ kw’ lhq’etsus")
      , (56, "lhq’utssulhshe’ ’i’ kw’ t’xum")
      , (57, "lhq’utssulhshe’ ’i’ kw’ tth’a’kwus")
      , (58, "lhq’utssulhshe’ ’i’ kw’ te’tsus")
      , (59, "lhq’utssulhshe’ ’i’ kw’ toohw")
      , (60, "t’xumulhshe’")
      , (61, "t’xumulhshe’ ’i’ kw’ nuts’a’")
      , (62, "t’xumulhshe’ ’i’ kw’ yuse’lu")
      , (63, "t’xumulhshe’ ’i’ kw’ lhihw")
      , (64, "t’xumulhshe’ ’i’ kw’ xu’athun")
      , (65, "t’xumulhshe’ ’i’ kw’ lhq’etsus")
      , (66, "t’xumulhshe’ ’i’ kw’ t’xum")
      , (67, "t’xumulhshe’ ’i’ kw’ tth’a’kwus")
      , (68, "t’xumulhshe’ ’i’ kw’ te’tsus")
      , (69, "t’xumulhshe’ ’i’ kw’ toohw")
      , (70, "tth’ukwsulhshe’")
      , (71, "tth’ukwsulhshe’ ’i’ kw’ nuts’a’")
      , (72, "tth’ukwsulhshe’ ’i’ kw’ yuse’lu")
      , (73, "tth’ukwsulhshe’ ’i’ kw’ lhihw")
      , (74, "tth’ukwsulhshe’ ’i’ kw’ xu’athun")
      , (75, "tth’ukwsulhshe’ ’i’ kw’ lhq’etsus")
      , (76, "tth’ukwsulhshe’ ’i’ kw’ t’xum")
      , (77, "tth’ukwsulhshe’ ’i’ kw’ tth’a’kwus")
      , (78, "tth’ukwsulhshe’ ’i’ kw’ te’tsus")
      , (79, "tth’ukwsulhshe’ ’i’ kw’ toohw")
      , (80, "tutssulhshe’")
      , (81, "tutssulhshe’ ’i’ kw’ nuts’a’")
      , (82, "tutssulhshe’ ’i’ kw’ yuse’lu")
      , (83, "tutssulhshe’ ’i’ kw’ lhihw")
      , (84, "tutssulhshe’ ’i’ kw’ xu’athun")
      , (85, "tutssulhshe’ ’i’ kw’ lhq’etsus")
      , (86, "tutssulhshe’ ’i’ kw’ t’xum")
      , (87, "tutssulhshe’ ’i’ kw’ tth’a’kwus")
      , (88, "tutssulhshe’ ’i’ kw’ te’tsus")
      , (89, "tutssulhshe’ ’i’ kw’ toohw")
      , (90, "toohwulhshe’")
      , (91, "toohwulhshe’ ’i’ kw’ nuts’a’")
      , (92, "toohwulhshe’ ’i’ kw’ yuse’lu")
      , (93, "toohwulhshe’ ’i’ kw’ lhihw")
      , (94, "toohwulhshe’ ’i’ kw’ xu’athun")
      , (95, "toohwulhshe’ ’i’ kw’ lhq’etsus")
      , (96, "toohwulhshe’ ’i’ kw’ t’xum")
      , (97, "toohwulhshe’ ’i’ kw’ tth’a’kwus")
      , (98, "toohwulhshe’ ’i’ kw’ te’tsus")
      , (99, "toohwulhshe’ ’i’ kw’ toohw")
      , (100, "nets’uwuts")
      , (101, "nets’uwuts ’i’ kw’ nuts’a’")
      , (102, "nets’uwuts ’i’ kw’ yuse’lu")
      , (103, "nets’uwuts ’i’ kw’ lhihw")
      , (104, "nets’uwuts ’i’ kw’ xu’athun")
      , (105, "nets’uwuts ’i’ kw’ lhq’etsus")
      , (106, "nets’uwuts ’i’ kw’ t’xum")
      , (107, "nets’uwuts ’i’ kw’ tth’a’kwus")
      , (108, "nets’uwuts ’i’ kw’ te’tsus")
      , (109, "nets’uwuts ’i’ kw’ toohw")
      , (110, "nets’uwuts ’i’ kw’ ’apun")
      , (123, "nets’uwuts ’i’ kw’ tskw’ush ’i’ kw’ lhihw")
      ]
    )
  ]
