{-|
[@ISO639-1@]        en

[@ISO639-2B@]       eng

[@ISO639-3@]        eng

[@Native name@]     English

[@English name@]    English
-}

module Text.Numeral.Language.ENG
    ( -- * Language entries
      gb_entry
    , us_entry
      -- * Conversions
    , gb_cardinal
    , gb_ordinal
    , gbPelletier_cardinal
    , us_cardinal
    , us_ordinal
      -- * Structure
    , shortScaleStruct
    , pelletierScaleStruct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum as BN
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- ENG
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "en"
    , entIso639_2    = ["eng"]
    , entIso639_3    = Just "eng"
    , entNativeNames = ["English"]
    , entEnglishName = Just "English"
    }

gb_entry :: Entry
gb_entry = entry
    { entVariant  = Just "en-GB"
    , entCardinal = Just Conversion
                    { toNumeral   = gb_cardinal
                    , toStructure = shortScaleStruct
                    }
    , entOrdinal  = Just Conversion
                    { toNumeral   = gb_ordinal
                    , toStructure = shortScaleStruct
                    }
    }

us_entry :: Entry
us_entry = entry
    { entVariant  = Just "en-US"
    , entCardinal = Just Conversion
                    { toNumeral   = us_cardinal
                    , toStructure = shortScaleStruct
                    }
    , entOrdinal  = Just Conversion
                    { toNumeral   = us_ordinal
                    , toStructure = shortScaleStruct
                    }
    }

gb_cardinal :: (Integral a) => Inflection -> a -> Maybe Text
gb_cardinal inf = render (cardinalRepr "minus " gb_add) inf . shortScaleStruct

gb_ordinal :: (Integral a) => Inflection -> a -> Maybe Text
gb_ordinal inf = render (ordinalRepr gb_add) inf . shortScaleStruct

gbPelletier_cardinal :: (Integral a) => Inflection -> a -> Maybe Text
gbPelletier_cardinal inf = render (cardinalRepr "minus " gb_add) { reprScale = pelletierRepr } inf
                         . pelletierScaleStruct
  where
    pelletierRepr = BN.pelletierRepr (\_ _ -> "illion")
                                     (\_ _ -> "illiard")
                                     []

us_cardinal :: (Integral a) => Inflection -> a -> Maybe Text
us_cardinal inf = render (cardinalRepr "negative " us_add) inf . shortScaleStruct

us_ordinal :: (Integral a) => Inflection -> a -> Maybe Text
us_ordinal inf = render (ordinalRepr us_add) inf . shortScaleStruct

shortScaleStruct :: (Integral a) => a -> Exp
shortScaleStruct = pos $ fix $ rule `combine` shortScale1 R L BN.rule

pelletierScaleStruct :: (Integral a) => a -> Exp
pelletierScaleStruct = pos $ fix $ rule `combine` pelletierScale1 R L BN.rule

rule :: (Integral a) => Rule a
rule = findRule (   0, lit       )
              [ (  13, add 10 L  )
              , (  20, mul 10 R L)
              , ( 100, step1  100   10 R L)
              , (1000, step1 1000 1000 R L)
              ]
                (dec 6 - 1)

bounds :: (Integral a) => (a, a)
bounds = let x = dec 30003 - 1 in (negate x, x)

genericRepr :: (Exp -> Exp -> Ctx Exp -> Text) -- ^ Add representation.
            -> Repr
genericRepr f =
    defaultRepr
    { reprAdd   = Just f
    , reprMul   = Just (⊞)
    }
    where
      (_ ⊞ Lit 10) _ = ""
      (_ ⊞ _     ) _ = " "

gb_add :: Exp -> Exp -> Ctx Exp -> Text
((_ `Mul` Lit 10) `gb_add` _) _ = "-"
((_ `Mul` _     ) `gb_add` _x) _
    --  | eval x < (100 :: Integer) = " and "
    | otherwise                 = " "
(_                `gb_add` _) _ = ""

us_add :: Exp -> Exp -> Ctx Exp -> Text
((_ `Mul` Lit 10) `us_add` _) _ = "-"
((_ `Mul` _     ) `us_add` _) _ = " "
(_                `us_add` _) _ = ""

cardinalRepr :: Text -- ^ Negative number prefix.
             -> (Exp -> Exp -> Ctx Exp -> Text)
             -> Repr
cardinalRepr neg f =
    (genericRepr f)
    { reprValue = \_ n -> M.lookup n syms
    , reprScale = BN.scaleRepr (\_ _ -> "illion") []
    , reprNeg   = Just $ \_ _ -> neg
    }
  where
    syms =
        M.fromList
        [ (0, const "zero")
        , (1, const "one")
        , (2, ten   "two"   "two"  "twen")
        , (3, ten   "three" "thir" "thir")
        , (4, ten   "four"  "four" "for")
        , (5, ten   "five"  "fif"  "fif")
        , (6, const "six")
        , (7, const "seven")
        , (8, ten   "eight" "eigh" "eigh")
        , (9, const "nine")
        , (10, \c -> case c of
                      CtxAdd _ (Lit _) _ -> "teen"
                      CtxMul R _       _ -> "ty"
                      _                  -> "ten"
          )
        , (11,   const "eleven")
        , (12,   const "twelve")
        , (100,  const "hundred")
        , (1000, const "thousand")
        ]

    ten :: s -> s -> s -> Ctx Exp -> s
    ten n a m = \c -> case c of
                       CtxAdd _ (Lit 10) _ -> a
                       CtxMul _ (Lit 10) _ -> m
                       _                   -> n

ordinalRepr :: (Exp -> Exp -> Ctx Exp -> Text) -> Repr
ordinalRepr f = (genericRepr f)
                { reprValue = \_ n -> M.lookup n syms
                , reprScale = BN.scaleRepr (BN.ordQuantityName "illion" "illionth"
                                                               "illion" "illionth"
                                           )
                                           []
                }
    where
      syms =
          M.fromList
          [ (0, const "zeroth")
          , (1, \c -> case c of
                       _ | isOutside R c -> "first"
                         | otherwise     -> "one"
            )
          , (2, ten   "second" "two"   "two"  "twen")
          , (3, ten   "third"  "three" "thir" "thir")
          , (4, ten   "fourth" "four"  "four" "for")
          , (5, ten   "fifth"  "five"  "fif"  "fif")
          , (6, \c -> if isOutside R c then "sixth"   else "six")
          , (7, \c -> if isOutside R c then "seventh" else "seven")
          , (8, ten   "eighth" "eight" "eigh" "eigh")
          , (9, \c -> if isOutside R c then "ninth"   else "nine")
          , (10, \c -> case c of
                        CtxAdd _ (Lit _) _ | isOutside R c -> "teenth"
                                           | otherwise     -> "teen"
                        CtxMul R _       _ | isOutside R c -> "tieth"
                                           | otherwise     -> "ty"
                        _                  | isOutside R c -> "tenth"
                                           | otherwise     -> "ten"
            )
          , (11,   \c -> if isOutside R c then "eleventh"   else "eleven")
          , (12,   \c -> if isOutside R c then "twelfth"    else "twelf")
          , (100,  \c -> if isOutside R c then "hundreth"   else "hundred")
          , (1000, \c -> if isOutside R c then "thousandth" else "thousand")
          ]

      ten :: s -- ^ Ordinal form.
          -> s -- ^ Cardinal form; normal.
          -> s -- ^ Cardinal form; added to ten.
          -> s -- ^ Cardinal form; multiplied with ten.
          -> Ctx Exp
          -> s
      ten o n a m ctx =
          case ctx of
            _ | isOutside R ctx -> o
            CtxAdd _ (Lit 10) _ -> a
            CtxMul _ (Lit 10) _ -> m
            _                   -> n
