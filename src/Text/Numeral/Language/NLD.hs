{-|
[@ISO639-1@]        nl

[@ISO639-2B@]       dut

[@ISO639-3@]        nld

[@Native name@]     Nederlands

[@English name@]    Dutch
-}

module Text.Numeral.Language.NLD
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
    , ordinal
    , partitive
    , multiplicative
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import "base" Data.Monoid ( (<>) )
import qualified "containers" Data.Map as M
import           "this" Text.Numeral hiding ( partitive, multiplicative )
import qualified "this" Text.Numeral.BigNum  as BN
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- NLD
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1       = Just "nl"
    , entIso639_2       = ["dut"]
    , entIso639_3       = Just "nld"
    , entNativeNames    = ["Nederlands"]
    , entEnglishName    = Just "Dutch"
    , entCardinal       = Just Conversion
                          { toNumeral   = cardinal
                          , toStructure = struct
                          }
    , entOrdinal        = Just Conversion
                          { toNumeral   = ordinal
                          , toStructure = struct
                          }
    , entPartitive      = Just Conversion
                          { toNumeral   = partitive
                          , toStructure = \(n, d) -> Frac (struct n) (struct d)
                          }
    , entMultiplicative = Just Conversion
                          { toNumeral   = multiplicative
                          , toStructure = struct
                          }
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

ordinal :: (Integral a) => Inflection -> a -> Maybe Text
ordinal inf = ordinalRepr "eerste" inf . struct

partitive :: (Integral a) => Inflection -> (a, a) -> Maybe Text
partitive inf (n, d) = do
  n' <- cardinal inf {iCase = Nothing, iNumber = Just Singular} n
  d' <- ordinalRepr "éénde" inf $ struct d
  pure $ n' <> " " <> d'

multiplicative :: (Integral a) => Inflection -> a -> Maybe Text
multiplicative inf = fmap (<> "maal") . cardinal (noCase $ singular inf)

struct :: (Integral a) => a -> Exp
struct = pos
       $ fix
       $ findRule (   0, lit               )
                [ (  13, add    10      L  )
                , (  20, mul    10      L L)
                , ( 100, step  100   10 R L)
                , (1000, step 1000 1000 R L)
                ]
                  (dec 6 - 1)
         `combine` pelletierScale R L BN.rule

bounds :: (Integral a) => (a, a)
bounds = let x = dec 60000 - 1 in (negate x, x)

genericRepr :: Repr
genericRepr = defaultRepr
               { reprAdd   = Just (⊞)
               , reprMul   = Just $ \_ _ _ -> ""
               , reprNeg   = Just $ \_ _   -> "min "
               }
    where
      (_     ⊞ Lit 10) _ = ""
      (Lit n ⊞ _) _ | n `elem` [2,3]  = "ën"
                    | n < 10     = "en"
                    | otherwise  = ""
      (_     ⊞ _) _             = ""

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render genericRepr
               { reprValue = \inf n -> M.lookup n (syms inf)
               , reprScale = BN.pelletierRepr (\i c -> if doPlural i c then "iljoenen" else "iljoen")
                                              (\i c -> if doPlural i c then "iljarden" else "iljard")
                                              []
               }
    where
      doPlural :: Inflection -> Ctx Exp -> Bool
      doPlural inf ctx = (isPlural inf || isDative inf) && isOutside R ctx

      syms inf =
          M.fromList
          [ (0, const "nul")
          , (1, pluralDative "één" "éénen" "éénen")
          , (2, forms "twee" "twin" "tweeën" "tweeën")
          , (3, forms "drie" "der"  "drieën" "drieën")
          , (4, forms "vier" "veer" "vieren" "vieren")
          , (5, pluralDative "vijf"  "vijven" "vijven")
          , (6, pluralDative "zes"   "zessen" "zessen")
          , (7, pluralDative "zeven" "zevens" "zevenen")
          , (8, \c -> case c of
                       CtxMul _ (Lit 10) _ -> "tach"
                       CtxAdd _ (Lit 10) _ -> "ach"
                       _ | dativeForm c || pluralForm c -> "achten"
                         | otherwise -> "acht"
            )
          , (9, pluralDative "negen" "negens" "negenen")
          , (10, \c -> case c of
                        CtxAdd R _ _
                          | dativeForm c -> "tienen"
                          | pluralForm c -> "tiens"
                        CtxMul R _ _
                          | dativeForm c -> "tigen"
                          | pluralForm c -> "tigs"
                          | otherwise    -> "tig"
                        _ | dativeForm c || pluralForm c -> "tienen"
                          | otherwise    -> "tien"
            )
          , (  11, pluralDative "elf"     "elven"     "elven")
          , (  12, pluralDative "twaalf"  "twaalven"  "twaalven")
          , ( 100, pluralDative "honderd" "honderden" "honderden")
          , (1000, pluralDative "duizend" "duizenden" "duizenden")
          ]
          where
            pluralDative :: s -> s -> s -> Ctx Exp -> s
            pluralDative  o p d ctx
                | pluralForm ctx = p
                | dativeForm ctx = d
                | otherwise      = o

            dativeForm :: Ctx Exp -> Bool
            dativeForm ctx = isDative inf && isOutside R ctx

            pluralForm :: Ctx Exp -> Bool
            pluralForm ctx = isPlural inf && isOutside R ctx

            forms :: s -- ^ Normal form.
                  -> s -- ^ Added to, or multiplied with ten.
                  -> s -- ^ Plural form.
                  -> s -- ^ Dative form.
                  -> Ctx Exp
                  -> s
            forms n t p d ctx =
                case ctx of
                  CtxMul _ (Lit 10) _ -> t
                  CtxAdd _ (Lit 10) _ -> t
                  _ | dativeForm ctx  -> d
                    | pluralForm ctx  -> p
                    | otherwise       -> n

ordinalRepr :: Text -> Inflection -> Exp -> Maybe Text
ordinalRepr one =
    render genericRepr
           { reprValue = \_ n -> M.lookup n syms
           , reprScale = BN.pelletierRepr ( BN.ordQuantityName "iljoen" "iljoenste"
                                                               "iljoen" "iljoenste"
                                          )
                                          ( BN.ordQuantityName "iljard" "iljardste"
                                                               "iljard" "iljardste"
                                          )
                                          []
           }
    where
      syms =
          M.fromList
          [ (0, const "nulde")
          , (1, \c -> case c of
                       CtxEmpty          -> one
                       _ | isOutside R c -> "éénde"
                         | otherwise     -> "één"
            )
          , (2, forms "tweede"  "twee"  "twin")
          , (3, forms "derde"   "drie"  "der")
          , (4, forms "vierde"  "vier"  "veer")
          , (5, forms "vijfde"  "vijf"  "vijf")
          , (6, forms "zesde"   "zes"   "zes")
          , (7, forms "zevende" "zeven" "zeven")
          , (8, \c -> case c of
                       _ | isOutside R c -> "achtste"
                       CtxMul _ (Lit 10) _ -> "tach"
                       CtxAdd _ (Lit _)  _ -> "ach"
                       _                   -> "acht"
            )
          , (9, forms "negende" "negen" "negen")
          , (10, \c -> case c of
                        CtxMul R _ _ | isOutside R c -> "tigste"
                                     | otherwise     -> "tig"
                        _            | isOutside R c -> "tiende"
                                     | otherwise     -> "tien"
            )
          , (11, \c -> if isOutside R c then "elfde"    else "elf")
          , (12, \c -> if isOutside R c then "twaalfde" else "twaalf")
          , (100,  \c -> if isOutside R c then "honderdste" else "honderd")
          , (1000, \c -> if isOutside R c then "duizendste" else "duizend")
          ]

      forms :: s -- ^ Ordinal form.
            -> s -- ^ Cardinal form.
            -> s -- ^ Added to, or multiplied with, ten.
            -> Ctx Exp
            -> s
      forms o c t ctx = case ctx of
                          _ | isOutside R ctx -> o
                          CtxMul _ (Lit 10) _ -> t
                          CtxAdd _ (Lit _)  _ -> t
                          _                   -> c
