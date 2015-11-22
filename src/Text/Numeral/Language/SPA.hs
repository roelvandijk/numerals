{-|
[@ISO639-1@]        es

[@ISO639-2B@]       spa

[@ISO639-3@]        spa

[@Native name@]     Español

[@English name@]    Spanish
-}

module Text.Numeral.Language.SPA
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum  as BN
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


-------------------------------------------------------------------------------
-- SPA
-------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "es"
    , entIso639_2    = ["spa"]
    , entIso639_3    = Just "spa"
    , entNativeNames = ["Español"]
    , entEnglishName = Just "Spanish"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

struct :: (Integral a) => a -> Exp
struct = pos $ fix $ rule `combine` longScale1_es
    where
      rule = findRule (   0, lit       )
                    [ (  11, add 10 L  )
                    , (  16, add 10 R  )
                    , (  20, lit       )
                    , (  21, add 20 R  )
                    , (  30, mul 10 R L)
                    , ( 100, step  100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                    (dec 6 - 1)

-- | Like 'longScale1' with the difference that all scale elements are
-- masculine.
longScale1_es :: (Integral a) => Rule a
longScale1_es = mulScale1_es 6 0 R L BN.rule
    where
      mulScale1_es = mulScale_ $ \f m s _ -> masculineMul (f m) s
      masculineMul x y = ChangeGender (Just Masculine) $ Mul x y

bounds :: (Integral a) => (a, a)
bounds = let x = dec 60000 - 1 in (negate x, x)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \inf n -> M.lookup n (syms inf)
               , reprScale = longScaleRepr
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _ -> "menos "
               }
    where
      (_                    ⊞ Lit 10) _ = ""
      (Lit 10               ⊞ _     ) _ = ""
      (Lit 20               ⊞ _     ) _ = ""
      ((Lit _ `Mul` Lit 10) ⊞ _     ) _ = " y "
      (_                    ⊞ _     ) _ = " "

      (_ ⊡ Lit n) _ | n < 1000 = ""
      (_ ⊡ _    ) _            = " "

      syms inf =
          M.fromList
          [ (0, const "cero")
          , (1, \c -> case c of
                       CtxAdd _ (Lit 10)  _  -> "on"
                       CtxAdd _ (Lit 20)  _
                           | isMasculine inf -> "ún"
                       _   | isFeminine  inf -> "una"
                           | isMasculine inf -> "un"
                           | otherwise       -> "uno"
            )
          , (2, \c -> case c of
                       CtxAdd _ (Lit 10)  _ -> "do"
                       CtxAdd _ (Lit 20)  _ -> "dós"
                       _                    -> "dos"
            )
          , (3, \c -> case c of
                       CtxAdd _ (Lit 10)  _ -> "tre"
                       CtxAdd _ (Lit 20)  _ -> "trés"
                       CtxMul _ (Lit 10)  _ -> "trein"
                       _                    -> "tres"
            )
          , (4, \c -> case c of
                       CtxAdd _ (Lit 10)  _ -> "cator"
                       CtxMul _ (Lit 10)  _ -> "cuaren"
                       _                    -> "cuatro"
            )
          , (5, \c -> case c of
                       CtxAdd _ (Lit 10)  _ -> "quin"
                       CtxMul _ (Lit 10)  _ -> "cincuen"
                       CtxMul _ (Lit 100) _ -> "quin"
                       _                    -> "cinco"
            )
          , (6, \c -> case c of
                       CtxAdd _ (Lit 10)  _ -> "séis"
                       CtxAdd _ (Lit 20)  _ -> "séis"
                       CtxMul _ (Lit 10)  _ -> "sesen"
                       _                    -> "seis"
            )
          , (7, \c -> case c of
                       CtxMul _ (Lit 10)  _ -> "seten"
                       CtxMul _ (Lit 100) _ -> "sete"
                       _                    -> "siete"
            )
          , (8, \c -> case c of
                       CtxMul _ (Lit 10)  _ -> "ochen"
                       _                    -> "ocho"
            )
          , (9, \c -> case c of
                       CtxMul _ (Lit 10)  _ -> "noven"
                       CtxMul _ (Lit 100) _ -> "nove"
                       _                    -> "nueve"
            )
          , (10, \c -> case c of
                        CtxAdd R (Lit _)  _ -> "ce"
                        CtxAdd L (Lit _)  _ -> "dieci"
                        CtxMul R _        _ -> "ta"
                        _                   -> "diez"
            )
          , (20, \c -> case c of
                        CtxAdd _ (Lit _)  _ -> "veinti"
                        _                   -> "veinte"
            )
          , (100, \c -> case c of
                         CtxEmpty           -> "cien"
                         CtxAdd {}          -> "ciento"
                         CtxMul _ (Lit 5) _
                           | isFeminine inf -> "ientas"
                           | otherwise      -> "ientos"
                         CtxMul L _       _ -> "cien"
                         _ | isFeminine inf -> "cientas"
                           | otherwise      -> "cientos"
            )
          , (1000, const "mil")
          ]

longScaleRepr :: Inflection -> Integer -> Integer -> Exp -> Ctx Exp -> Maybe Text
longScaleRepr =
    BN.scaleRepr (BN.quantityName "illón" "illones")
                 [ (4, BN.forms "cuatr" "cuator" "cuator" "cuatra" "cuatri")
                 , (9, BN.forms "non"   "noven"  "noven"  "nona"   "non")
                 ]
