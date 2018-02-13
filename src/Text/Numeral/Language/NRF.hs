{-|
[@ISO639-1@]        -

[@ISO639-2B@]       -

[@ISO639-3@]        nrf

[@Native name@]     Jèrriais

[@English name@]    Jersey French / Jersey Norman French
-}

module Text.Numeral.Language.NRF
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , cardinalStruct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import qualified "containers" Data.Map as M
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum  as BN
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- NRF
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Nothing
    , entIso639_2    = []
    , entIso639_3    = Just "nrf"
    , entNativeNames = ["Jèrriais"]
    , entEnglishName = Just "Jersey French / Jersey Norman French"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = cardinalStruct
                       }
    , entOrdinal     = Nothing
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . cardinalStruct

cardinalStruct :: (Integral a) => a -> Exp
cardinalStruct = pos $ fix $ rule `combine` pelletierScale1 R L BN.rule

rule :: (Integral a) => Rule a
rule = findRule (   0, lit         )
              [ (  11, add   10 L  )
              , (  17, add   10 R  )
              , (  20, lit         )
              , (  21, add   20 R  )
              , (  30, mul   10 R L)
              , (  80, mul   20 R L)
              , (  89, add   80 R  )
              , (  90, mul   10 R L)
              , (  91, add   90 R  )
              , ( 100, step  100   10 R L)
              , (1000, step 1000 1000 R L)
              , (dec 6, step (dec 6) 1000 R L)
              ]
                (snd bounds)

bounds :: (Integral a) => (a, a)
bounds = (1, dec 9 - 1)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr =
    render defaultRepr
           { reprValue = \inf n -> M.lookup n (syms inf)
           , reprAdd   = Just (⊞)
           , reprMul   = Just (⊡)
           , reprNeg   = Just $ \_ _ -> "moins "
           }
  where
    (Lit n                ⊞ Lit 10) _  | n <= 6 = ""
    (Lit 10               ⊞ Lit n ) _  | n >= 7 = "-"
    ((Lit 4 `Mul` Lit 20) ⊞ _     ) _           = "-"
    (_                    ⊞ Lit 1 ) _           = "’tch’"
    ((Lit _ `Mul` Lit 10) ⊞ _     ) _           = "-"
    (Lit 20               ⊞ _     ) _           = "-"
    (_                    ⊞ _     ) _           = " "

    (_ ⊡ Lit 10) _ = ""
    (_ ⊡ Lit 20) _ = "-"
    (_ ⊡ _     ) _ = " "

    syms _inf =
        M.fromList
        [ (1, \c -> case c of
                     CtxAdd _ (Lit 10) _ -> "on"
                     _ -> "ieune"
          )
        , (2, ten   "deux"   "dou"    "deux")
        , (3, ten   "trais"  "trei"   "tren")
        , (4, ten   "quat’"  "quator" "quar")
        , (5, ten   "chîn"   "tchîn"  "chînqu")
        , (6, ten   "six"    "sei"    "souaix")
        , (7, const "sept")
        , (8, const "huit")
        , (9, ten   "neuf"   "neuf"   "nén")
        , (10, \c -> case c of
                      CtxAdd _ (Lit n) _ | n < 7     -> "ze"
                                         | otherwise -> "dgiêx"
                      CtxMul _ (Lit 3) _             -> "te"
                      CtxMul R _       _             -> "ante"
                      _                              -> "dgix"
          )
        , (20, \c -> case c of
                      CtxMul _ _ CtxEmpty -> "vîngts"
                      _                   -> "vîngt"
          )
        , (100, \c -> case c of
                       CtxMul R _ CtxEmpty -> "chents"
                       _                   -> "chent"
          )
        , (1000, const "mille")
        , (dec 6, const "million")
        ]

    ten n a m ctx = case ctx of
                      CtxAdd _ (Lit 10) _ -> a
                      CtxMul _ (Lit 10) _ -> m
                      _                   -> n
