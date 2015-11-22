{-|
[@ISO639-1@]        ru

[@ISO639-2@]        rus

[@ISO639-3@]        rus

[@Native name@]     Русский язык

[@English name@]    Russian
-}

module Text.Numeral.Language.RUS
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import qualified "containers" Data.Map as M ( fromList, lookup )
import "this" Text.Numeral
import "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- RUS
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "ru"
    , entIso639_2    = ["rus"]
    , entIso639_3    = Just "rus"
    , entNativeNames = ["Русский язык"]
    , entEnglishName = Just "Russian"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

struct :: (Integral a) => a -> Exp
struct = pos
       $ fix
       $ findRule (   0, lit               )
                [ (  11, add 10 L          )
                , (  20, mul 10 R L        )
                , (  40, lit               )
                , (  41, add 40 R          )
                , (  50, mul 10 R L        )
                , ( 100, step  100   10 R L)
                , (1000, step 1000 1000 R L)
                ]
                  (dec 6 - 1)

bounds :: (Integral a) => (a, a)
bounds = let x = dec 6 - 1 in (negate x, x)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _ -> "минус "
               }
    where
      (Lit n ⊞ Lit 10) _ | n < 10 = "на"
      (_     ⊞ _     ) _          = " "

      (_ ⊡ Lit n) _ | n <= 100 = ""
      (_ ⊡ _    ) _            = " "

      syms =
          M.fromList
          [ (0, const "ноль")
          , (1, \c -> case c of
                       CtxAdd _ (Lit 40)    (CtxMul {}) -> "одна"
                       CtxAdd _ (_ `Mul` _) (CtxMul {}) -> "одна"
                       _                                -> "один"
            )
          , (2, \c -> case c of
                       CtxAdd _ (Lit 10)    _           -> "две"
                       CtxAdd _ (Lit 40)    (CtxMul {}) -> "две"
                       CtxAdd _ (_ `Mul` _) (CtxMul {}) -> "две"
                       CtxMul _ (Lit n)     _ | n > 10  -> "две"
                       _                                -> "два"
            )
          , (3, const "три")
          , (4, \c -> case c of
                       CtxAdd _ (Lit 10) _ -> "четыр"
                       _                   -> "четыре"
            )
          , (5, \c -> case c of
                       CtxAdd _ (Lit 10) _ -> "пят"
                       _                   -> "пять"
            )
          , (6, \c -> case c of
                       CtxAdd _ (Lit 10) _ -> "шест"
                       _                   -> "шесть"
            )
          , (7, \c -> case c of
                       CtxAdd _ (Lit 10) _ -> "сем"
                       _                   -> "семь"
            )
          , (8, \c -> case c of
                       CtxAdd _ (Lit 10) _ -> "восем"
                       _                   -> "восемь"
            )
          , (9, \c -> case c of
                       CtxAdd _ (Lit 10) _ -> "девят"
                       CtxMul _ (Lit 10) _ -> "девя"
                       _                   -> "девять"
            )
          , (10, \c -> case c of
                        CtxAdd _ (Lit n) _ | n <= 9 -> "дцать"
                        CtxMul _ (Lit n) _ | n <  4 -> "дцать"
                                           | n <  9 -> "десят"
                                           | n == 9 -> "носто"
                        _                           -> "десять"
            )
          , (40, const "сорок")
          , (100, \c -> case c of
                         CtxMul _ (Lit n) _ | n == 2 -> "сти"
                                            | n <= 4 -> "ста"
                                            | n <= 9 -> "сот"
                         _                           -> "сто"
            )
          , (1000, \c -> case c of
                          CtxMul _ (Lit 40    `Add` Lit n) _ | n == 1 -> "тысяча"
                                                             | n <= 4 -> "тысячи"
                                                             | n >= 5 -> "тысяч"
                          CtxMul _ (_ `Mul` _ `Add` Lit n) _ | n == 1 -> "тысяча"
                                                             | n <= 4 -> "тысячи"
                                                             | n >= 5 -> "тысяч"
                          CtxMul _ (Lit n) _ | n <= 4 -> "тысячи"
                          CtxMul {}                   -> "тысяч"
                          _                           -> "тысяча"
            )
          , (dec 6, const "миллион")
          , (dec 9, const "миллиард")
          ]
