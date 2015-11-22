{-# LANGUAGE FlexibleContexts  #-}

{-|
[@ISO639-1@]        he

[@ISO639-2@]        heb

[@ISO639-3@]        heb

[@Native name@]     עִבְרִית

[@English name@]    Modern Hebrew
-}

module Text.Numeral.Language.HEB
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

import "base" Data.Monoid ( (<>) )
import "base" Data.Function ( fix )
import qualified "containers" Data.Map as M ( fromList, lookup )
import "this" Text.Numeral
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- HEB
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1       = Just "he"
    , entIso639_2       = ["heb"]
    , entIso639_3       = Just "heb"
    , entNativeNames    = ["עִבְרִית"]
    , entEnglishName    = Just "Modern Hebrew"
    , entCardinal       = Just Conversion
                          { toNumeral   = cardinal
                          , toStructure = struct
                          }
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

struct :: (Integral a) => a -> Exp
struct = pos
       $ fix
       $ findRule (  0, lit)
                  ( [ (11, add 10 L)
                    , (20, changeNumber (Just Dual) . mapRule (`div` 10) lit)
                    , (21, add 20 R)
                    ]
                  <> concat [ [ (n, changeNumber (Just Plural) . mapRule (`div` 10) lit)
                             , (n+1, add n R)
                             ]
                           | n <- [20,30..90]
                           ]
                  <> [ (100, step 100 10 R L)
                    , (200, changeNumber (Just Dual) . mapRule (`div` 2) lit)
                    , (201, add 200 R)
                    , (300, step 100 10 R L)
                    ]
                  )
                  1000

bounds :: (Integral a) => (a, a)
bounds = let x = 1000 in (negate x, x)

-- TODO: Handle inflection, mainly masculine and feminine
-- gender. Maybe get rid of Dual and Plural in the expression language
-- and use grammatical number for that.
cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \i n -> M.lookup n (syms i)
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _ -> "מינוס "
               , reprAddCombine = Just addCombine
               }
    where
      (_ ⊞ Lit 10) _ = "ה"
      (ChangeNumber _ _ ⊞ _) _ = " ו"
      (_ ⊞ _) _ = " "

      (_ ⊡ Lit 10) _ = ""
      (_ ⊡ _     ) _ = " "

      addCombine a x _ y (Lit 10) = x <> " " <> y <> a
      addCombine a x _ y _        = x <> a <> y

      syms inf =
          M.fromList
          [ (0, \c -> case c of
                       _ -> "אפס"
            )
          , (1, \c -> case c of
                       _ -> "אחת"
            )
          , (2, \c -> case c of
                       CtxAdd _ (Lit 10) _ -> "שתים"
                       _ | isDual inf    -> "עשרים"
                         | otherwise       -> "שתיים"
            )
          , (3, \c -> case c of
                       _ | isPlural inf  -> "שלושים"
                         | otherwise       -> "שלוש"
            )
          , (4, \c -> case c of
                       _ | isPlural inf -> "ארבעים"
                         | otherwise      -> "ארבע"
            )
          , (5, \c -> case c of
                       _ | isPlural inf -> "חמישים"
                         | otherwise      -> "חמש"
            )
          , (6, \c -> case c of
                       _ | isPlural inf -> "ששים"
                         | otherwise      -> "שש"
            )
          , (7, \c -> case c of
                       _ | isPlural inf -> "שבעים"
                         | otherwise      -> "שבע"
            )
          , (8, \c -> case c of
                       _ | isPlural inf -> "שמונים"
                         | otherwise      -> "שמונה"
            )
          , (9, \c -> case c of
                       _ | isPlural inf -> "תשעים"
                         | otherwise      -> "תשע"
            )
          , (10, \c -> case c of
                        CtxMul _ (Lit n) _ | n <= 9 -> ""
                        _ -> "עשר"
            )
          , (100, \c -> case c of
                         _ | isDual   inf -> "מאתיים"
                           | isPlural inf -> "מאות"
                           | otherwise    -> "מאה"
            )
          , (1000, \c -> case c of
                          _ -> "אלף"
            )
          ]
