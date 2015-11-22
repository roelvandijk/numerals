{-|
[@ISO639-1@]        bg

[@ISO639-2@]        bul

[@ISO639-3@]        bul

[@Native name@]     Български език

[@English name@]    Bulgarian
-}

module Text.Numeral.Language.BUL
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
import qualified "containers" Data.Map as M
import "this" Text.Numeral
import "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import qualified "this" Text.Numeral.BigNum  as BN
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- BUL
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "bg"
    , entIso639_2    = ["bul"]
    , entIso639_3    = Just "bul"
    , entNativeNames = ["Български език"]
    , entEnglishName = Just "Bulgarian"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

struct :: (Integral a) => a -> Exp
struct = pos $ fix $ rule `combine` shortScale1_bg
    where
       rule = findRule (   0, lit               )
                     [ (  13, add 10 L          )
                     , (  20, mul 10 R L        )
                     , ( 100, step  100   10 R L)
                     , (1000, step 1000 1000 R L)
                     ]
                     (dec 6 - 1)

-- | Like 'shortScale1' but forces the right-hand-side to have
-- masculine gender.
shortScale1_bg :: (Integral a) => Rule a
shortScale1_bg = mulScale_ bgMul 3 3 R L rule
    where
      bgMul f m scale' _ = masculineMul (f m) scale'
      masculineMul x y = ChangeGender (Just Masculine) $ Mul x y
      rule = findRule (1, lit) [] 14

bounds :: (Integral a) => (a, a)
bounds = let x = dec 48 - 1 in (negate x, x)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \inf n -> M.lookup n (syms inf)
               , reprScale = shortScaleRepr
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _ -> "минус "
               }
    where
      (Lit 100 ⊞  Lit _)          _ = " и "
      (_ ⊞ Add _ (Lit 10))        _ = " и "
      (_ ⊞ Mul _ (Lit 10))        _ = " и "
      (_ ⊞ Lit 10)                _ = ""
      ((_ `Mul` Lit _)   ⊞ Lit _) _ = " и "
      (ChangeGender _ _  ⊞ Lit _) _ = " и "
      (_                 ⊞ _ )    _ = " "

      (_ ⊡ Lit n) _ | n <= 100 = ""
      (_ ⊡ _    ) _            = " "

      syms inf =
          M.fromList
          [ (0, const "нула")
          , (1, const $ if isFeminine inf
                        then "една"
                        else if isMasculine inf
                             then "един"
                             else "едно"
            )
          , (2, \c -> case c of
                       CtxMul _ (Lit   10) _ -> "два"
                       CtxMul _ (Lit  100) _ -> "две"
                       CtxMul _ (Lit 1000) _ -> "две"
                       _ | isMasculine inf   -> "два"
                         | otherwise         -> "две"
            )
          , (3, const "три")
          , (4, const "четири")
          , (5, const "пет")
          , (6, const "шест")
          , (7, const "седем")
          , (8, const "осем")
          , (9, const "девет")
          , (10, \c -> case c of
                        CtxAdd _ (Lit n) _
                            | n <= 9 -> "надесет"
                        _            -> "десет"
            )
          , (11, const "единадесет")
          , (12, const "дванадесет")
          , (100, \c -> case c of
                         CtxMul _ (Lit 2) _ -> "ста"
                         CtxMul _ (Lit 3) _ -> "ста"
                         CtxMul R _ _       -> "стотин"
                         _                  -> "сто"
            )
          , (1000, \c -> case c of
                          CtxMul R _ _ -> "хиляди"
                          _            -> "хиляда"
            )
          ]

shortScaleRepr :: Inflection -> Integer -> Integer -> Exp -> Ctx Exp -> Maybe Text
shortScaleRepr inf b o e
    = case e of
        Lit 2 -> BN.scaleRepr (BN.quantityName "илиард" "илиарда") syms inf b o e
        _ -> BN.scaleRepr (BN.quantityName "илион" "илиона") syms inf b o e
    where
      syms = [ ( 1, const "м")
             , ( 2, const "м")
             , ( 3, const "тр")
             , ( 4, const "квадр")
             , ( 5, const "квинт")
             , ( 6, const "секст")
             , ( 7, const "септ")
             , ( 8, const "окт")
             , ( 9, const "нон")
             , (10, const "дец")
             , (11, const "индец")
             , (12, const "дуодец")
             , (13, const "тридец")
             , (14, const "куадродец")
             ]
