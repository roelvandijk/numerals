{-|
[@ISO639-1@]        hr

[@ISO639-2B@]       cro

[@ISO639-3@]        cro

[@Native name@]     Hrvatski

[@English name@]    Croatian
-}

module Text.Numeral.Language.CRO
    ( -- * Language entries
      entry
      -- * Conversions
    , cardinal
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
-- CRO
--------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "hr"
    , entIso639_2    = ["cro"]
    , entIso639_3    = Just "cro"
    , entNativeNames = ["Hrvatski"]
    , entEnglishName = Just "Croatian"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

struct :: (Integral a) => a -> Exp
struct = pos $ fix $ rule

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = render (cardinalRepr "minus " hr_add) inf . shortScaleStruct

shortScaleStruct :: (Integral a) => a -> Exp
shortScaleStruct = pos $ fix $ rule `combine` shortScale1 R L BN.rule

pelletierScaleStruct :: (Integral a) => a -> Exp
pelletierScaleStruct = pos $ fix $ rule `combine` pelletierScale1 R L BN.rule

rule :: (Integral a) => Rule a
rule = findRule (   0, lit       )
              [ (  11, add 10 L  )
              , (  20, mul 10 R L)
              , ( 100, step  100   10 R L)
              , (1000, step 1000 1000 R L)
              ] (dec 6 - 1)

bounds :: (Integral a) => (a, a)
bounds = let x = dec 30003 - 1 in (negate x, x)

genericRepr :: (Exp -> Exp -> Ctx Exp -> Text) -- ^ Add representation.
            -> Repr
genericRepr _ =
    defaultRepr
    { reprAdd   = Just (hr_add)
    , reprMul   = Just (⊞)
    }
    where
      (_ ⊞ Lit 10) _  = ""
      (_ ⊞ Lit 100) _ = ""
      (_ ⊞ _     ) _  = " "

hr_add :: Exp -> Exp -> Ctx Exp -> Text
((Lit 100)  `hr_add` _) _       = " "
((Lit 1000)  `hr_add`_) _       = " "
((_ `Mul` _     ) `hr_add` _) _ = " "
(_                `hr_add` _) _ = ""


cardinalRepr :: Text -- ^ Negative number prefix.
             -> (Exp -> Exp -> Ctx Exp -> Text)
             -> Repr
cardinalRepr neg f =
    (genericRepr f)
    { reprValue = \_ n -> M.lookup n syms
    , reprScale = BN.scaleRepr (\_ _ -> "ljun") []
    , reprNeg   = Just $ \_ _ -> neg
    }
  where
    syms =
        M.fromList
        [ (0, const "nula")
        , (1, ten "jedan" "jeda" "jedan")
        , (2, const "dva")
        , (3, const "tri")
        , (4, ten "četiri" "četr" "četr")
        , (5, ten "pet" "pet" "pe")
        , (6, hun "šest" "šes" "še" "šes")
        , (7, const "sedam")
        , (8, const "osam")
        , (9, ten "devet" "devet" "deve")
        , (10, \c -> case c of
                      CtxAdd _ (Lit 100) _ -> "deset"
                      CtxAdd _ (Lit _) _   -> "naest"
                      _                    -> "deset"
          )
        , (60,  const "šedeset")
        , (100, const "sto")
        , (1000, \c -> case c of
                      CtxMul R _ _ -> "tisuća"
                      _            -> "tisuću")
        ]

    ten :: Text -> Text -> Text -> Ctx Exp -> Text
    ten n a m = \c -> case c of
                       CtxAdd _ (Lit 10) _ -> a
                       CtxMul _ (Lit 10) _ -> m
                       _                   -> n

    hun :: Text -> Text -> Text -> Text -> Ctx Exp -> Text
    hun n a h m = \c -> case c of
                       CtxAdd _ (Lit 10)  _ -> a
                       CtxMul _ (Lit 10)  _ -> m
                       CtxMul _ (Lit 100) _ -> h
                       _                    -> n


