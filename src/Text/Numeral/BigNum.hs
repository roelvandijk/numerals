module Text.Numeral.BigNum
  ( cardinal
  , rule
  , cardinalRepr
  , symMap
  , forms

  , PostfixRepr
  , scaleRepr
  , pelletierRepr

  , quantityName
  , ordQuantityName
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import "base" Data.Monoid ( (<>) )
import qualified "containers" Data.Map as M
import "this" Text.Numeral
import "this" Text.Numeral.Misc ( dec )
import "text" Data.Text ( Text )


--------------------------------------------------------------------------------
-- Language of Big Numbers
--------------------------------------------------------------------------------

cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = render cardinalRepr inf . (pos $ fix rule)

rule :: (Integral a) => Rule a
rule = findRule (   1, lit         )
              [ (  11, add   10 L  )
              , (  20, mul   10 L L)
              , ( 100, lit         )
              , ( 101, add  100 L  )
              , ( 200, mul  100 R L)
              , (1000, lit         )
              , (1001, add 1000 L  )
              , (2000, mul 1000 R L)
              ]
                (dec 4 - 1)

cardinalRepr :: Repr
cardinalRepr =
    defaultRepr { reprValue = \_ n -> M.lookup n symMap
                , reprAdd   = Just $ \_ _ _ -> ""
                , reprMul   = Just $ \_ _ _ -> ""
                }

symMap :: (Integral a) => M.Map a (Ctx Exp -> Text)
symMap = M.fromList
         [ (1, forms "m"     "un"       "un"       ""        "")
         , (2, forms "b"     "duo"      "duo"      "vi"      "du")
         , (3, forms "tr"    "tre"      "tres"     "tri"     "tre")
         , (4, forms "quadr" "quattuor" "quattuor" "quadra"  "quadri")
         , (5, forms "quint" "quin"     "quinqua"  "quinqua" "quin")
         , (6, forms "sext"  "sex"      "ses"      "sexa"    "ses")
         , (7, forms "sept"  "septen"   "septem"   "septua"  "septin")
         , (8, forms "oct"   "octo"     "octo"     "octo"    "octin")
         , (9, forms "non"   "novem"    "novem"    "nona"    "non")
         , (10, \c -> case c of
                       CtxAdd _ (Lit 100) _              -> "deci"
                       CtxMul _ _ (CtxAdd L (Lit 100) _) -> "ginta"
                       CtxMul {}                         -> "gint"
                       _                                 -> "dec"
           )
         , (100, \c -> case c of
                        CtxMul _ (Lit n) _
                            | n `elem` [2,3,6] -> "cent"
                            | otherwise        -> "gent"
                        _                      -> "cent"
           )
         , (1000, const "millin")
         ]

forms :: Text -> Text -> Text -> Text -> Text -> Ctx Exp -> Text
forms t a1 a2 m1 m2 ctx =
    case ctx of
      CtxAdd _ (Lit 10)  _ -> a1
      CtxAdd {}            -> a2
      CtxMul _ (Lit 10)  _ -> m1
      CtxMul {}            -> m2
      _                    -> t


--------------------------------------------------------------------------------
-- Representations of scales
--------------------------------------------------------------------------------

-- | Function that renders the postfix part of a large number name. Or
-- more simply put, this calculates the \"illion\" part of
-- \"million\", \"billion\", etc.
type PostfixRepr = Inflection -- ^ Current inflection.
                 -> Ctx Exp   -- ^ Context.
                 -> Text      -- ^ Postfix representation.

scaleRepr :: PostfixRepr
          -> [(Integer, Ctx Exp -> Text)] -- ^ Additional symbol map entries.
          -> ScaleRepr
scaleRepr pf syms inf _ _ e ctx = (<> pf inf ctx) <$> render repr inf e
    where
      repr = cardinalRepr { reprValue = \_ n -> M.lookup n syms' }
      syms' = M.fromList syms `M.union` symMap

pelletierRepr :: PostfixRepr -- ^Postfix for offset 0 names.
              -> PostfixRepr -- ^Postfix for offset 3 names.
              -> [(Integer, Ctx Exp -> Text)] -- ^ Additional symbol map entries.
              -> ScaleRepr
pelletierRepr pf0 pf3 syms inf
              b o e ctx | o == 0 = scaleRepr pf0 syms inf b o e ctx
                        | o == 3 = scaleRepr pf3 syms inf b o e ctx
                        | otherwise = Nothing

quantityName :: Text -> Text -> PostfixRepr
quantityName s p _ ctx =
    case ctx of
      CtxMul _ (Lit 1) _ -> s
      CtxMul {}          -> p
      _                  -> s

ordQuantityName :: Text -> Text -> Text -> Text -> PostfixRepr
ordQuantityName sc so pc po _ ctx =
    case ctx of
      CtxMul _ (Lit 1) _ | outside   -> so
                         | otherwise -> sc
      CtxMul {}          | outside   -> po
                         | otherwise -> pc
      _                  | outside   -> so
                         | otherwise -> sc
    where
      outside = isOutside R ctx
