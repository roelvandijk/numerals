{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        es

[@ISO639-2B@]       spa

[@ISO639-3@]        spa

[@Native name@]     Español

[@English name@]    Spanish
-}

module Text.Numeral.Language.ES
    ( cardinal
    , struct
    , cardinalRepr
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( ($), const, fix )
import Data.Functor  ( (<$>) )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, (-), Integer )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from containers-unicode-symbols:
import Data.Map.Unicode ( (∪) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.BigNum      as BN
import qualified Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- ES
-------------------------------------------------------------------------------

-- Sources:
--   http://www.sf.airnet.ne.jp/~ts/language/number/spanish.html
--   http://spanish.about.com/cs/forbeginners/a/cardinalnum_beg.htm
--   http://www.learn-spanish-help.com/count-in-spanish.html
--   http://www.donquijote.org/spanishlanguage/numbers/numbers1.asp
--   http://en.wiktionary.org/wiki/Appendix:Spanish_numerals

cardinal ∷ (Monoid s, IsString s, Integral α, C.Scale α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ ( Integral α, C.Scale α
         , C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → Maybe β
struct = pos (fix $ rule `combine` longScale R L BN.rule)
    where
      rule = findRule (   0, lit         )
                    [ (  11, add   10 L  )
                    , (  16, add   10 R  )
                    , (  20, lit         )
                    , (  21, add   20 R  )
                    , (  30, mul   10 R L)
                    , ( 100, lit         )
                    , ( 101, add  100 R  )
                    , ( 200, mul  100 R L)
                    , (1000, lit         )
                    , (1001, add 1000 R  )
                    , (2000, mul 1000 R L)
                    ]
                    (dec 6 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprScale = longScaleRepr
               , reprAdd   = (⊞)
               , reprMul   = (⊡)
               , reprNeg   = \_ → Just "menos "
               }
    where
      _                    ⊞ Lit 10 = Just ""
      Lit 10               ⊞ _      = Just ""
      Lit 20               ⊞ _      = Just ""
      (Lit _ `Mul` Lit 10) ⊞ _      = Just " y "
      _                    ⊞ _      = Just " "

      _ ⊡ Lit n | n < 1000 = Just ""
      _ ⊡ _                = Just " "

      symMap = M.fromList
               [ (0, const "cero")
               , (1, \c → case c of
                            CtxAddL (Lit 10)  _ → "on"
                            _                   → "uno"
                 )
               , (2, \c → case c of
                            CtxAddL (Lit 10)  _ → "do"
                            CtxAddR (Lit 20)  _ → "dós"
                            _                   → "dos"
                 )
               , (3, \c → case c of
                            CtxAddL (Lit 10)  _ → "tre"
                            CtxAddR (Lit 20)  _ → "trés"
                            CtxMulL (Lit 10)  _ → "trein"
                            _                   → "tres"
                 )
               , (4, \c → case c of
                            CtxAddL (Lit 10)  _ → "cator"
                            CtxMulL (Lit 10)  _ → "cuaren"
                            _                   → "cuatro"
                 )
               , (5, \c → case c of
                            CtxAddL (Lit 10)  _ → "quin"
                            CtxMulL (Lit 10)  _ → "cincuen"
                            CtxMulL (Lit 100) _ → "quin"
                            _                   → "cinco"
                 )
               , (6, \c → case c of
                            CtxAddR (Lit 10)  _ → "séis"
                            CtxAddR (Lit 20)  _ → "séis"
                            CtxMulL (Lit 10)  _ → "sesen"
                            _                   → "seis"
                 )
               , (7, \c → case c of
                            CtxMulL (Lit 10)  _ → "seten"
                            CtxMulL (Lit 100) _ → "sete"
                            _                   → "siete"
                 )
               , (8, \c → case c of
                            CtxMulL (Lit 10)  _ → "ochen"
                            _                   → "ocho"
                 )
               , (9, \c → case c of
                            CtxMulL (Lit 10)  _ → "noven"
                            CtxMulL (Lit 100) _ → "nove"
                            _                   → "nueve"
                 )
               , (10, \c → case c of
                             CtxAddR (Lit _)  _ → "ce"
                             CtxAddL (Lit _)  _ → "dieci"
                             CtxMulR {}         → "ta"
                             _                  → "diez"
                 )
               , (20, \c → case c of
                             CtxAddL (Lit _)  _ → "veinti"
                             _                  → "veinte"
                 )
               , (100, \c → case c of
                              CtxEmpty          → "cien"
                              CtxAddL {}        → "ciento"
                              CtxMulR (Lit 5) _ → "ientos"
                              CtxMulL {}        → "cien"
                              _                 → "cientos"
                 )
               , (1000, const "mil")
               ]

longScaleRepr ∷ (IsString s, Monoid s)
              ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
longScaleRepr _ _ e c = let s = case c of
                                  CtxMulR {} → "illones"
                                  _          → "illón"
                        in (⊕ s) <$> textify repr e
    where
      repr = BN.cardinalRepr { reprValue = \n → M.lookup n $ diff ∪ BN.symMap }
      diff = M.fromList
             [ (4, BN.forms "cuatr" "cuator" "cuator" "cuatra" "cuatri")
             , (9, BN.forms "non"   "noven"  "noven"  "nona"   "non")
             ]

