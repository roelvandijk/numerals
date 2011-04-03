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
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, (-), Integer )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

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

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = longScaleRepr
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               , reprNeg   = Just $ \_ _ → "menos "
               }
    where
      (_                    ⊞ Lit 10) _ = ""
      (Lit 10               ⊞ _     ) _ = ""
      (Lit 20               ⊞ _     ) _ = ""
      ((Lit _ `Mul` Lit 10) ⊞ _     ) _ = " y "
      (_                    ⊞ _     ) _ = " "

      (_ ⊡ Lit n) _ | n < 1000 = ""
      (_ ⊡ _    ) _            = " "

      syms =
          M.fromList
          [ (0, const "cero")
          , (1, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "on"
                       _                    → "uno"
            )
          , (2, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "do"
                       CtxAdd _ (Lit 20)  _ → "dós"
                       _                    → "dos"
            )
          , (3, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "tre"
                       CtxAdd _ (Lit 20)  _ → "trés"
                       CtxMul _ (Lit 10)  _ → "trein"
                       _                    → "tres"
            )
          , (4, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "cator"
                       CtxMul _ (Lit 10)  _ → "cuaren"
                       _                    → "cuatro"
            )
          , (5, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "quin"
                       CtxMul _ (Lit 10)  _ → "cincuen"
                       CtxMul _ (Lit 100) _ → "quin"
                       _                    → "cinco"
            )
          , (6, \c → case c of
                       CtxAdd _ (Lit 10)  _ → "séis"
                       CtxAdd _ (Lit 20)  _ → "séis"
                       CtxMul _ (Lit 10)  _ → "sesen"
                       _                    → "seis"
            )
          , (7, \c → case c of
                       CtxMul _ (Lit 10)  _ → "seten"
                       CtxMul _ (Lit 100) _ → "sete"
                       _                    → "siete"
            )
          , (8, \c → case c of
                       CtxMul _ (Lit 10)  _ → "ochen"
                       _                    → "ocho"
            )
          , (9, \c → case c of
                       CtxMul _ (Lit 10)  _ → "noven"
                       CtxMul _ (Lit 100) _ → "nove"
                       _                    → "nueve"
            )
          , (10, \c → case c of
                        CtxAdd R (Lit _)  _ → "ce"
                        CtxAdd L (Lit _)  _ → "dieci"
                        CtxMul R _        _ → "ta"
                        _                   → "diez"
            )
          , (20, \c → case c of
                        CtxAdd _ (Lit _)  _ → "veinti"
                        _                   → "veinte"
            )
          , (100, \c → case c of
                         CtxEmpty           → "cien"
                         CtxAdd {}          → "ciento"
                         CtxMul _ (Lit 5) _ → "ientos"
                         CtxMul L _       _ → "cien"
                         _                  → "cientos"
            )
          , (1000, const "mil")
          ]

longScaleRepr ∷ (IsString s, Monoid s)
              ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
longScaleRepr =
    BN.scaleRepr "illón" "illones"
                 [ (4, BN.forms "cuatr" "cuator" "cuator" "cuatra" "cuatri")
                 , (9, BN.forms "non"   "noven"  "noven"  "nona"   "non")
                 ]

