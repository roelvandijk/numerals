{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        de

[@ISO639-2B@]       ger

[@ISO639-3@]        deu

[@Native name@]     Deutsch

[@English name@]    German
-}

module Text.Numeral.Language.DE
    ( cardinal
    , struct
    , cardinalRepr
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Bool     ( otherwise )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, (-), Integer )

-- from base-unicode-symbols:
import Data.List.Unicode ( (∈) )
import Data.Ord.Unicode  ( (≥) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.BigNum      as BN
import qualified Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- DE
-------------------------------------------------------------------------------

{-
Sources:
  http://de.wikipedia.org/wiki/Zahlennamen
-}

cardinal ∷ (Monoid s, IsString s, Integral α, C.Scale α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ ( Integral α, C.Scale α
         , C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → Maybe β
struct = pos $ fix $ rule `combine` pelletierScale R L BN.rule
    where
      rule = findRule (   0, lit         )
                    [ (  13, add   10   L)
                    , (  20, mul   10 L L)
                    , ( 100, lit1        )
                    , ( 101, add  100 R  )
                    , ( 200, mul1 100 R L)
                    , (1000, lit         )
                    , (1001, add 1000 R  )
                    , (2000, mul 1000 R L)
                    ]
                      (dec 6 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprScale = pelletierRepr
               , reprAdd   = (⊞)
               , reprMul   = \_ _ → Just ""
               , reprNeg   = \_   → Just "minus "
               }
    where
      _ ⊞ (_ `Mul` Lit 10) = Just "und"
      _ ⊞ _                = Just ""

      symMap = M.fromList
               [ (0, const "null")
               , (1, \c → case c of
                            CtxAddL {}      → "ein"
                            CtxMulL (Lit n) _
                                | n ≥ dec 6 → "eine"
                                | n ≥ 100   → "ein"
                            _               → "eins"
                 )
               , (2, \c → case c of
                            CtxMulL (Lit 10) _ → "zwan"
                            _                  → "zwei"
                 )
               , (3, const "drei")
               , (4, const "vier")
               , (5, const "fünf")
               , (6, \c → case c of
                            CtxAddL (Lit 10) _ → "sech"
                            CtxMulL (Lit 10) _ → "sech"
                            _                  → "sechs"
                 )
               , (7, \c → case c of
                            CtxAddL (Lit 10) _ → "sieb"
                            CtxMulL (Lit 10) _ → "sieb"
                            _                  → "sieben"
                 )
               , (8, const "acht")
               , (9, const "neun")
               , (10, \c → case c of
                             CtxMulR (Lit 3) _ → "ßig"
                             CtxMulR (Lit _) _ → "zig"
                             _                 → "zehn"
                 )
               , (11, const "elf")
               , (12, const "zwölf")
               , (100, const "hundert")
               , (1000, const "tausend")
               ]

pelletierRepr ∷ (IsString s, Monoid s)
              ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
pelletierRepr =
    BN.pelletierRepr
      "illion"   "illion"
      "illiarde" "illiarde"
      [ (8, BN.forms "okt" "okto" "okto" "okto" "oktin")
      , (10, \c → case c of
                    CtxAddL (Lit 100) _             → "dezi"
                    CtxMulR _ (CtxAddL (Lit 100) _) → "ginta"
                    CtxMulR {}                      → "gint"
                    _                               → "dez"
        )
      , (100, \c → case c of
                     CtxMulR (Lit n) _
                         | n ∈ [2,3,6] → "zent"
                         | otherwise   → "gent"
                     _                 → "zent"
        )
      ]
