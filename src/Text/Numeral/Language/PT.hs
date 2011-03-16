{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        pt

[@ISO639-2@]        por

[@ISO639-3@]        por

[@Native name@]     Português

[@English name@]    Portuguese
-}

module Text.Numeral.Language.PT
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
import Data.Functor  ( (<$>) )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, (-), Integer )

-- from base-unicode-symbols:
import Data.Eq.Unicode     ( (≡) )
import Data.Monoid.Unicode ( (⊕) )
import Data.Ord.Unicode    ( (≤) )

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
-- PT
-------------------------------------------------------------------------------

-- Sources:
--   http://www.sonia-portuguese.com/text/numerals.htm
--   http://www.smartphrase.com/Portuguese/po_numbers_voc.shtml

cardinal ∷ (Monoid s, IsString s, Integral α, C.Scale α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ ( Integral α, C.Scale α
         , C.Lit β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → Maybe β
struct = checkPos (fix $ rule `combine` shortScale R L BN.rule)
    where
      rule = findRule (   0, lit         )
                    [ (  11, add   10 L  )
                    , (  16, add   10 R  )
                    , (  20, mul   10 R L)
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
               , reprScale = shortScaleRepr
               , reprAdd   = (⊞)
               , reprMul   = (⊡)
               }
    where
      Lit 10 ⊞ Lit n | n < 8     = Just "as"
                     | n ≡ 8     = Just ""
                     | otherwise = Just "a"
      Lit _  ⊞ Lit 10            = Just ""
      _      ⊞ _                 = Just " e "


      _ ⊡ Lit 10  = Just ""
      _ ⊡ Lit 100 = Just ""
      _ ⊡ _       = Just " "

      symMap = M.fromList
               [ (0, const "zero")
               , (1, \c → case c of
                            CtxAddL (Lit 10) _ → "on"
                            _                  → "um"
                 )
               , (2, \c → case c of
                            CtxAddL (Lit 10)  _ → "do"
                            CtxMulL (Lit 10)  _ → "vin"
                            CtxMulL (Lit 100) _ → "duz"
                            _                   → "dois"
                 )
               , (3, \c → case c of
                            CtxAddL (Lit 10)  _ → "tre"
                            CtxMulL (Lit 10)  _ → "trin"
                            CtxMulL (Lit 100) _ → "trez"
                            _                   → "três"
                 )
               , (4, \c → case c of
                            CtxAddL (Lit 10)  _ → "cator"
                            CtxMulL (Lit 10)  _ → "quaren"
                            _                   → "quatro"
                 )
               , (5, \c → case c of
                            CtxAddL (Lit 10)  _ → "quin"
                            CtxMulL (Lit 10)  _ → "cinquen"
                            CtxMulL (Lit 100) _ → "quin"
                            _                   → "cinco"
                 )
               , (6, \c → case c of
                            CtxMulL (Lit 10) _ → "sessen"
                            _                  → "seis"
                 )
               , (7, \c → case c of
                            CtxMulL (Lit 10) _ → "seten"
                            _                  → "sete"
                 )
               , (8, \c → case c of
                            CtxMulL (Lit 10) _ → "oiten"
                            _                  → "oito"
                 )
               , (9, \c → case c of
                            CtxMulL (Lit 10) _ → "noven"
                            _                  → "nove"
                 )
               , (10, \c → case c of
                             CtxAddR (Lit _) _ → "ze"
                             CtxMulR (Lit 2) _ → "te"
                             CtxMulR (Lit _) _ → "ta"
                             _                 → "dez"
                 )
               , (100, \c → case c of
                              CtxAddL {}      → "cento"
                              CtxMulR (Lit n) _
                                  | n ≤ 3     → "entos"
                                  | n ≡ 4     → "centos"
                                  | n ≡ 5     → "hentos"
                                  | otherwise → "centos"
                              _               → "cem"
                 )
               , (1000, const "mil")
               ]

shortScaleRepr ∷ (IsString s, Monoid s)
               ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
shortScaleRepr _ _ e c = let s = case c of
                                   CtxMulR {} → "ilhões"
                                   _          → "ilhão"
                         in (⊕ s) <$> textify repr e
    where
      repr = BN.cardinalRepr { reprValue = \n → M.lookup n $ diff ∪ BN.symMap }
      diff = M.fromList
             [ (4, BN.forms "quatr" "quator" "quator" "quatra" "quatri")
             ]

