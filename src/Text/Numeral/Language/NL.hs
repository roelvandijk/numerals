{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        nl

[@ISO639-2B@]       dut

[@ISO639-3@]        nld

[@Native name@]     Nederlands

[@English name@]    Dutch
-}

module Text.Numeral.Language.NL
    ( cardinal
    , struct
    , cardinalRepr
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Bool     ( otherwise )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, (-) )

-- from base-unicode-symbols:
import Data.List.Unicode ( (∈) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.Exp.Classes as C
import qualified Text.Numeral.BigNum as BN ( rule, pelletierRepr )


--------------------------------------------------------------------------------
-- NL
--------------------------------------------------------------------------------
-- scale 3 R L

cardinal ∷ (Monoid s, IsString s, Integral α, C.Scale α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Scale α, C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β)
       ⇒ α → Maybe β
struct = pos $ fix $ rule `combine` pelletierScale R L BN.rule

rule ∷ (Integral α, C.Lit β, C.Add β, C.Mul β) ⇒ Rule α β
rule = findRule (   0, lit         )
              [ (  13, add   10 L  )
              , (  20, mul   10 L L)
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
               , reprScale = BN.pelletierRepr "iljoen" "iljoen"
                                              "iljard" "iljard"
                                              []
               , reprAdd   = (⊞)
               , reprMul   = \_ _ → Just ""
               , reprNeg   = \_   → Just "min "
               }
    where
      _     ⊞ Lit 10         = Just ""
      Lit n ⊞ _ | n ∈ [2,3]  = Just "ën"
                | n < 10     = Just "en"
                | otherwise  = Just ""
      _     ⊞ _              = Just " "

      symMap = M.fromList
               [ (0, const "nul")
               , (1, const "een")
               , (2, ten   "twee" "twin")
               , (3, ten   "drie" "der")
               , (4, ten   "vier" "veer")
               , (5, const "vijf")
               , (6, const "zes")
               , (7, const "zeven")
               , (8, \c → case c of
                            CtxMulL (Lit 10) _ → "tach"
                            CtxAddL (Lit 10) _ → "ach"
                            _                  → "acht"
                 )
               , (9, const "negen")
               , (10, \c → case c of
                             CtxMulR {} → "tig"
                             _          → "tien"
                 )
               , (11, const "elf")
               , (12, const "twaalf")
               , (100, const "honderd")
               , (1000, const "duizend")
               ]

      ten n t ctx = case ctx of
                      CtxMulL (Lit 10) _ → t
                      CtxAddL (Lit 10) _ → t
                      _                  → n
