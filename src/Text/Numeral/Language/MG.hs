{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        mg

[@ISO639-2@]        mlg

[@ISO639-3@]        mlg

[@Native name@]     -

[@English name@]    Malagasy
-}

module Text.Numeral.Language.MG
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
import Data.List     ( concat, map )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, (+), (-) )

-- from base-unicode-symobls:
import Prelude.Unicode ( (⋅) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- MG
-------------------------------------------------------------------------------

{-
Sources:
  http://en.wikipedia.org/wiki/Malagasy_language
  http://www.sf.airnet.ne.jp/~ts/language/number/malagasy.html
-}

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Add β, C.Mul β)
       ⇒ α → Maybe β
struct = checkPos
       $ fix
       $ findRule (0, lit)
                  (concat [ [ (  n  , lit)
                            , (  n+1, add n L)
                            , (2⋅n  , mul n L L)
                            ]
                          | n ← map dec [1..6]
                          ]
                  )
                  (dec 7 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = (⊞)
               , reprMul   = (⊡)
               }
    where
      _ ⊞              Lit 10  = Just " ambin'ny "
      _ ⊞ (Lit _ `Mul` Lit 10) = Just " amby "
      _ ⊞ _                    = Just " sy "

      _ ⊡ Lit 10  = Just ""
      _ ⊡ Lit 100 = Just ""
      _ ⊡ _       = Just " "

      symMap = M.fromList
               [ (0, const "haotra")
               , (1, \c → case c of
                            CtxAddL {} → "iraika"
                            _          → "iray"
                 )
               , (2, mulForms "roa"    "roa"   "roan")
               , (3, mulForms "telo"   "telo"  "telon")
               , (4, mulForms "efatra" "efa"   "efa"  )
               , (5, mulForms "dimy"   "dimam" "diman")
               , (6, mulForms "enina"  "enim"  "enin" )
               , (7, mulForms "fito"   "fito"  "fiton")
               , (8, mulForms "valo"   "valo"  "valon")
               , (9, mulForms "sivy"   "sivi"  "sivin")
               , (10, \c → case c of
                             CtxMulR (Lit n) _ | n < 9 → "polo"
                             _ → "folo"
                 )
               , (100, \c → case c of
                              CtxMulR {} → "jato"
                              _          → "zato"
                 )
               , (1000, const "arivo")
               , (dec 4, const "alina")
               , (dec 5, const "hetsy")
               , (dec 6, const "tapitrisa")
               ]

      mulForms o t h = \c → case c of
                              CtxMulL (Lit 10) _  → t
                              CtxMulL (Lit 100) _ → h
                              _                   → o
