{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        it

[@ISO639-2B@]       ita

[@ISO639-3@]        ita

[@Native name@]     Italiano

[@English name@]    Italian
-}

module Text.Numeral.Language.IT
    ( cardinal
    , struct
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, (-) )

-- from base-unicode-symbols:
import Data.List.Unicode ( (∈) )
import Data.Ord.Unicode  ( (≥) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- IT
--------------------------------------------------------------------------------

-- Sources:
--   http://www.sf.airnet.ne.jp/~ts/language/number/italian.html
--   http://www.orbilat.com/Languages/Italian/Grammar/Italian-Numerals.html
--   http://italian.about.com/library/weekly/aa042600a.htm

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Neg β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = pos
       $ fix
       $ findRule (   0, lit         )
                [ (  11, add   10 L  )
                , (  17, add   10 R  )
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
               , reprAdd   = (⊞)
               , reprMul   = (⊡)
               , reprNeg   = \_ → Just "meno "
               }
    where
      Lit 10 ⊞ Lit 7 = Just "as"
      Lit 10 ⊞ Lit 9 = Just "an"
      _      ⊞ _     = Just ""


      Lit n ⊡ Lit 10 | n ≥ 4 = Just "an"
      _     ⊡ _              = Just ""

      symMap = M.fromList
               [ (0, const "zero")
               , (1, \c → case c of
                            CtxAddL (Lit 10) _ → "un"
                            _                  → "uno"
                 )
               , (2, \c → case c of
                            CtxAddL (Lit 10) _ → "do"
                            _                  → "due"
                 )
               , (3, \c → case c of
                            CtxAddR {}         → "tré"
                            CtxMulL (Lit 10) _ → "tren"
                            _                  → "tre"
                 )
               , (4, \c → case c of
                            CtxAddL (Lit 10) _ → "quattor"
                            CtxMulL (Lit 10) _ → "quar"
                            _                  → "quattro"
                 )
               , (5, \c → case c of
                            CtxAddL (Lit 10) _ → "quin"
                            CtxMulL (Lit 10) _ → "cinqu"
                            _                  → "cinque"
                 )
               , (6, \c → case c of
                            CtxAddL (Lit 10) _ → "se"
                            CtxMulL (Lit 10) _ → "sess"
                            _                  → "sei"
                 )
               , (7, \c → case c of
                            CtxMulL (Lit 10) _ → "sett"
                            _                  → "sette"
                 )
               , (8, \c → case c of
                            CtxMulL (Lit 10) _ → "ott"
                            _                  → "otto"
                 )
               , (9, \c → case c of
                            CtxMulL (Lit 10) _ → "nov"
                            _                  → "nove"
                 )
               , (10, \c → case c of
                             CtxAddR (Lit _) _ → "dici"
                             CtxAddL (Lit _) _ → "dici"
                             -- Last vowel removed because of a
                             -- phonetic rule:
                             CtxMulR (Lit _) (CtxAddL (Lit n) _)
                                 | n ∈ [1,8]   → "t"
                             CtxMulR (Lit _) _ → "ta"
                             _                 → "dieci"
                 )
               , (20, \c → case c of
                             CtxAddL (Lit n) _
                                 | n ∈ [1,8]   → "vent"
                             _                 → "venti"
                 )
               , (100, \c → case c of
                              _ → "cento"
                 )
               , (1000, \c → case c of
                               CtxMulR {} → "mila"
                               _          → "mille"
                 )
               ]
