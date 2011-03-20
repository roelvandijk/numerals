{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        gv

[@ISO639-2@]        glv

[@ISO639-3@]        glv

[@Native name@]     Gaelg

[@English name@]    Manx
-}

module Text.Numeral.Language.GV
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
-- GV
-------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/manx.html
  http://www.gaelg.iofm.net/LESSONS/P/P19.html
  http://www.gaelg.iofm.net/LESSONS/mona/Lessons.pdf
-}

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = checkPos
       $ fix
       $ findRule (   1, lit        )
                [ (  11, add  10 L  )
                , (  20, lit        )
                , (  21, add  20 L  )
                , (  40, lit        )
                , (  41, add  40 L  )
                , (  60, mul  20 R L)
                , ( 100, lit        )
                , ( 101, add 100 R  )
                , ( 200, mul 100 R L)
                , (1000, lit        )
                ]
                   1000

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = (⊞)
               , reprMul   = \_ _ → Just " "
               }
    where
      (Lit _ `Mul` Lit 20) ⊞ Lit 10 = Just " as "
      _                    ⊞ Lit 10 = Just "-"
      _                    ⊞ _      = Just " as "

      symMap = M.fromList
               [ (1, const "nane")
               , (2, \c → case c of
                            CtxAddL (Lit 10) _ → "daa"
                            CtxMulL {}         → "daa"
                            _                  → "jees"
                 )
               , (3, const "tree")
               , (4, const "kiare")
               , (5, const "queig")
               , (6, const "shey")
               , (7, const "shiaght")
               , (8, const "hoght")
               , (9, const "nuy")
               , (10, \c → case c of
                             CtxAddR (Lit 2)              _ → "yeig"
                             CtxAddR (Lit _ `Mul` Lit 20) _ → "jeih"
                             CtxAddR {}                     → "jeig"
                             _                              → "jeih"
                 )
               , (20, const "feed")
               , (40, const "daeed")
               , (100, \c → case c of
                              CtxMulR {} → "cheead"
                              _          → "keead"
                 )
               , (1000, const "thousane")
               ]
