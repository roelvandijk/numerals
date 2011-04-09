{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        wo

[@ISO639-2@]        wo1

[@ISO639-3@]        wo1

[@Native name@]     Wolof

[@English name@]    Wolof
-}

module Text.Numeral.Language.WO
    ( cardinal
    , struct
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

{-
Sources:
  http://en.wikipedia.org/wiki/Wolof_language#Numerals
  http://www.sf.airnet.ne.jp/~ts/language/number/wolof.html
-}

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )
import Data.Ord.Unicode    ( (≥) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- WO
--------------------------------------------------------------------------------

cardinal ∷ (Integral α, C.Scale α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = checkPos
       $ fix
       $ findRule (  0, lit        )
                [ (  6, add 5 R    )
                , ( 10, lit        )
                , ( 11, add 10 R   )
                , ( 20, mul 10 R L )
                , (100,  step 100 10 R L)
                , (1000, step 1000 1000 R L)
                , (dec 6, lit)
                ]
                  (dec 6)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               }
    where
      (Lit 5 ⊞ _) _ = "-"
      (_     ⊞ _) _ = " ak "

      (_ ⊡ Lit 10) _ = "-"
      (_ ⊡ _     ) _ = " "

      syms =
          M.fromList
          [ (0, const "tus")
          , (1, i "benn")
          , (2, i "ñaar")
          , (3, i "ñett")
          , (4, i "ñeent")
          , (5, i "juróom")
          , (10, i "fukk")
          , (100, i "téeméer")
          , (1000, const "junni")
          , (dec 6, const "tamndareet")
          ]

      i s = \c → s ⊕ case c of
                       CtxMul _ (Lit n) _ | n ≥ 100 → "i"
                       CtxAdd R _ (CtxMul _ (Lit n) _) | n ≥ 100 → "i"
                       _ → ""
