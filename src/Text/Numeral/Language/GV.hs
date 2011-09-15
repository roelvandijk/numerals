{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

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
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "numerals-base" Text.Numeral
import qualified "numerals-base" Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- GV
-------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/manx.html
  http://www.gaelg.iofm.net/LESSONS/P/P19.html
  http://www.gaelg.iofm.net/LESSONS/mona/Lessons.pdf
-}

cardinal ∷ (Integral α, Monoid s, IsString s) ⇒ α → Maybe s
cardinal = cardinalRepr ∘ struct

struct ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β) ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (   1, lit       )
                [ (  11, add 10 L  )
                , (  20, lit       )
                , (  21, add 20 L  )
                , (  40, lit       )
                , (  41, add 40 L  )
                , (  60, mul 20 R L)
                , ( 100, step 100 10 R L)
                , (1000, lit)
                ]
                   1000

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just $ \_ _ _ → " "
               }
    where
      ((Lit _ `Mul` Lit 20) ⊞ Lit 10) _ = " as "
      (_                    ⊞ Lit 10) _ = "-"
      (_                    ⊞ _     ) _ = " as "

      syms =
          M.fromList
          [ (1, const "nane")
          , (2, \c → case c of
                       CtxAdd _ (Lit 10) _ → "daa"
                       CtxMul {}           → "daa"
                       _                   → "jees"
            )
          , (3, const "tree")
          , (4, const "kiare")
          , (5, const "queig")
          , (6, const "shey")
          , (7, const "shiaght")
          , (8, const "hoght")
          , (9, const "nuy")
          , (10, \c → case c of
                        CtxAdd _ (Lit 2)              _ → "yeig"
                        CtxAdd _ (Lit _ `Mul` Lit 20) _ → "jeih"
                        CtxAdd R _                    _ → "jeig"
                        _                               → "jeih"
            )
          , (20, const "feed")
          , (40, const "daeed")
          , (100, \c → case c of
                         CtxMul {} → "cheead"
                         _         → "keead"
            )
          , (1000, const "thousane")
          ]
