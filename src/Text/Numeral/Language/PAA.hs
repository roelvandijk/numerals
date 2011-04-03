{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        -

[@ISO639-2B@]       paa

[@ISO639-3@]        hui

[@Native name@]     -

[@English name@]    Huli
-}

module Text.Numeral.Language.PAA
    ( cardinal
    , struct
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
import Prelude       ( Integral )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import qualified Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- PAA
--------------------------------------------------------------------------------

{-
TODO:
Need new Exp constructor to express
42 = (15 × 2) + (12 obj. of the 3rd 15) = ngui ki, ngui tebone-gonaga hombearia

Probably also need a constructor to express "4 obj" as opposed to just "4".
-}

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, C.Lit β, C.Add β, C.Mul β) ⇒ α → Maybe β
struct = checkPos
       $ fix
       $ findRule ( 1, lit       )
                [ (16, add 15 R  )
                , (30, mul 15 R R)
                , (31, add 30 R  )
                ]
                  100

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               }
    where
      -- _ ⊞ _ = ", ngui "
      _ ⊞ _ = "-ni "

      _ ⊡ _ = " "

      syms =
          M.fromList
          [ ( 1, const "mbira")
          , ( 2, \c → case c of
                        CtxMul {} → "ki"
                        _         → "kira"
            )
          , ( 3, const "tebira")
          , ( 4, const "maria")
          , ( 5, const "duria")
          , ( 6, const "waragaria")
          , ( 7, const "karia")
          , ( 8, const "halira")
          , ( 9, const "dira")
          , (10, const "pira")
          , (11, const "bearia")
          , (12, const "hombearia")
          , (13, const "haleria")
          , (14, const "deria")
          , (15, \c → case c of
                        CtxMul {} → "ngui"
                        _         → "nguira"
            )
          ]
