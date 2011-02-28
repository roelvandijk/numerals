{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        fr

[@ISO639-2B@]       fre

[@ISO639-3@]        fra

[@Native name@]     Français

[@English name@]    French

[@French name@]     Français

[@Spanish name@]    Francés

[@Chinese name@]    法语

[@Russian name@]    французский

[@German name@]     Französisch

[@Language family@] Indo-European,
                    Italic,
                    Romance,
                    Italo-Western,
                    Western Romance,
                    Gallo-Iberian,
                    Gallo-Romance,
                    Gallo-Rhaetian,
                    Oïl,
                    French

[@Scope@]           Individual language

[@Type@]            Living
-}

module Text.Numeral.Language.FR
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
import Data.Function ( const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral


--------------------------------------------------------------------------------
-- FR
--------------------------------------------------------------------------------

{-
Sources:
  http://www.sf.airnet.ne.jp/~ts/language/number/french.html
  http://www.french-linguistics.co.uk/tutorials/numbers/
-}

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, Num β) ⇒ α → Maybe β
struct = positive (fix rule)

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule (0, atom)
              [ ( 11, add  10 L  )
              , ( 17, add  10 R  )
              , ( 20, atom       )
              , ( 21, add  20 R  )
              , ( 30, mul  10 R L)
              , ( 70, add  60 R  )
              , ( 80, mul  20 R L)
              , ( 89, add  80 R  )
              , (100, atom       )
              , (101, add 100 R  )
              , (200, mul 100 R L)
              ]
                 1000

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = (⊞)
               , reprMul   = (⊡)
               , reprNeg   = \_ → Just "moins "
               }
    where
      C _  ⊞ C 10           = Just ""
      _    ⊞ C 10           = Just "-"
      (C 4 :*: C 20) ⊞ _    = Just "-"
      _    ⊞ C 1            = Just " et "
      _    ⊞ (C 1 :+: C 10) = Just " et "
      C 10 ⊞ _              = Just "-"
      _    ⊞ _              = Just "-"

      _ ⊡ 20 = Just "-"
      _ ⊡ _  = Just ""

      symMap = M.fromList
               [ (0, const "zéro")
               , (1, ten   "un"     "on"     "un")
               , (2, ten   "deux"   "dou"    "deux")
               , (3, ten   "trois"  "trei"   "tren")
               , (4, ten   "quatre" "quator" "quar")
               , (5, ten   "cinq"   "quin"   "cinqu")
               , (6, ten   "six"    "sei"    "soix")
               , (7, const "sept")
               , (8, const "huit")
               , (9, const "neuf")
               , (10, \c → case c of
                             AddR (C n) _ | n < 7     → "ze"
                                          | otherwise → "dix"
                             MulR (C 3) _ → "te"
                             MulR {}      → "ante"
                             _            → "dix"
                 )
               , (20,   \c → case c of
                               MulR _ Empty → "vingts"
                               _            → "vingt"
                 )
               , (100,  const "cent")
               , (1000, const "mille")
               ]

      ten n a m ctx = case ctx of
                        AddL (C 10) _ → a
                        MulL (C 10) _ → m
                        _             → n
