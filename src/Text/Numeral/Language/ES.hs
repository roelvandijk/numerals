{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        es

[@ISO639-2B@]       spa

[@ISO639-3@]        spa

[@Native name@]     Español

[@English name@]    Spanish

[@French name@]     Espagnole

[@Spanish name@]    Español

[@Chinese name@]    西班牙语

[@Russian name@]    Испанский

[@German name@]     Spanisch

[@Language family@] Indo-European -
                    Italic -
                    Romance -
                    Italo-Western -
                    Gallo-Iberian -
                    Ibero-Romance -
                    West Iberian -
                    Spanish, Castilian

[@Scope@]           Individual

[@Type@]            Living
-}

module Text.Numeral.Language.ES
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
import Data.Functor  ( (<$>) )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<) )
import Data.String   ( IsString )
import Prelude       ( Integral, Num, (-) )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.Language.BigNum as BN ( rule, cardinalRepr )


-------------------------------------------------------------------------------
-- ES
-------------------------------------------------------------------------------

-- Sources:
--   http://www.sf.airnet.ne.jp/~ts/language/number/spanish.html
--   http://spanish.about.com/cs/forbeginners/a/cardinalnum_beg.htm
--   http://www.learn-spanish-help.com/count-in-spanish.html
--   http://www.donquijote.org/spanishlanguage/numbers/numbers1.asp
--   http://en.wiktionary.org/wiki/Appendix:Spanish_numerals

cardinal ∷ (Monoid s, IsString s, Integral α, Scale α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, Scale α, Num β, Scale β) ⇒ α → Maybe β
struct = positive (fix $ rule `combine` longScale R L BN.rule)

rule  ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule (   0, atom        )
              [ (  11, add   10 L  )
              , (  16, add   10 R  )
              , (  20, atom        )
              , (  21, add   20 R  )
              , (  30, mul   10 R L)
              , ( 100, atom        )
              , ( 101, add  100 R  )
              , ( 200, mul  100 R L)
              , (1000, atom        )
              , (1001, add 1000 R  )
              , (2000, mul 1000 R L)
              ]
              (dec 6 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprScale = longScaleRepr
               , reprAdd   = (⊞)
               , reprMul   = (⊡)
               }
    where
      longScaleRepr _ _ e c = let s = case c of
                                        MulR {} → "illones"
                                        _       → "illón"
                              in (⊕ s) <$> textify BN.cardinalRepr e

      _              ⊞ C 10 = Just ""
      C 10           ⊞ _    = Just ""
      C 20           ⊞ _    = Just ""
      (C _ :*: C 10) ⊞ _    = Just " y "
      _              ⊞ _    = Just " "

      _ ⊡ C n | n < 1000 = Just ""
      _ ⊡ _              = Just " "

      symMap = M.fromList
               [ (0, const "cero")
               , (1, \c → case c of
                            AddL (C 10)  _ → "on"
                            _              → "uno"
                 )
               , (2, \c → case c of
                            AddL (C 10)  _ → "do"
                            AddR (C 20)  _ → "dós"
                            _              → "dos"
                 )
               , (3, \c → case c of
                            AddL (C 10)  _ → "tre"
                            AddR (C 20)  _ → "trés"
                            MulL (C 10)  _ → "trein"
                            _              → "tres"
                 )
               , (4, \c → case c of
                            AddL (C 10)  _ → "cator"
                            MulL (C 10)  _ → "cuaren"
                            _              → "cuatro"
                 )
               , (5, \c → case c of
                            AddL (C 10)  _ → "quin"
                            MulL (C 10)  _ → "cincuen"
                            MulL (C 100) _ → "quin"
                            _              → "cinco"
                 )
               , (6, \c → case c of
                            AddR (C 10)  _ → "séis"
                            AddR (C 20)  _ → "séis"
                            MulL (C 10)  _ → "sesen"
                            _              → "seis"
                 )
               , (7, \c → case c of
                            MulL (C 10)  _ → "seten"
                            MulL (C 100) _ → "sete"
                            _              → "siete"
                 )
               , (8, \c → case c of
                            MulL (C 10)  _ → "ochen"
                            _              → "ocho"
                 )
               , (9, \c → case c of
                            MulL (C 10)  _ → "noven"
                            MulL (C 100) _ → "nove"
                            _              → "nueve"
                 )
               , (10, \c → case c of
                             AddR (C _)  _ → "ce"
                             AddL (C _)  _ → "dieci"
                             MulR {}       → "ta"
                             _             → "diez"
                 )
               , (20, \c → case c of
                             AddL (C _)  _ → "veinti"
                             _             → "veinte"
                 )
               , (100, \c → case c of
                              Empty        → "cien"
                              AddL {}      → "ciento"
                              MulR (C 5) _ → "ientos"
                              MulL {}      → "cien"
                              _            → "cientos"
                 )
               , (1000, const "mil")
               ]
