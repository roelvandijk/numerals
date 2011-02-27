{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        de

[@ISO639-2B@]       ger

[@ISO639-3@]        deu

[@Native name@]     Deutsch

[@English name@]    German

[@French name@]     allemand

[@Spanish name@]    alemán

[@Chinese name@]    德语

[@Russian name@]    немецкий

[@German name@]     Deutsch

[@Language family@] Indo-European,
                    Germanic,
                    West Germanic,
                    High German,
                    German

[@Scope@]           Individual language

[@Type@]            Living
-}

module Text.Numeral.Language.DE
    ( cardinal
    , rule
    , cardinalRepr
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Function ( const )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from base-unicode-symbols:
import Data.Ord.Unicode ( (≥) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )


-------------------------------------------------------------------------------
-- DE
-------------------------------------------------------------------------------
-- scale 3 R L

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = mkCardinal rule cardinalRepr

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule (   0, atom        )
              [ (  13, add   10   L)
              , (  20, mul   10 L L)
              , ( 100, atom1       )
              , ( 101, add  100 R  )
              , ( 200, mul1 100 R L)
              , (1000, atom        )
              , (1001, add 1000 R  )
              , (2000, mul 1000 R L)
              ]
               999999

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
cardinalRepr = defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = (⊞)
               , reprMul   = \_ _ → Just ""
               , reprSub   = \_ _ → Just ""
               , reprNeg   = \_   → Just "minus "
               }
    where
      _ ⊞ (_ :*: C 10) = Just "und"
      _ ⊞ _            = Just ""

      symMap = M.fromList
               [ (0, const "null")
               , (1, \c → case c of
                            AddL {} → "ein"
                            MulL (C n) _ | n ≥ dec 6 → "eine"
                                         | n ≥ 100   → "ein"
                            _ → "eins"
                 )
               , (2, \c → case c of
                            MulL (C 10) _ → "zwan"
                            _             → "zwei"
                 )
               , (3, const "drei")
               , (4, const "vier")
               , (5, const "fünf")
               , (6, \c → case c of
                            AddL (C 10) _ → "sech"
                            MulL (C 10) _ → "sech"
                            _             → "sechs"
                 )
               , (7, \c → case c of
                            AddL (C 10) _ → "sieb"
                            MulL (C 10) _ → "sieb"
                            _             → "sieben"
                 )
               , (8, const "acht")
               , (9, const "neun")
               , (10, \c → case c of
                             MulR (C 3) _ → "ßig"
                             MulR (C _) _ → "zig"
                             _            → "zehn"
                 )
               , (11, const "elf")
               , (12, const "zwölf")
               , (100, const "hundert")
               , (1000, const "tausend")
               ]
