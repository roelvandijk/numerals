{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.DE
    ( cardinal
    , findRule
    , cardinalRepr
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>>=) )
import Data.Function ( const )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from base-unicode-symbols:
import Data.Ord.Unicode ( (≥) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc      ( dec )
import Text.Numeral.Pelletier ( scale )


-------------------------------------------------------------------------------
-- DE
-------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal n = deconstruct findRule n >>= textify cardinalRepr

findRule ∷ (Integral α, Num β) ⇒ FindRule α β
findRule = mkFindRule rules (scale 3 R L)

rules ∷ (Integral α, Num β) ⇒ Rules α β
rules = [ ((  0,  12), atom)
        , (( 13,  19), add 10 L)
        , (( 20,  99), mul 10 L L)
        , ((100, 100), atom1)
        , ((101, 199), add 100 R)
        , ((200, 999), mul1 100 R L)
        ]

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd   = (⊞)
         , reprMul   = \_ _ → ""
         , reprNeg   = "minus "
         }
    where
      _ ⊞ (_ :*: C 10) = "und"
      _ ⊞ _            = ""

      symMap = M.fromList
               [ (0, const "null")
               , (1, \c → case c of
                            LA {} → "ein"
                            LM (C n) _ | n ≥ dec 6 → "eine"
                                       | n ≥ 100   → "ein"
                            _ → "eins"
                 )
               , (2, \c → case c of
                            LM (C 10) _ → "zwan"
                            _           → "zwei"
                 )
               , (3, const "drei")
               , (4, const "vier")
               , (5, const "fünf")
               , (6, \c → case c of
                            LA (C 10) _ → "sech"
                            LM (C 10) _ → "sech"
                            _           → "sechs"
                 )
               , (7, \c → case c of
                            LA (C 10) _ → "sieb"
                            LM (C 10) _ → "sieb"
                            _           → "sieben"
                 )
               , (8, const "acht")
               , (9, const "neun")
               , (10, \c → case c of
                             RM (C 3) _ → "ßig"
                             RM (C _) _ → "zig"
                             _          → "zehn"
                 )
               , (11, const "elf")
               , (12, const "zwölf")
               , (100, const "hundert")
               , (1000, const "tausend")
               ]
