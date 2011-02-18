{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Pelletier
    ( scale
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool ( Bool(True) )
import Prelude   ( Integral )

-- from base-unicode-symbols:
import Prelude.Unicode ( (⋅) )

-- from numerals:
import Text.Numeral ( AddType, Rule(Rule), RuleType(Mul) )
import Text.Numeral.Misc ( dec )


--------------------------------------------------------------------------------
-- Pelletier
--------------------------------------------------------------------------------

scale ∷ (Integral i) ⇒ AddType → i → [Rule i]
scale a g = [Rule Mul (dec n) (dec n) (dec g) a True | n ← [g, 2⋅g ..]]


{-
    ( longScale
    , longScalePlural
    , shortScale
    , shortScalePlural
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- base
import Control.Monad ( fmap )
import Data.Bool     ( otherwise )
import Data.Function ( ($), const, flip )
import Data.List     ( map )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String
import Data.Tuple    ( snd )
import Prelude       ( (^), Integer, error )

-- base-unicode-symbols
import Data.Function.Unicode ( (∘) )
import Data.List.Unicode     ( (∈) )
import Prelude.Unicode       ( (⋅) )

-- numerals
import Text.Numeral
import Text.Numeral.Misc ( d, untilNothing, weave, withSnd )

-- from string-combinators:
import Data.String.Combinators ( (<>) )


-------------------------------------------------------------------------------
-- Short and long scale
-------------------------------------------------------------------------------

longScale ∷ (Monoid s, IsString s) ⇒ s → s → [NumSymbol s]
longScale a b = longScalePlural a a b b

longScalePlural ∷ (Monoid s, IsString s) ⇒ s → s → s → s → [NumSymbol s]
longScalePlural a as b bs = untilNothing $ weave (map illion  [1..])
                                                 (map illiard [1..])
    where illion  n = genScale (d 6 ^ n)       a as n
          illiard n = genScale (d 6 ^ n ⋅ d 3) b bs n

          genScale p x y n = fmap (\s → mul p $ mulForms (s <> x) (s <> y))
                                  $ bigCardinal n

shortScale ∷ (Monoid s, IsString s) ⇒ s → [NumSymbol s]
shortScale a = shortScalePlural a a

shortScalePlural ∷ (Monoid s, IsString s) ⇒ s → s → [NumSymbol s]
shortScalePlural a as = untilNothing $ map illion [1..]
    where illion n = fmap ( \s → mul (d 3 ⋅ (d 3 ^ n))
                                 $ mulForms (s <> a) (s <> as)
                          )
                          $ bigCardinal n

bigCardinal ∷ (Monoid s, IsString s) ⇒ Integer → Maybe s
bigCardinal = cardinal bigNum Masculine


-------------------------------------------------------------------------------
-- Big Num 'language'
-------------------------------------------------------------------------------

bigNum ∷ (Monoid s, IsString s) ⇒ NumConfig s
bigNum = NumConfig { ncNeg      = error "bigNumNeg: undefined"
                   , ncOne      = snd
                   , ncAdd      = withSnd ∘ flip $ (<>)
                   , ncMul      = withSnd (<>)
                   , ncCardinal = findSym bigNumTable
                   }

bigNumTable ∷ (Monoid s, IsString s) ⇒ [NumSymbol s]
bigNumTable =
    [ term 0     $ const "nulla"
    , term 1     $ forms "m"     "un"       "un"       "mi"      "mi"
    , term 2     $ forms "b"     "duo"      "duo"      "vi"      "du"
    , term 3     $ forms "tr"    "tre"      "tres"     "tri"     "tre"
    , term 4     $ forms "quadr" "quattuor" "quattuor" "quadra"  "quadrin"
    , term 5     $ forms "quint" "quin"     "quinqua"  "quinqua" "quin"
    , term 6     $ forms "sext"  "sex"      "ses"      "sexa"    "ses"
    , term 7     $ forms "sept"  "septen"   "septem"   "septua"  "septin"
    , term 8     $ forms "oct"   "octo"     "octo"     "octo"    "octin"
    , term 9     $ forms "non"   "novem"    "novem"    "nona"    "non"
    , mul  10    $ \ctx → case ctx of
                            RM {} → "gint"
                            _     → "dec"
    , mul  100   $ \ctx → case ctx of
                            RM n _ | n ∈ [2, 3, 6] → "cent"
                                   | otherwise     → "gent"
                            _                      → "cent"
    , mul  1000  $ const "mill"
    , mul  10000 $ const "myr"
    ]
    where forms t a1 a2 m1 m2 ctx = case ctx of
                                      RA 10  _ → a1
                                      RA _   _ → a2
                                      LM 100 _ → m2
                                      LM _   _ → m1
                                      _        → t

-}
