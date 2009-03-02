-- -*- coding: utf-8 -*-

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Numerals where


{-
TODO

Types of numerals

  * Cardinal numerals: how many items - one, two, three

  * Ordinal numerals: position - first, second, third.
      en   1 = first
      en   2 = second
      en  32 = thirty-second
      nl   8 = achtste
      nl   9 = negende
      nl  89 = negenentachtigste

  * Partitive numerals: expresses a fraction - half, third, quarter.
      en 1%2 = half
      en 2%3 = two thirds
      nl 2%3 = twee derden
      nl 3%4 = drie kwart

  * Decimals == fractions of powers of tens
      en 0.7   = seven-tenths
      en 0.065 = sixty-five thousanths
      nl 0.28  = achtentwintig honderdsten

  * Multiplicative numerals: how many times - once, twice, thrice.
      en   1 = once
      en   2 = twice
      en   3 = thrice
      en [4..] = undefined - or use a convention like "four times,
                 five times, etc."

  * Distributive numerals: expresses a group of the number specified:
    In pairs, by the dozen. English does not have distributive
    numerals for these but other languages such as Georgian do.
      en   2 = pair
      en  12 = dozen
      nl 144 = gros

Languages (which should not be too hard)

  * West Germanic
    - Dutch
    - German
    - English

  * North Germanic
    - Norwegian
    - Danish
    - Swedish

  * Romance
    - French
    - Italian
    - Spanish
    - Latin

Other counting bases
  * duodecimal
    12^1 = dozen
    12^2 = gross
    12^3 = great gross

Automatically generate a list of symbol names using latin.
  short scale: 10 ^ (3 * n + 3)
  long  scale: 10 ^ (6 * n)
  number scale n == latin n ++ "illion"
  Should work for at least English, Dutch and French.


Expose deeper structure of numerals. It would be nice if the algorithm
could exploit the deeper structure of numerals in various
languages. But even a relatively complex language like French can be
described in less than 30 entries in a symbol table. So it is probably
easier to just use the symbol table in its present form.

Dutch
  1:  een / e
  2:  twee / twaa / twin
  3:  drie / der
  4:  vier / veer
  8:  acht / tach
  10: tien / lf / tig

English
  2:  two / twen
  3:  three / thir
  4:  four / for
  5:  five / fif
  8:  eight / eigh
  10: ten / teen / ty

French
  1:  un / on
  2:  deux / dou / ving
  3:  trois / trei / tren
  4:  quatre / quator / quaran
  5:  cinq / quin / cinquan
  6:  six / sei / soixan
  10: dix / ze / t / te

Negative numbers:

nl: min
en: minus
fr: moins
de: minus
sp:
it:
se:
no:
eo:
la:
ja: mainasu


-}

import Data.List
import qualified Data.DString as DS
import Data.String
import Data.Monoid

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data SymbolType = Terminal | Add | Mul deriving Show

data NumSymbol = NumSym { symType  :: SymbolType
                        , symVal   :: Integer
                        , symScope :: Integer
                        , symStr   :: String
                        } deriving Show

-- Easy construction of NumSymbols
term :: Integer -> String -> NumSymbol
term v = NumSym Terminal v 1

add :: Integer -> Integer -> String -> NumSymbol
add = NumSym Add

mul :: Integer -> String -> NumSymbol
mul v = NumSym Mul v v

-- Scope is given as a size. So a scope of 10 includes the value of the symbol itself.
-- NumSym 10 10 10 "ten"
-- "ten" is in the additive scope for [10 .. 10 + 10 - 1]
-- "ten" is in the multiplicative scope for [10 .. 10 * 10 - 1]

data NumConfig = forall s. (IsString s, Stringable s) =>
                 NumConfig { ncCardinal :: Integer -> Maybe NumSymbol
                           , ncNeg      :: s -> s
                           , ncOne      :: (Integer, s) -> s
                           , ncAdd      :: (Integer, s) -> (Integer, s) -> s
                           , ncMul      :: (Integer, s) -> (Integer, s) -> s
                           }

-- TODO: Better names:
type NegCombinator = DS.DString -> DS.DString
type OneCombinator = (Integer, DS.DString) -> DS.DString
type Combinator    = (Integer, DS.DString) -> (Integer, DS.DString) -> DS.DString

(<>) :: Monoid s => s -> s -> s
(<>) = mappend

(<+>), (<->) :: (Monoid s, IsString s) => s -> s -> s

x <+> y = x <> " " <> y
x <-> y = x <> "-" <> y

infixr 5 <>, <+>, <-> -- Same as ++

class Stringable s where
    toString :: s -> String

instance Stringable String where
    toString = id

instance Stringable DS.DString where
    toString = DS.toString

d :: Integer -> Integer
d = (10 ^)

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

cardinal :: NumConfig -> Integer -> Maybe String
cardinal NumConfig {..} 0 = fmap symStr $ ncCardinal 0
cardinal NumConfig {..} x | x < 0     = fmap (toString . ncNeg) $ go $ abs x
                          | otherwise = fmap toString $ go x
    where go n = do (NumSym _ v _ v') <- ncCardinal n
                    let vs = fromString v'
                    case n `divMod` v of
                      (1, 0) -> return $ ncOne (v, vs)
                      (1, r) -> do rs <- go r
                                   return $ (v, ncOne (v, vs)) `ncAdd` (r, rs)
                      (q, r) | q >= v    -> Nothing
                             | otherwise -> do qs <- go q
                                               if r == 0
                                                 then return $ (q, qs) `ncMul` (v, vs)
                                                 else do rs <- go r
                                                         return $ (q*v, (q, qs) `ncMul` (v, vs)) `ncAdd` (r, rs)

findSym :: [NumSymbol] -> Integer -> Maybe NumSymbol
findSym []     _ = Nothing
findSym (e:es) n = go e e es
    where go :: NumSymbol -> NumSymbol -> [NumSymbol] -> Maybe NumSymbol
          go a m [] = stop a m
          go a m (x@(NumSym t v _ _) : xs)
              | v == n    = Just x
              | otherwise = case t of
                              Terminal        -> go a m xs
                              Add | v > n     -> stop a m
                                  | otherwise -> go x m xs
                              Mul | v > n     -> stop a m
                                  | otherwise -> go x x xs

          stop :: NumSymbol -> NumSymbol -> Maybe NumSymbol
          stop a@(NumSym {..}) m | n < symVal + symScope = return a
                                 | otherwise             = return m

-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------

-- A047357
-- Numbers that are congruent to {0, 1, 3} mod 7
testNums :: Integer -> [Integer]
testNums 0 = 0 : testNums 1
testNums n = n : testNums (1 + n + n `mod` 7)

test :: NumConfig -> [Integer] -> IO ()
test nc = mapM_ (putStrLn . pretty)
    where pretty n = show n ++ " == " ++ (maybe "-" id $ cardinal nc n)

testSome :: NumConfig -> Integer -> Integer -> IO ()
testSome nc start amount = test nc . genericTake amount . testNums $ start

-------------------------------------------------------------------------------
-- Numeral configurations
-------------------------------------------------------------------------------

nlNeg :: NegCombinator
nlNeg s = "min" <+> s

nlOne :: OneCombinator
nlOne = snd

nlAdd :: Combinator
nlAdd (x, x') (y, y') | x < 20    = y' <> x'
                      | x < 100   = y' <> (if y == 2 || y == 3
                                           then "ën"
                                           else "en")
                                       <> x'
                      | otherwise = x' <+> y'

nlMul :: Combinator
nlMul (_, x') (_, y') = x' <+> y'

nlTable :: [NumSymbol]
nlTable = [ term 0          "nul"

          , term 1          "één"
          , term 2          "twee"
          , term 3          "drie"
          , term 4          "vier"
          , term 5          "vijf"
          , term 6          "zes"
          , term 7          "zeven"
          , term 8          "acht"
          , term 9          "negen"

          , mul  10         "tien"

          , term 11         "elf"
          , term 12         "twaalf"
          , term 13         "dertien"
          , term 14         "veertien"

          , add  20  10     "twintig"
          , add  30  10     "dertig"
          , add  40  10     "veertig"
          , add  50  10     "vijftig"
          , add  60  10     "zestig"
          , add  70  10     "zeventig"
          , add  80  10     "tachtig"
          , add  90  10     "negentig"

          , mul  (d 2)      "honderd"

          , mul  (d 3)      "duizend"

          , mul  (d 6)      "miljoen"              -- m 1         where m = (d 6 ^)
          , mul  (d 9)      "miljard"              -- m 1  * d 3
          , mul  (d 12)     "biljoen"              -- m 2
          , mul  (d 15)     "biljard"              -- m 2  * d 3
          , mul  (d 18)     "triljoen"             -- m 3
          , mul  (d 21)     "triljard"             -- m 3  * d 3
          , mul  (d 24)     "quadriljoen"          -- m 4
          , mul  (d 27)     "quadriljard"          -- m 4  * d 3
          , mul  (d 30)     "quintiljoen"          -- m 5
          , mul  (d 33)     "quintiljard"          -- m 5  * d 3
          , mul  (d 36)     "sextiljoen"           -- m 6
          , mul  (d 39)     "sextiljard"           -- m 6  * d 3
          , mul  (d 42)     "septiljoen"           -- m 7
          , mul  (d 45)     "septiljard"           -- m 7  * d 3
          , mul  (d 48)     "octiljoen"            -- m 8
          , mul  (d 51)     "octiljard"            -- m 8  * d 3
          , mul  (d 54)     "noniljoen"            -- m 9
          , mul  (d 57)     "noniljard"            -- m 9  * d 3

          , mul  (d 60)     "deciljoen"            -- m 10
          , mul  (d 63)     "deciljard"            -- m 10  * d 3

          , mul  (d 66)     "undeciljoen"          -- m 11
          , mul  (d 69)     "undeciljard"          -- m 11  * d 3
          , mul  (d 72)     "duodeciljoen"         -- m 12
          , mul  (d 75)     "duodeciljard"         -- m 12  * d 3
          , mul  (d 78)     "tredeciljoen"         -- m 13
          , mul  (d 81)     "tredeciljard"         -- m 13  * d 3
          , mul  (d 84)     "quattuordeciljoen"    -- m 14
          , mul  (d 87)     "quattuordeciljard"    -- m 14  * d 3
          , mul  (d 90)     "quindeciljoen"        -- m 15
          , mul  (d 93)     "quindeciljard"        -- m 15  * d 3
          , mul  (d 96)     "sexdeciljoen"         -- m 16
          , mul  (d 99)     "sexdeciljard"         -- m 16  * d 3
          , mul  (d 102)    "septendeciljoen"      -- m 17
          , mul  (d 105)    "septendeciljard"      -- m 17  * d 3
          , mul  (d 108)    "octodeciljoen"        -- m 18
          , mul  (d 111)    "octodeciljard"        -- m 18  * d 3
          , mul  (d 114)    "novemdeciljoen"       -- m 19
          , mul  (d 117)    "novemdeciljard"       -- m 19  * d 3

          , mul  (d 120)    "vigintiljoen"         -- m 20
          , mul  (d 123)    "vigintiljard"         -- m 20  * d 3
          , mul  (d 180)    "trigintiljoen"        -- m 30
          , mul  (d 183)    "trigintiljard"        -- m 30  * d 3
          , mul  (d 240)    "quadragintiljoen"     -- m 40
          , mul  (d 243)    "quadragintiljard"     -- m 40  * d 3
          , mul  (d 300)    "quindragintiljoen"    -- m 50
          , mul  (d 303)    "quindragintiljard"    -- m 50  * d 3
          , mul  (d 360)    "sexagintiljoen"       -- m 60
          , mul  (d 363)    "sexagintiljard"       -- m 60  * d 3
          , mul  (d 420)    "septuagintiljoen"     -- m 70
          , mul  (d 423)    "septuagintiljard"     -- m 70  * d 3
          , mul  (d 480)    "octogintiljoen"       -- m 80
          , mul  (d 483)    "octogintiljard"       -- m 80  * d 3
          , mul  (d 540)    "nonagintiljoen"       -- m 90
          , mul  (d 543)    "nonagintiljard"       -- m 90  * d 3

          , mul (d 600)     "centiljoen"           -- m (d 2)
          , mul (d 603)     "centiljard"           -- m (d 2) * d 3

          , mul (d 6000)    "milliljoen"           -- m (d 3)
          , mul (d 6003)    "milliljard"           -- m (d 3) * d 3
          ]

nl :: NumConfig
nl = NumConfig { ncNeg      = nlNeg
               , ncOne      = nlOne
               , ncAdd      = nlAdd
               , ncMul      = nlMul
               , ncCardinal = findSym nlTable
               }

-------------------------------------------------------------------------------

enNeg :: NegCombinator
enNeg s = "minus" <+> s

enOne :: OneCombinator
enOne (v,  v') | v >= 100  = "one" <+> v'
               | otherwise = v'

enAdd :: Combinator
enAdd (x, x') (_, y') | x < 100   = x' <-> y'
                      | otherwise = x' <+> y'

enMul :: Combinator
enMul (_, x') (_, y') = x' <+> y'

enTable :: [NumSymbol]
enTable = [ term 0       "zero"
          , term 1       "one"
          , term 2       "two"
          , term 3       "three"
          , term 4       "four"
          , term 5       "five"
          , term 6       "six"
          , term 7       "seven"
          , term 8       "eight"
          , term 9       "nine"
          , mul  10      "ten"
          , term 11      "eleven"
          , term 12      "twelve"
          , term 13      "thirteen"
          , term 14      "fourteen"
          , term 15      "fifteen"
          , term 16      "sixteen"
          , term 17      "seventeen"
          , term 18      "eighteen"
          , term 19      "nineteen"
          , add  20   10 "twenty"
          , add  30   10 "thirty"
          , add  40   10 "forty"
          , add  50   10 "fifty"
          , add  60   10 "sixty"
          , add  70   10 "seventy"
          , add  80   10 "eighty"
          , add  90   10 "ninety"
          , mul  100     "hundred"
          , mul  1000    "thousand"
          ]

enShortTable :: [NumSymbol]
enShortTable = enTable ++
               [ mul (d 6)  "million"
               , mul (d 9)  "billion"
               , mul (d 12) "trillion"
               , mul (d 15) "quadrillion"
               , mul (d 18) "quintillion"
               , mul (d 21) "sextillion"
               , mul (d 24) "septillion"
               , mul (d 27) "octillion"
               , mul (d 30) "nonillion"
               , mul (d 33) "decillion"
               , mul (d 36) "undecillion"
               , mul (d 39) "duodecillion"
               , mul (d 42) "tredecillion"
               , mul (d 45) "quattuordecillion"
               , mul (d 48) "quindecillion"
               , mul (d 51) "sexdecillion"
               , mul (d 54) "septendecillion"
               , mul (d 57) "octodecillion"
               , mul (d 60) "novemdecillion"
               , mul (d 63) "vigintillion"
               ]

enShort :: NumConfig
enShort = NumConfig { ncNeg      = enNeg
                    , ncOne      = enOne
                    , ncAdd      = enAdd
                    , ncMul      = enMul
                    , ncCardinal = findSym enShortTable
                    }

enLongTable :: [NumSymbol]
enLongTable = enTable ++
              [ mul (d 6)   "million"
              , mul (d 9)   "milliard"
              , mul (d 12)  "billion"
              , mul (d 15)  "billiard"
              , mul (d 18)  "trillion"
              , mul (d 21)  "trilliard"
              , mul (d 24)  "quadrillion"
              , mul (d 30)  "quintillion"
              , mul (d 36)  "sextillion"
              , mul (d 42)  "septillion"
              , mul (d 48)  "octillion"
              , mul (d 54)  "nonillion"
              , mul (d 60)  "decillion"
              , mul (d 66)  "undecillion"
              , mul (d 72)  "duodecillion"
              , mul (d 78)  "tredecillion"
              , mul (d 84)  "quattuordecillion"
              , mul (d 90)  "quinquadecillion"
              , mul (d 96)  "sedecillion"
              , mul (d 102) "septendecillion"
              , mul (d 108) "octodecillion"
              , mul (d 114) "novendecillion"
              , mul (d 120) "vigintillion"
              ]

enLong :: NumConfig
enLong = NumConfig { ncNeg      = enNeg
                   , ncOne      = enOne
                   , ncAdd      = enAdd
                   , ncMul      = enMul
                   , ncCardinal = findSym enLongTable
                   }

-------------------------------------------------------------------------------

deNeg :: NegCombinator
deNeg s = "minus" <+> s

deOne :: OneCombinator
deOne (v, v') | v >= (d 6) = "eine" <+> v'
              | v >= 100   = "ein"  <>  v'
              | otherwise  = v'

deAdd :: Combinator
deAdd (x, x') (y, y') | x < 20    = y' <> x'
                      | x < 100   = (if y == 1
                                     then "ein"
                                     else y') <> "und" <> x'
                      | otherwise = x' <> y'

deMul :: Combinator
deMul (_, x') (y, y') | y < (d 6) = x' <> y'
                      | otherwise = x' <> y'

deTable :: [NumSymbol]
deTable = [ term 0        "null"
          , term 1        "eins"
          , term 2        "zwei"
          , term 3        "drei"
          , term 4        "vier"
          , term 5        "fünf"
          , term 6        "sechs"
          , term 7        "sieben"
          , term 8        "acht"
          , term 9        "neun"
          , mul  10       "zehn"
          , term 11       "elf"
          , term 12       "zwölf"
          , term 16       "sechzehn"
          , term 17       "siebzehn"
          , add  20    10 "zwanzig"
          , add  30    10 "dreißig"
          , add  40    10 "vierzig"
          , add  50    10 "fünfzig"
          , add  60    10 "sechzig"
          , add  70    10 "siebzig"
          , add  80    10 "achtzig"
          , add  90    10 "neunzig"
          , mul  100      "hundert"
          , mul  1000     "tausend"
          , mul  (d 6)    "million"
          , mul  (d 9)    "milliarde"
          ]

de :: NumConfig
de = NumConfig { ncNeg      = deNeg
               , ncOne      = deOne
               , ncAdd      = deAdd
               , ncMul      = deMul
               , ncCardinal = findSym deTable
               }

-------------------------------------------------------------------------------

seNeg :: NegCombinator
seNeg = error "seNeg: not defined yet you fool!"

seOne :: OneCombinator
seOne = snd

seAdd :: Combinator
seAdd (_, x') (_, y') = x' <> y'

seMul :: Combinator
seMul (_, x') (_, y') = x' <> y'

seTable :: [NumSymbol]
seTable = [ term 0           "noll"
          , term 1           "ett"
          , term 2           "två"
          , term 3           "tre"
          , term 4           "fyra"
          , term 5           "fem"
          , term 6           "sex"
          , term 7           "sju"
          , term 8           "åtta"
          , term 9           "nio"
          , mul  10          "tio"
          , term 11          "elva"
          , term 12          "tolv"
          , term 13          "fretton"
          , term 14          "fjorton"
          , term 15          "femton"
          , term 16          "sexton"
          , term 17          "sjutton"
          , term 18          "arton"
          , term 19          "nitton"
          , add  20    10    "tjugo"
          , add  30    10    "trettio"
          , add  40    10    "fyrtio"
          , add  50    10    "femtio"
          , add  60    10    "sextio"
          , add  70    10    "sjuttio"
          , add  80    10    "åttio"
          , add  90    10    "nittio"
          , mul  100         "hundra"
          , mul  (d 3)       "tusen"
          , add  (d 6) (d 6) "miljon"
          , mul  (d 6)       "miljoner"
          , add  (d 9) (d 9) "miljard"
          , mul  (d 9)       "miljarder"
          ]

se :: NumConfig
se = NumConfig { ncNeg      = seNeg
               , ncOne      = seOne
               , ncAdd      = seAdd
               , ncMul      = seMul
               , ncCardinal = findSym seTable
               }

-------------------------------------------------------------------------------

noNeg :: NegCombinator
noNeg = error "noNeg: not defined yet you fool!"

noOne :: OneCombinator
noOne (v, v') | v >= (d 6) = "én" <+> v'
              | otherwise  = v'

noAdd :: Combinator
noAdd (x, x') (_, y') | x == 100  = x' <+> "og" <+> y'
                      | otherwise = x' <> y'

noMul :: Combinator
noMul (_, x') (_, y') = x' <> y'

noTable :: [NumSymbol]
noTable = [ term 0           "null"
          , term 1           "én"
          , term 2           "to"
          , term 3           "tre"
          , term 4           "fire"
          , term 5           "fem"
          , term 6           "seks"
          , term 7           "sju"
          , term 8           "åtte"
          , term 9           "ni"
          , mul  10          "ti"
          , term 11          "elleve"
          , term 12          "tolv"
          , term 13          "tretten"
          , term 14          "fjorten"
          , term 15          "femten"
          , term 16          "seksten"
          , term 17          "sytten"
          , term 18          "atten"
          , term 19          "nitten"
          , add  20    10    "tjue"
          , add  30    10    "tretti"
          , add  40    10    "førti"
          , add  50    10    "femti"
          , add  60    10    "seksti"
          , add  70    10    "sytti"
          , add  80    10    "åtti"
          , add  90    10    "nitti"
          , mul  100         "hundre"
          , mul  (d 3)       "tusen"
          , add  (d 6) (d 6) "million"
          , mul  (d 6)       "millioner"
          , add  (d 9) (d 9) "milliard"
          , mul  (d 9)       "milliarder"
          ]

no :: NumConfig
no = NumConfig { ncNeg      = noNeg
               , ncOne      = noOne
               , ncAdd      = noAdd
               , ncMul      = noMul
               , ncCardinal = findSym noTable
               }

-------------------------------------------------------------------------------

laNeg :: NegCombinator
laNeg = error "laNeg: not defined yet you fool!"

laOne :: OneCombinator
laOne = snd

laAdd :: Combinator
laAdd (_, x') (_, y') = x' <+> y'

laMul :: Combinator
laMul (_, x') (_, y') = x' <+> y'

laTable :: [NumSymbol]
laTable = [ term 0         "nulla"
          , term 1         "unus"
          , term 2         "duo"
          , term 3         "tres"
          , term 4         "quattuor"
          , term 5         "quinque"
          , term 6         "sex"
          , term 7         "septem"
          , term 8         "octo"
          , term 9         "novem"
          , mul  10        "decem"
          , term 11        "undecim"
          , term 12        "duodecim"
          , term 13        "tredecim"
          , term 14        "quattuordecim"
          , term 15        "quindecim"
          , term 16        "sedecim"
          , term 17        "septendecim"
          , term 18        "duodeviginti"
          , term 19        "undeviginti"
          , add  20   10   "viginti"
          , term 28        "duodetriginta"
          , term 29        "undetriginta"
          , add  30   10   "triginta"
          , term 38        "duodequadraginta"
          , term 39        "undequadraginta"
          , add  40   10   "quadraginta"
          , term 48        "duodequinquaginta"
          , term 49        "undequinquaginta"
          , add  50   10   "quinquaginta"
          , term 58        "duodesexaginta"
          , term 59        "undesexaginta"
          , add  60   10   "sexaginta"
          , term 68        "duodeseptuaginta"
          , term 69        "undeseptuaginta"
          , add  70   10   "septuaginta"
          , term 78        "duodeoctoginta"
          , term 79        "undeoctoginta"
          , add  80   10   "octoginta"
          , term 88        "duodenonaginta"
          , term 89        "undenonaginta"
          , add  90   10   "nonaginta"
          , term 98        "duodecentum"
          , term 99        "undecentum"
          , mul  100       "centum"
          , add  200  100  "ducenti"
          , add  300  100  "trecenti"
          , add  400  100  "quadrigenti"
          , add  500  100  "quingenti"
          , add  600  100  "sescenti"
          , add  700  100  "septingenti"
          , add  800  100  "octigenti"
          , add  900  100  "nongenti"
          , add  1000 1000 "mille"
          , mul  1000      "millia"
          , term (d 6)     "decies centena milia"
          ]

la :: NumConfig
la = NumConfig { ncNeg      = laNeg
               , ncOne      = laOne
               , ncAdd      = laAdd
               , ncMul      = laMul
               , ncCardinal = findSym laTable
               }

-------------------------------------------------------------------------------

frNeg :: NegCombinator
frNeg s = "moins" <+> s

frOne :: OneCombinator
frOne = snd

frAdd :: Combinator
frAdd (x, x') (y, y') | x < 80 && y == 1 = x' <+> "et" <+> y'
                      | x < 100          = x' <-> y'
                      | otherwise        = x' <+> y'

frMul :: Combinator
frMul (_, x') (_, y') = x' <+> y'

frTable :: [NumSymbol]
frTable = [ term 0         "zéro"
          , term 1         "un"
          , term 2         "deux"
          , term 3         "trois"
          , term 4         "quatre"
          , term 5         "cinq"
          , term 6         "six"
          , term 7         "sept"
          , term 8         "huit"
          , term 9         "neuf"
          , mul  10        "dix"
          , term 11        "onze"
          , term 12        "douze"
          , term 13        "treize"
          , term 14        "quatorze"
          , term 15        "quinze"
          , term 16        "seize"
          , add  20    10  "vingt"
          , add  30    10  "trente"
          , add  40    10  "quarante"
          , add  50    10  "cinquante"
          , add  60    10  "soixante"
          , term 71        "soixante et onze"
          , term 80        "quatre-vingts"
          , add  80    10  "quatre-vingt"
          , add  100   100 "cent"
          , mul  100       "cents"
          , mul  1000      "mille"
          , mul  (d 6)     "million"
          , mul  (d 9)     "millard"
          ]

fr :: NumConfig
fr = NumConfig { ncNeg      = frNeg
               , ncOne      = frOne
               , ncAdd      = frAdd
               , ncMul      = frMul
               , ncCardinal = findSym frTable
               }

-------------------------------------------------------------------------------

spNeg :: NegCombinator
spNeg = error "spNeg: not defined yet you fool!"

spOne :: OneCombinator
spOne = snd

spAdd :: Combinator
spAdd (x, x') (_, y') | x < 100   =  x' <+> "y" <+> y'
                      | otherwise  = x' <+> y'

spMul :: Combinator
spMul (_, x') (_, y') = x' <+> y'

spTable :: [NumSymbol]
spTable = [ term 0         "cero"
          , term 1         "uno"
          , term 2         "dos"
          , term 3         "tres"
          , term 4         "cuatro"
          , term 5         "cinco"
          , term 6         "seis"
          , term 7         "siete"
          , term 8         "ocho"
          , term 9         "nueve"
          , mul  10        "diez"
          , term 11        "once"
          , term 12        "doce"
          , term 13        "trece"
          , term 14        "catorce"
          , term 15        "quince"
          , term 16        "dieciseis"
          , term 17        "diecisiete"
          , term 18        "dieciocho"
          , term 19        "diecinueve"
          , add  20    10  "veinte"
          , add  30    10  "treinta"
          , add  40    10  "cuarenta"
          , add  50    10  "cincuenta"
          , add  60    10  "sesenta"
          , add  70    10  "setenta"
          , add  80    10  "ochenta"
          , add  90    10  "noventa"
          , term 100       "cien"
          , mul  100       "ciento"
          , add  500   100 "quinientos"
          , add  700   100 "setecientos"
          , add  900   100 "novocientos"
          , mul  1000      "mil"
          , mul  (d 6)     "un millón"
          ]

sp :: NumConfig
sp = NumConfig { ncNeg      = spNeg
               , ncOne      = spOne
               , ncAdd      = spAdd
               , ncMul      = spMul
               , ncCardinal = findSym spTable
               }

-------------------------------------------------------------------------------

itNeg :: NegCombinator
itNeg = error "itNeg: not defined yet you fool!"

itOne :: OneCombinator
itOne = snd

itAdd :: Combinator
itAdd (_, x') (y, y') | y == 3    = x' <> "tré"
                      | otherwise = x' <> y'

itMul :: Combinator
itMul (_, x') (y, y') | y < d 6   = x' <> y'
                      | otherwise = x' <+> y'

itTable :: [NumSymbol]
itTable = [ term 0           "zero"
          , term 1           "uno"
          , term 2           "due"
          , term 3           "tre"
          , term 4           "quattro"
          , term 5           "cinque"
          , term 6           "sei"
          , term 7           "sette"
          , term 8           "otto"
          , term 9           "nove"
          , mul  10          "dieci"
          , term 11          "undici"
          , term 12          "dodici"
          , term 13          "tredici"
          , term 14          "quattordici"
          , term 15          "quindici"
          , term 16          "sedici"
          , term 17          "diciassette"
          , term 18          "diciotto"
          , term 19          "diciannove"
          , add  20    10    "venti"
          , term 21          "ventuno"
          , term 28          "ventotto"
          , add  30    10    "trenta"
          , term 31          "trentuno"
          , term 38          "trentotto"
          , add  40    10    "quaranta"
          , term 41          "quarantuno"
          , term 48          "quarantotto"
          , add  50    10    "cinquanta"
          , term 51          "cinquantuno"
          , term 58          "cinquantotto"
          , add  60    10    "sessanta"
          , term 61          "sessantuno"
          , term 68          "sessantotto"
          , add  70    10    "settanta"
          , term 71          "settantuno"
          , term 78          "settantotto"
          , add  80    10    "ottanta"
          , term 81          "ottantuno"
          , term 88          "ottantotto"
          , add  90    10    "novanta"
          , term 91          "novantuno"
          , term 98          "novantotto"
          , mul  100         "cento"
          , add  1000  1000  "mille"
          , mul  1000        "mila"
          , add  (d 6) (d 6) "milione"
          , mul  (d 6)       "milioni"
          , add  (d 9) (d 9) "miliardo"
          , mul  (d 9)       "miliardi"
          ]

it :: NumConfig
it = NumConfig { ncNeg      = itNeg
               , ncOne      = itOne
               , ncAdd      = itAdd
               , ncMul      = itMul
               , ncCardinal = findSym itTable
               }

-------------------------------------------------------------------------------

eoNeg :: NegCombinator
eoNeg = error "eoNeg: not defined yet you fool!"

eoOne :: OneCombinator
eoOne = snd

eoAdd :: Combinator
eoAdd (_, x') (_, y') = x' <+> y'

eoMul :: Combinator
eoMul (_, x') (_, y') = x' <> y'

eoTable :: [NumSymbol]
eoTable = [ term 0    "nulo"
          , term 1    "unu"
          , term 2    "du"
          , term 3    "tri"
          , term 4    "kvar"
          , term 5    "kvin"
          , term 6    "ses"
          , term 7    "sep"
          , term 8    "ok"
          , term 9    "naŭ"
          , mul 10    "dek"
          , mul 100   "cent"
          , mul 1000  "mil"
          , mul (d 6) "miliono"
          ]

eo :: NumConfig
eo = NumConfig { ncNeg      = eoNeg
               , ncOne      = eoOne
               , ncAdd      = eoAdd
               , ncMul      = eoMul
               , ncCardinal = findSym eoTable
               }

-------------------------------------------------------------------------------

jaNeg :: NegCombinator
jaNeg s = "mainasu" <+> s

jaOne :: OneCombinator
jaOne (v, v') | v < 100 || (300 >= v && v < 400) = v'
              | otherwise = "ichi" <-> v'

jaAdd :: Combinator
jaAdd (_, x') (_, y') = x' <+> y'

jaMul :: Combinator
jaMul (_, x') (_, y') = x' <-> y'

jaTable :: [NumSymbol]
jaTable = [ term 0         "rei"
          , term 1         "ichi"
          , term 2         "ni"
          , term 3         "san"
          , term 4         "yon"
          , term 5         "go"
          , term 6         "roku"
          , term 7         "nana"
          , term 8         "hachi"
          , term 9         "kyū"
          , mul 10         "jū"
          , mul 100        "hyaku"
          , add 300    100 "san-byaku" -- rendaku
          , mul 1000       "sen"
          , mul (d 4)      "man"
          , mul (d 8)      "oku"
          , mul (d 12)     "chō"
          , mul (d 16)     "kei"
          , mul (d 20)     "gai"
          , mul (d 24)     "jo"
          , mul (d 28)     "jō"
          , mul (d 32)     "kō"
          , mul (d 36)     "kan"
          , mul (d 40)     "sei"
          , mul (d 44)     "sai"
          , mul (d 48)     "goku"
          , mul (d 52)     "gōgasha"
          , mul (d 56)     "asōgi"
          , mul (d 60)     "nayuta"
          , mul (d 64)     "fukashigi"
          , mul (d 68)     "muryōtaisū"
          ]

ja :: NumConfig
ja = NumConfig { ncNeg      = jaNeg
               , ncOne      = jaOne
               , ncAdd      = jaAdd
               , ncMul      = jaMul
               , ncCardinal = findSym jaTable
               }

