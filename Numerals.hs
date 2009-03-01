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
-}

import Data.List
import qualified Data.DString as DS
import Data.String
import Data.Monoid

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data SymbolType = Terminal
                | Add Integer
                | Mul Integer Integer
                  deriving Show

data NumSymbol = NumSym { symType     :: SymbolType
                        , symVal      :: Integer
                        , symStr      :: String
                        } deriving Show

-- Scope is given as a size. So a scope of 10 includes the value of the symbol itself.
-- NumSym 10 10 10 "ten"
-- "ten" is in the additive scope for [10 .. 10 + 10 - 1]
-- "ten" is in the multiplicative scope for [10 .. 10 * 10 - 1]

data NumConfig = forall s. (IsString s, Stringable s) =>
                 NumConfig { ncCardinal :: Integer -> Maybe NumSymbol
                           , ncOne      :: NumSymbol -> s
                           , ncAdd      :: (Integer, s) -> (Integer, s) -> s
                           , ncMul      :: (Integer, s) -> (Integer, s) -> s
                           }

-- TODO: Better names:
type OneCombinator = NumSymbol -> DS.DString
type Combinator    = (Integer, DS.DString) -> (Integer, DS.DString) -> DS.DString

(+++) :: Monoid m => m -> m -> m
(+++) = mappend

infixr 5 +++ -- Same as ++

class Stringable s where
    toString :: s -> String

instance Stringable String where
    toString = id

instance Stringable DS.DString where
    toString = DS.toString

-- Easy construction of NumSymbols
term :: Integer -> String -> NumSymbol
term = NumSym Terminal

add :: Integer -> Integer -> String -> NumSymbol
add v aScope = NumSym (Add aScope) v

mul :: Integer -> Integer -> String -> NumSymbol
mul v mScope = NumSym (Mul v mScope) v

d :: Integer -> Integer
d = (10 ^)

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------


cardinal :: NumConfig -> Integer -> Maybe String
cardinal NumConfig {..} 0 = fmap symStr $ ncCardinal 0
cardinal NumConfig {..} x | x < 0     = Nothing
                          | otherwise = fmap toString $ go x
    where go n = do sym@(NumSym _ v v') <- ncCardinal n
                    let vs = fromString v'
                    case n `divMod` v of
                      (1, 0) -> return $ ncOne sym
                      (1, r) -> do rs <- go r
                                   return $ (v, ncOne sym) `ncAdd` (r, rs)
                      (q, r) | q > v     -> Nothing
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
          go a m (x@(NumSym t v _) : xs)
              | v == n    = Just x
              | otherwise = case t of
                              Terminal              -> go a m xs
                              Add aS    | v > n     -> stop a m
                                        | otherwise -> go x m xs
                              Mul aS mS | v > n     -> stop a m
                                        | otherwise -> go a x xs

          stop :: NumSymbol -> NumSymbol -> Maybe NumSymbol
          stop a m | inAddScope a = return a
                   | inMulScope m = return m
                   | otherwise    = Nothing

          inAddScope :: NumSymbol -> Bool
          inAddScope (NumSym (Add a)   v _) = n < v + a
          inAddScope (NumSym (Mul a _) v _) = n < v + a
          inAddScope _                      = False

          inMulScope :: NumSymbol -> Bool
          inMulScope (NumSym (Mul _ m) v _) = n < m * v
          inMulScope _                      = False

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

nlOne :: OneCombinator
nlOne = fromString . symStr

nlAdd :: Combinator
nlAdd (x, x') (y, y') | x < 20    = y' +++ x'
                      | x < 100   = case y of
                                      2 -> "tweëen" +++ x'
                                      3 -> "driëen" +++ x'
                                      _ -> y' +++ "en" +++ x'
                      | otherwise = x' +++ " " +++ y'

nlMul :: Combinator
nlMul (_, x') (_, y') = x' +++ " " +++ y'

nlTable :: [NumSymbol]
nlTable = [ term 0             "nul"
          , term 1             "één"
          , term 2             "twee"
          , term 3             "drie"
          , term 4             "vier"
          , term 5             "vijf"
          , term 6             "zes"
          , term 7             "zeven"
          , term 8             "acht"
          , term 9             "negen"
          , mul  10      10    "tien"
          , term 11            "elf"
          , term 12            "twaalf"
          , term 13            "dertien"
          , term 14            "veertien"
          , add  20      10    "twintig"
          , add  30      10    "dertig"
          , add  40      10    "veertig"
          , add  50      10    "vijftig"
          , add  60      10    "zestig"
          , add  70      10    "zeventig"
          , add  80      10    "tachtig"
          , add  90      10    "negentig"
          , mul  (d 2)   10    "honderd"
          , mul  (d 3)   (d 3) "duizend"
          , mul  (d 6)   (d 3) "miljoen"
          , mul  (d 9)   (d 3) "miljard"
          , mul  (d 12)  (d 3) "biljoen"
          , mul  (d 15)  (d 3) "biljard"
          , mul  (d 18)  (d 3) "triljoen"
          , mul  (d 21)  (d 3) "triljard"
          , mul  (d 24)  (d 3) "quadriljoen"
          , mul  (d 27)  (d 3) "quadriljard"
          , mul  (d 30)  (d 3) "quintiljoen"
          , mul  (d 33)  (d 3) "quintiljard"
          , mul  (d 36)  (d 3) "sextiljoen"
          , mul  (d 39)  (d 3) "sextiljard"
          , mul  (d 42)  (d 3) "septiljoen"
          , mul  (d 45)  (d 3) "septiljard"
          , mul  (d 48)  (d 3) "octiljoen"
          , mul  (d 51)  (d 3) "octiljard"
          , mul  (d 54)  (d 3) "noniljoen"
          , mul  (d 57)  (d 3) "noniljard"
          , mul  (d 60)  (d 3) "deciljoen"
          , mul  (d 63)  (d 3) "deciljard"
          , mul  (d 66)  (d 3) "undeciljoen"
          , mul  (d 69)  (d 3) "undeciljard"
          , mul  (d 72)  (d 3) "duodeciljoen"
          , mul  (d 75)  (d 3) "duodeciljard"
          , mul  (d 78)  (d 3) "tredeciljoen"
          , mul  (d 81)  (d 3) "tredeciljard"
          , mul  (d 84)  (d 3) "quattuordeciljoen"
          , mul  (d 87)  (d 3) "quattuordeciljard"
          , mul  (d 90)  (d 3) "quindeciljoen"
          , mul  (d 93)  (d 3) "quindeciljard"
          , mul  (d 96)  (d 3) "sexdeciljoen"
          , mul  (d 99)  (d 3) "sexdeciljard"
          , mul  (d 102) (d 3) "septendeciljoen"
          , mul  (d 105) (d 3) "septendeciljard"
          , mul  (d 108) (d 3) "octodeciljoen"
          , mul  (d 111) (d 3) "octodeciljard"
          , mul  (d 114) (d 3) "novemdeciljoen"
          , mul  (d 117) (d 3) "novemdeciljard"
          , mul  (d 120) (d 3) "vigintiljoen"
          , mul  (d 123) (d 3) "vigintiljard"
          ]

nl :: NumConfig
nl = NumConfig { ncOne      = nlOne
               , ncAdd      = nlAdd
               , ncMul      = nlMul
               , ncCardinal = findSym nlTable
               }

-------------------------------------------------------------------------------

enTable :: [NumSymbol]
enTable = [ term 0         "zero"
          , term 1         "one"
          , term 2         "two"
          , term 3         "three"
          , term 4         "four"
          , term 5         "five"
          , term 6         "six"
          , term 7         "seven"
          , term 8         "eight"
          , term 9         "nine"
          , mul  10   10   "ten"
          , term 11        "eleven"
          , term 12        "twelve"
          , term 13        "thirteen"
          , term 14        "fourteen"
          , term 15        "fifteen"
          , term 16        "sixteen"
          , term 17        "seventeen"
          , term 18        "eighteen"
          , term 19        "nineteen"
          , add  20   10   "twenty"
          , add  30   10   "thirty"
          , add  40   10   "forty"
          , add  50   10   "fifty"
          , add  60   10   "sixty"
          , add  70   10   "seventy"
          , add  80   10   "eighty"
          , add  90   10   "ninety"
          , mul  100  10   "hundred"
          , mul  1000 1000 "thousand"
          ]

enOne :: OneCombinator
enOne (NumSym (Mul _ _) v v') | v >= 100  = "one" +++ " " +++ vs
                              | otherwise = vs
    where vs = fromString v'

enOne sym = fromString $ symStr sym

enAdd :: Combinator
enAdd (x, x') (_, y') | x < 100   = x' +++ "-" +++ y'
                      | otherwise = x' +++ " " +++ y'

enMul :: Combinator
enMul (_, x') (_, y') = x' +++ " " +++ y'

shortEnTable :: [NumSymbol]
shortEnTable = enTable ++
               [ mul (d 6)  (d 3) "million"
               , mul (d 9)  (d 3) "billion"
               , mul (d 12) (d 3) "trillion"
               , mul (d 15) (d 3) "quadrillion"
               , mul (d 18) (d 3) "quintillion"
               , mul (d 21) (d 3) "sextillion"
               , mul (d 24) (d 3) "septillion"
               , mul (d 27) (d 3) "octillion"
               , mul (d 30) (d 3) "nonillion"
               , mul (d 33) (d 3) "decillion"
               , mul (d 36) (d 3) "undecillion"
               , mul (d 39) (d 3) "duodecillion"
               , mul (d 42) (d 3) "tredecillion"
               , mul (d 45) (d 3) "quattuordecillion"
               , mul (d 48) (d 3) "quindecillion"
               , mul (d 51) (d 3) "sexdecillion"
               , mul (d 54) (d 3) "septendecillion"
               , mul (d 57) (d 3) "octodecillion"
               , mul (d 60) (d 3) "novemdecillion"
               , mul (d 63) (d 3) "vigintillion"
               ]

shortEn :: NumConfig
shortEn = NumConfig { ncOne      = enOne
                    , ncAdd      = enAdd
                    , ncMul      = enMul
                    , ncCardinal = findSym shortEnTable
                    }

longEnTable :: [NumSymbol]
longEnTable = enTable ++
              [ mul (d 6)   (d 3) "million"
              , mul (d 9)   (d 3) "milliard"
              , mul (d 12)  (d 3) "billion"
              , mul (d 15)  (d 3) "billiard"
              , mul (d 18)  (d 3) "trillion"
              , mul (d 21)  (d 3) "trilliard"
              , mul (d 24)  (d 6) "quadrillion"
              , mul (d 30)  (d 6) "quintillion"
              , mul (d 36)  (d 6) "sextillion"
              , mul (d 42)  (d 6) "septillion"
              , mul (d 48)  (d 6) "octillion"
              , mul (d 54)  (d 6) "nonillion"
              , mul (d 60)  (d 6) "decillion"
              , mul (d 66)  (d 6) "undecillion"
              , mul (d 72)  (d 6) "duodecillion"
              , mul (d 78)  (d 6) "tredecillion"
              , mul (d 84)  (d 6) "quattuordecillion"
              , mul (d 90)  (d 6) "quinquadecillion"
              , mul (d 96)  (d 6) "sedecillion"
              , mul (d 102) (d 6) "septendecillion"
              , mul (d 108) (d 6) "octodecillion"
              , mul (d 114) (d 6) "novendecillion"
              , mul (d 120) (d 6) "vigintillion"
              ]

longEn :: NumConfig
longEn = NumConfig { ncOne      = enOne
                   , ncAdd      = enAdd
                   , ncMul      = enMul
                   , ncCardinal = findSym longEnTable
                   }

-------------------------------------------------------------------------------


deOne :: OneCombinator
deOne (NumSym _ v v') | v >= (d 6) = "eine" +++ " " +++ vs
                      | v >= 100   = "ein" +++ vs
                      | otherwise  = vs
    where vs = fromString v'

deAdd :: Combinator
deAdd (x, x') (y, y') | x < 20    = y' +++ x'
                      | x < 100   = case y of
                                      1 -> "einund" +++ x'
                                      _ -> y' +++ "und" +++ x'
                      | otherwise = x' +++ y'

deMul :: Combinator
deMul (_, x') (y, y') | y < (d 6) = x' +++ y'
                      | otherwise = x' +++ y' -- x' +++ " " +++ y'

deTable :: [NumSymbol]
deTable = [ term 0           "null"
          , term 1           "eins"
          , term 2           "zwei"
          , term 3           "drei"
          , term 4           "vier"
          , term 5           "fünf"
          , term 6           "sechs"
          , term 7           "sieben"
          , term 8           "acht"
          , term 9           "neun"
          , mul  10    10    "zehn"
          , term 11          "elf"
          , term 12          "zwölf"
          , term 16          "sechzehn"
          , term 17          "siebzehn"
          , add  20    10    "zwanzig"
          , add  30    10    "dreißig"
          , add  40    10    "vierzig"
          , add  50    10    "fünfzig"
          , add  60    10    "sechzig"
          , add  70    10    "siebzig"
          , add  80    10    "achtzig"
          , add  90    10    "neunzig"
          , mul  100   10    "hundert"
          , mul  1000  1000  "tausend"
          , mul  (d 6) (d 3) "million"
          , mul  (d 9) (d 3) "milliarde"
          ]

de :: NumConfig
de = NumConfig { ncOne      = deOne
               , ncAdd      = deAdd
               , ncMul      = deMul
               , ncCardinal = findSym deTable
               }

-------------------------------------------------------------------------------

seOne :: OneCombinator
seOne = fromString . symStr

seAdd :: Combinator
seAdd (_, x') (_, y') = x' +++ y'

seMul :: Combinator
seMul (_, x') (_, y') = x' +++ y'

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
          , mul  10    10    "tio"
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
          , mul  100   10    "hundra"
          , mul  (d 3) (d 3) "tusen"
          , add  (d 6) (d 6) "miljon"
          , mul  (d 6) (d 3) "miljoner"
          , add  (d 9) (d 9) "miljard"
          , mul  (d 9) (d 3) "miljarder"
          ]

se :: NumConfig
se = NumConfig { ncOne      = seOne
               , ncAdd      = seAdd
               , ncMul      = seMul
               , ncCardinal = findSym seTable
               }

-------------------------------------------------------------------------------

noOne :: OneCombinator
noOne (NumSym _ v v') | v >= (d 6) = "én" +++ " " +++ vs
                      | otherwise  = vs
    where vs = fromString v'

noAdd :: Combinator
noAdd (x, x') (_, y') | x == 100  = x' +++ " " +++ "og" +++ " " +++ y'
                      | otherwise = x' +++ y'

noMul :: Combinator
noMul (_, x') (_, y') = x' +++ y'

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
          , mul  10    10    "ti"
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
          , mul  100   10    "hundre"
          , mul  (d 3) (d 3) "tusen"
          , add  (d 6) (d 6) "million"
          , mul  (d 6) (d 3) "millioner"
          , add  (d 9) (d 9) "milliard"
          , mul  (d 9) (d 3) "milliarder"
          ]

no :: NumConfig
no = NumConfig { ncOne      = noOne
               , ncAdd      = noAdd
               , ncMul      = noMul
               , ncCardinal = findSym noTable
               }

-------------------------------------------------------------------------------

latinOne :: OneCombinator
latinOne = fromString . symStr

latinAdd :: Combinator
latinAdd (_, x') (_, y') = x' +++ " " +++ y'

latinMul :: Combinator
latinMul (_, x') (_, y') = x' +++ " " +++ y'

latinTable :: [NumSymbol]
latinTable = [ term 0         "nulla"
             , term 1         "unus"
             , term 2         "duo"
             , term 3         "tres"
             , term 4         "quattuor"
             , term 5         "quinque"
             , term 6         "sex"
             , term 7         "septem"
             , term 8         "octo"
             , term 9         "novem"
             , mul  10   10   "decem"
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
             , mul  100  10   "centum"
             , add  200  100  "ducenti"
             , add  300  100  "trecenti"
             , add  400  100  "quadrigenti"
             , add  500  100  "quingenti"
             , add  600  100  "sescenti"
             , add  700  100  "septingenti"
             , add  800  100  "octigenti"
             , add  900  100  "nongenti"
             , add  1000 1000 "mille"
             , mul  1000 1000 "millia"
             , term (d 6)     "decies centena milia"
             ]

latin :: NumConfig
latin = NumConfig { ncOne      = latinOne
                  , ncAdd      = latinAdd
                  , ncMul      = latinMul
                  , ncCardinal = findSym latinTable
                  }

-------------------------------------------------------------------------------

frOne :: OneCombinator
frOne = fromString . symStr

frAdd :: Combinator
frAdd (x, x') (y, y') | x < 80 && y == 1 = x' +++ " " +++ "et" +++ " " +++ y'
                      | x < 100          = x' +++ "-" +++ y'
                      | otherwise        = x' +++ " " +++ y'

frMul :: Combinator
frMul (_, x') (_, y') = x' +++ " " +++ y'

frTable :: [NumSymbol]
frTable = [ term 0           "zéro"
          , term 1           "un"
          , term 2           "deux"
          , term 3           "trois"
          , term 4           "quatre"
          , term 5           "cinq"
          , term 6           "six"
          , term 7           "sept"
          , term 8           "huit"
          , term 9           "neuf"
          , mul  10    10    "dix"
          , term 11          "onze"
          , term 12          "douze"
          , term 13          "treize"
          , term 14          "quatorze"
          , term 15          "quinze"
          , term 16          "seize"
          , add  20    10    "vingt"
          , add  30    10    "trente"
          , add  40    10    "quarante"
          , add  50    10    "cinquante"
          , add  60    10    "soixante"
          , term 71          "soixante et onze"
          , term 80          "quatre-vingts"
          , add  80    10    "quatre-vingt"
          , add  100   100   "cent"
          , mul  100   10    "cents"
          , mul  1000  (d 3) "mille"
          , mul  (d 6) (d 3) "million"
          , mul  (d 9) (d 3) "millard"
          ]

fr :: NumConfig
fr = NumConfig { ncOne      = frOne
               , ncAdd      = frAdd
               , ncMul      = frMul
               , ncCardinal = findSym frTable
               }

-------------------------------------------------------------------------------

spOne :: OneCombinator
spOne = fromString . symStr

spAdd :: Combinator
spAdd (x, x') (_, y') | x < 100   =  x' +++ " " +++ "y" +++ " " +++ y'
                      | otherwise  = x' +++ " " +++ y'

spMul :: Combinator
spMul (_, x') (_, y') = x' +++ " " +++ y'

spTable :: [NumSymbol]
spTable = [ term 0           "cero"
          , term 1           "uno"
          , term 2           "dos"
          , term 3           "tres"
          , term 4           "cuatro"
          , term 5           "cinco"
          , term 6           "seis"
          , term 7           "siete"
          , term 8           "ocho"
          , term 9           "nueve"
          , mul  10    10    "diez"
          , term 11          "once"
          , term 12          "doce"
          , term 13          "trece"
          , term 14          "catorce"
          , term 15          "quince"
          , term 16          "dieciseis"
          , term 17          "diecisiete"
          , term 18          "dieciocho"
          , term 19          "diecinueve"
          , add  20    10    "veinte"
          , add  30    10    "treinta"
          , add  40    10    "cuarenta"
          , add  50    10    "cincuenta"
          , add  60    10    "sesenta"
          , add  70    10    "setenta"
          , add  80    10    "ochenta"
          , add  90    10    "noventa"
          , term 100         "cien"
          , mul  100   10    "ciento"
          , add  500   100   "quinientos"
          , add  700   100   "setecientos"
          , add  900   100   "novocientos"
          , mul  1000  (d 3) "mil"
          , mul  (d 6) (d 3) "un millón"
          ]

sp :: NumConfig
sp = NumConfig { ncOne      = spOne
               , ncAdd      = spAdd
               , ncMul      = spMul
               , ncCardinal = findSym spTable
               }

-------------------------------------------------------------------------------

itOne :: OneCombinator
itOne = fromString . symStr

itAdd :: Combinator
itAdd (_, x') (y, y') | y == 3    = x' +++ "tré"
                      | otherwise = x' +++ y'

itMul :: Combinator
itMul (_, x') (y, y') | y < d 6   = x' +++ y'
                      | otherwise = x' +++ " " +++ y'

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
          , mul  10    10    "dieci"
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
          , mul  100   10    "cento"
          , add  1000  1000  "mille"
          , mul  1000  (d 3) "mila"
          , add  (d 6) (d 6) "milione"
          , mul  (d 6) (d 3) "milioni"
          , add  (d 9) (d 9) "miliardo"
          , mul  (d 9) (d 3) "miliardi"
          ]

it :: NumConfig
it = NumConfig { ncOne      = itOne
               , ncAdd      = itAdd
               , ncMul      = itMul
               , ncCardinal = findSym itTable
               }

-------------------------------------------------------------------------------

eoOne :: OneCombinator
eoOne = fromString . symStr

eoAdd :: Combinator
eoAdd (_, x') (_, y') = x' +++ " " +++ y'

eoMul :: Combinator
eoMul (_, x') (_, y') = x' +++ y'

eoTable :: [NumSymbol]
eoTable = [ term 0          "nulo"
          , term 1          "unu"
          , term 2          "du"
          , term 3          "tri"
          , term 4          "kvar"
          , term 5          "kvin"
          , term 6          "ses"
          , term 7          "sep"
          , term 8          "ok"
          , term 9          "naŭ"
          , mul 10    10    "dek"
          , mul 100   10    "cent"
          , mul 1000  (d 3) "mil"
          , mul (d 6) (d 3) "miliono"
          ]

eo :: NumConfig
eo = NumConfig { ncOne      = eoOne
               , ncAdd      = eoAdd
               , ncMul      = eoMul
               , ncCardinal = findSym eoTable
               }

-------------------------------------------------------------------------------

jaOne :: OneCombinator
jaOne (NumSym _ v v') | v < 100 || (300 >= v && v < 400) = vs
                      | otherwise = "ichi" +++ "-" +++ vs
    where vs = fromString v'

jaAdd :: Combinator
jaAdd (_, x') (_, y') = x' +++ " " +++ y'

jaMul :: Combinator
jaMul (_, x') (_, y') = x' +++ "-" +++ y'

jaTable :: [NumSymbol]
jaTable = [ term 0           "zero"
          , term 1           "ichi"
          , term 2           "ni"
          , term 3           "san"
          , term 4           "yon"
          , term 5           "go"
          , term 6           "roku"
          , term 7           "nana"
          , term 8           "hachi"
          , term 9           "kyū"
          , mul 10     10    "jū"
          , mul 100    10    "hyaku"
          , add 300    100   "san-byaku" -- rendaku
          , mul 1000   10    "sen"
          , mul (d 4)  (d 4) "man"
          , mul (d 8)  (d 4) "oku"
          , mul (d 12) (d 4) "chō"
          , mul (d 16) (d 4) "kei"
          , mul (d 20) (d 4) "gai"
          , mul (d 24) (d 4) "jo"
          , mul (d 28) (d 4) "jō"
          , mul (d 32) (d 4) "kō"
          , mul (d 36) (d 4) "kan"
          , mul (d 40) (d 4) "sei"
          , mul (d 44) (d 4) "sai"
          , mul (d 48) (d 4) "goku"
          , mul (d 52) (d 4) "gōgasha"
          , mul (d 56) (d 4) "asōgi"
          , mul (d 60) (d 4) "nayuta"
          , mul (d 64) (d 4) "fukashigi"
          , mul (d 68) (d 4) "muryōtaisū"
          ]

ja :: NumConfig
ja = NumConfig { ncOne      = jaOne
               , ncAdd      = jaAdd
               , ncMul      = jaMul
               , ncCardinal = findSym jaTable
               }
