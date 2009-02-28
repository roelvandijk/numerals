-- -*- coding: utf-8 -*-

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

  * Decimals == fractions
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

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------


data SymbolType = Base     -- Multiplication base
                | Terminal
                  deriving Show

data NumSymbol = NumSym { sumType :: SymbolType
                        , symVal  :: Integer
                        , symStr  :: String
                        } deriving Show

data NumConfig = NumConfig { ncMax      :: Maybe Integer
                           , ncCardinal :: Integer -> Maybe NumSymbol
                           , ncOne      :: NumSymbol -> ShowS
                           , ncAdd      :: (Integer, ShowS) -> (Integer, ShowS) -> ShowS
                           , ncMul      :: (Integer, ShowS) -> (Integer, ShowS) -> ShowS
                           }

-- Easy construction of NumSymbols
b, t :: Integer -> String -> NumSymbol
b = NumSym Base
t = NumSym Terminal

d :: Integer -> Integer
d = (10 ^)

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------


cardinal :: NumConfig -> Integer -> Maybe String
cardinal nc 0 = do zero <- ncCardinal nc $ 0
                   return $ symStr zero
cardinal nc x | x < 0     = Nothing
              | otherwise = maybe (fmap toString $ go x)
                                  (\m -> if (x <= m)
                                         then fmap toString $ go x
                                         else Nothing
                                  )
                                  $ ncMax nc
    where go n = do sym@(NumSym _ v v') <- (ncCardinal nc) n
                    let vs = showString v'
                    case n `divMod` v of
                      (1, 0) -> return $ one sym
                      (1, r) -> do rs <- go r
                                   return $ (v, one sym) `add` (r, rs)
                      (q, r) | q > v     -> Nothing
                             | otherwise -> do qs <- go q
                                               if r == 0
                                                 then return $ (q, qs) `mul` (v, vs)
                                                 else do rs <- go r
                                                         return $ (q*v, (q, qs) `mul` (v, vs)) `add` (r, rs)
          one = ncOne nc
          add = ncAdd nc
          mul = ncMul nc

toString :: ShowS -> String
toString = ($ [])

-- | Find the first symbol which has exactly the requested value. If
--   none is found find the last multiplication base which is smaller
--   than the given number. If none can be found the result is Nothing.
findSym :: [NumSymbol] -> Integer -> Maybe NumSymbol
findSym []     _ = Nothing
findSym (t:ts) n = go t ts
    where go prev [] = Just prev
          go prev (x@(NumSym Terminal val _) : xs) | val == n  = Just x
                                                   | otherwise = go prev xs
          go prev (x@(NumSym Base     val _) : xs) | val <= n  = go x xs
                                                   | otherwise = Just prev

-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------

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

nlOne = showString . symStr
nlAdd (x, x') (y, y') | x < 20    = y' . x'
                      | x < 100   = case y of
                                      2 -> showString "tweëen" . x'
                                      3 -> showString "driëen" . x'
                                      _ -> y' . showString "en" . x'
                      | otherwise = x' . showChar ' ' . y'
nlMul (_, x') (_, y') = x' . showChar ' ' .  y'

nlTable = [ t 0       "nul"
          , t 1       "een"
          , t 2       "twee"
          , t 3       "drie"
          , t 4       "vier"
          , t 5       "vijf"
          , t 6       "zes"
          , t 7       "zeven"
          , t 8       "acht"
          , t 9       "negen"
          , b 10      "tien"
          , t 11      "elf"
          , t 12      "twaalf"
          , t 13      "dertien"
          , t 14      "veertien"
          , b 20      "twintig"
          , b 30      "dertig"
          , b 40      "veertig"
          , b 50      "vijftig"
          , b 60      "zestig"
          , b 70      "zeventig"
          , b 80      "tachtig"
          , b 90      "negentig"
          , b (d 2)   "honderd"
          , b (d 3)   "duizend"
          , b (d 6)   "miljoen"
          , b (d 9)   "miljard"
          , b (d 12)  "biljoen"
          , b (d 15)  "biljard"
          , b (d 18)  "triljoen"
          , b (d 21)  "triljard"
          , b (d 24)  "quadriljoen"
          , b (d 27)  "quadriljard"
          , b (d 30)  "quintiljoen"
          , b (d 33)  "quintiljard"
          , b (d 36)  "sextiljoen"
          , b (d 39)  "sextiljard"
          , b (d 42)  "septiljoen"
          , b (d 45)  "septiljard"
          , b (d 48)  "octiljoen"
          , b (d 51)  "octiljard"
          , b (d 54)  "noniljoen"
          , b (d 57)  "noniljard"
          , b (d 60)  "deciljoen"
          , b (d 63)  "deciljard"
          , b (d 66)  "undeciljoen"
          , b (d 69)  "undeciljard"
          , b (d 72)  "duodeciljoen"
          , b (d 75)  "duodeciljard"
          , b (d 78)  "tredeciljoen"
          , b (d 81)  "tredeciljard"
          , b (d 84)  "quattuordeciljoen"
          , b (d 87)  "quattuordeciljard"
          , b (d 90)  "quindeciljoen"
          , b (d 93)  "quindeciljard"
          , b (d 96)  "sexdeciljoen"
          , b (d 99)  "sexdeciljard"
          , b (d 102) "septendeciljoen"
          , b (d 105) "septendeciljard"
          , b (d 108) "octodeciljoen"
          , b (d 111) "octodeciljard"
          , b (d 114) "novemdeciljoen"
          , b (d 117) "novemdeciljard"
          , b (d 120) "vigintiljoen"
          , b (d 123) "vigintiljard"
          ]

nl :: NumConfig
nl = NumConfig { ncMax      = Just $ d 126 - 1
               , ncOne      = nlOne
               , ncAdd      = nlAdd
               , ncMul      = nlMul
               , ncCardinal = findSym nlTable
               }

enTable :: [NumSymbol]
enTable = [ t 0    "zero"
          , t 1    "one"
          , t 2    "two"
          , t 3    "three"
          , t 4    "four"
          , t 5    "five"
          , t 6    "six"
          , t 7    "seven"
          , t 8    "eight"
          , t 9    "nine"
          , b 10   "ten"
          , t 11   "eleven"
          , t 12   "twelve"
          , t 13   "thirteen"
          , t 14   "fourteen"
          , t 15   "fifteen"
          , t 16   "sixteen"
          , t 17   "seventeen"
          , t 18   "eighteen"
          , t 19   "nineteen"
          , b 20   "twenty"
          , b 30   "thirty"
          , b 40   "forty"
          , b 50   "fifty"
          , b 60   "sixty"
          , b 70   "seventy"
          , b 80   "eighty"
          , b 90   "ninety"
          , b 100  "hundred"
          , b 1000 "thousand"
          ]

enOne (NumSym Base v v') | v >= 100  = showString "one " . vs
                         | otherwise = vs
    where
      vs = showString v'

enOne sym = showString $ symStr sym

enAdd (x, x') (_, y') | x < 100   = x' . showChar '-' . y'
                      | otherwise = x' . showChar ' ' . y'

enMul (_, x') (_, y') = x' . showChar ' ' . y'

shortEnTable :: [NumSymbol]
shortEnTable = enTable ++
               [ b (d 6)  "million"
               , b (d 9)  "billion"
               , b (d 12) "trillion"
               , b (d 15) "quadrillion"
               , b (d 18) "quintillion"
               , b (d 21) "sextillion"
               , b (d 24) "septillion"
               , b (d 27) "octillion"
               , b (d 30) "nonillion"
               , b (d 33) "decillion"
               , b (d 36) "undecillion"
               , b (d 39) "duodecillion"
               , b (d 42) "tredecillion"
               , b (d 45) "quattuordecillion"
               , b (d 48) "quindecillion"
               , b (d 51) "sexdecillion"
               , b (d 54) "septendecillion"
               , b (d 57) "octodecillion"
               , b (d 60) "novemdecillion"
               , b (d 63) "vigintillion"
               ]

shortEn :: NumConfig
shortEn = NumConfig { ncMax      = Just $ d 66 - 1
                    , ncOne      = enOne
                    , ncAdd      = enAdd
                    , ncMul      = enMul
                    , ncCardinal = findSym shortEnTable
                    }

longEnTable :: [NumSymbol]
longEnTable = enTable ++
              [ b (d 6)   "million"
              , b (d 9)   "milliard"
              , b (d 12)  "billion"
              , b (d 15)  "billiard"
              , b (d 18)  "trillion"
              , b (d 21)  "trilliard"
              , b (d 24)  "quadrillion"
              , b (d 30)  "quintillion"
              , b (d 36)  "sextillion"
              , b (d 42)  "septillion"
              , b (d 48)  "octillion"
              , b (d 54)  "nonillion"
              , b (d 60)  "decillion"
              , b (d 66)  "undecillion"
              , b (d 72)  "duodecillion"
              , b (d 78)  "tredecillion"
              , b (d 84)  "quattuordecillion"
              , b (d 90)  "quinquadecillion"
              , b (d 96)  "sedecillion"
              , b (d 102) "septendecillion"
              , b (d 108) "octodecillion"
              , b (d 114) "novendecillion"
              , b (d 120) "vigintillion"
              ]

longEn :: NumConfig
longEn = NumConfig { ncMax      = Just $ d 126 - 1
                   , ncOne      = enOne
                   , ncAdd      = enAdd
                   , ncMul      = enMul
                   , ncCardinal = findSym longEnTable
                   }

latinOne = showString . symStr
latinAdd (_, x') (_, y') = x' . showString " A " . y'
latinMul (_, x') (_, y') = x' . showString " M " . y'

latinTable :: [NumSymbol]
latinTable = [ t 0     "nulla"
             , t 1     "unus"
             , t 2     "duo"
             , t 3     "tres"
             , t 4     "quattuor"
             , t 5     "quinque"
             , t 6     "sex"
             , t 7     "septem"
             , t 8     "octo"
             , t 9     "novem"
             , t 10    "decem"
             , t 11    "undecim"
             , t 12    "duodecim"
             , t 13    "tredecim"
             , t 14    "quattuordecim"
             , t 15    "quindecim"
             , t 16    "sedecim"
             , t 17    "septendecim"
             , t 18    "duodeviginti"
             , t 19    "undeviginti"
             , b 20    "viginti"
             , t 28    "duodetriginta"
             , t 29    "undetriginta"
             , b 30    "triginta"
             , t 38    "duodequadraginta"
             , t 39    "undequadraginta"
             , b 40    "quadraginta"
             , t 48    "duodequinquaginta"
             , t 49    "undequinquaginta"
             , b 50    "quinquaginta"
             , t 58    "duodesexaginta"
             , t 59    "undesexaginta"
             , b 60    "sexaginta"
             , t 68    "duodeseptuaginta"
             , t 69    "undeseptuaginta"
             , b 70    "septuaginta"
             , t 78    "duodeoctoginta"
             , t 79    "undeoctoginta"
             , b 80    "octoginta"
             , t 88    "duodenonaginta"
             , t 89    "undenonaginta"
             , b 90    "nonaginta"
             , t 98    "duodecentum"
             , t 99    "undecentum"
             , b 100   "centum"
             , b 200   "ducenti"
             , b 300   "trecenti"
             , b 400   "quadrigenti"
             , b 500   "quingenti"
             , b 600   "sescenti"
             , b 700   "septingenti"
             , b 800   "octigenti"
             , b 900   "nongenti"
             , t 1000  "mille"
             , b 1000  "milia"
             , t (d 6) "decies centena milia"
             ]

latin :: NumConfig
latin = NumConfig { ncMax      = Just $ d 6
                  , ncOne      = latinOne
                  , ncAdd      = latinAdd
                  , ncMul      = latinMul
                  , ncCardinal = findSym latinTable
                  }

frOne sym = showString $ symStr sym
frAdd (x, x') (y, y') | x < 80 && y == 1 = x' . showString " et " . y'
                      | x < 100          = x' . showChar '-' . y'
                      | otherwise        = x' . showChar ' ' . y'
frMul (_, x') (y, y') | y == 100  = xy' . showChar 's'
                      | otherwise = xy'
    where xy' = x' . showChar ' ' . y'

frTable :: [NumSymbol]
frTable = [ t 0     "zéro"
          , t 1     "un"
          , t 2     "deux"
          , t 3     "trois"
          , t 4     "quatre"
          , t 5     "cinq"
          , t 6     "six"
          , t 7     "sept"
          , t 8     "huit"
          , t 9     "neuf"
          , b 10    "dix"
          , t 11    "onze"
          , t 12    "douze"
          , t 13    "treize"
          , t 14    "quatorze"
          , t 15    "quinze"
          , t 16    "seize"
          , b 20    "vingt"
          , b 30    "trente"
          , b 40    "quarante"
          , b 50    "cinquante"
          , b 60    "soixante"
          , t 71    "soixante et onze"
          , t 80    "quatre-vingts"
          , b 80    "quatre-vingt"
          , b 100   "cent"
          , b 1000  "mille"
          , b (d 6) "million"
          , b (d 9) "millard"
          ]

fr :: NumConfig
fr = NumConfig { ncMax      = Nothing
               , ncOne      = frOne
               , ncAdd      = frAdd
               , ncMul      = frMul
               , ncCardinal = findSym frTable
               }


{-
infixr 9 +.+ -- Just like (.)

(+.+) :: ShowS -> ShowS -> ShowS
-- (+.+) = (.)
x +.+ y = x . showChar ' ' . y
-- x +.+ y = p x . showChar '.' . p y
--     where p s = showChar '(' . s . showChar ')'

ss :: String -> ShowS
ss = showString
-- ss s = showChar '"' . showString s . showChar '"' . showString " ++"
-}

{-
nl :: NumConfig
nl = NumConfig { ncMax   = Just $ (10^6)^10^3*10^3
               , ncTable = [ (t, 0,                    "nul")
                           --------------------------------
                           , (t, 1,                    "een")
                           , (t, 2,                    "twee")
                           , (t, 3,                    "drie")
                           , (t, 4,                    "vier")
                           , (t, 5,                    vijf)
                           , (t, 6,                    zes)
                           , (t, 7,                    zeven)
                           , (t, 8,                    acht)
                           , (t, 9,                    negen)
                           --------------------------------
                           , (b, 10,                   tien)
                           --------------------------------
                           , (t, (1+10),               "elf")
                           , (t, (2+10),               "twaalf")
                           , (t, (3+10),               "der"   ++ tien)
                           , (t, (4+10),               "veer"  ++ tien)
                           , (t, (5+10),               vijf    ++ tien)
                           , (t, (6+10),               zes     ++ tien)
                           , (t, (7+10),               zeven   ++ tien)
                           , (t, (8+10),               acht    ++ tien)
                           , (t, (9+10),               negen   ++ tien)
                           --------------------------------
                           , (b, (2*10),               "twin"  ++ tig)
                           , (b, (3*10),               "der"   ++ tig)
                           , (b, (4*10),               "veer"  ++ tig)
                           , (b, (5*10),               vijf    ++ tig)
                           , (b, (6*10),               zes     ++ tig)
                           , (b, (7*10),               zeven   ++ tig)
                           , (b, (8*10),               "tach"  ++ tig)
                           , (b, (9*10),               negen   ++ tig)
                           --------------------------------
                           , (b, (10^2),               "honderd")
                           --------------------------------
                           , (b, (10^3),               "duizend")
                           --------------------------------
                           , (b, ((10^6)^1),           mil         ++ joen)
                           , (b, ((10^6)^1*10^3),      mil         ++ jard)

                           , (b, ((10^6)^2),           bil         ++ joen)
                           , (b, ((10^6)^2*10^3),      bil         ++ jard)

                           , (b, ((10^6)^3),           tril        ++ joen)
                           , (b, ((10^6)^3*10^3),      tril        ++ jard)

                           , (b, ((10^6)^4),           quad ++ ril ++ joen)
                           , (b, ((10^6)^4*10^3),      quad ++ ril ++ jard)

                           , (b, ((10^6)^5),           quin ++ til ++ joen)
                           , (b, ((10^6)^5*10^3),      quin ++ til ++ jard)

                           , (b, ((10^6)^6),           sex  ++ til ++ joen)
                           , (b, ((10^6)^6*10^3),      sex  ++ til ++ jard)

                           , (b, ((10^6)^7),           sep  ++ til ++ joen)
                           , (b, ((10^6)^7*10^3),      sep  ++ til ++ jard)

                           , (b, ((10^6)^8),           oc   ++ til ++ joen)
                           , (b, ((10^6)^8*10^3),      oc   ++ til ++ jard)

                           , (b, ((10^6)^9),           no   ++ nil ++ joen)
                           , (b, ((10^6)^9*10^3),      no   ++ nil ++ jard)
                           --------------------------------
                           , (b, ((10^6)^10),          deCilJoen)
                           , (b, ((10^6)^10*10^3),     deCilJard)
                           --------------------------------
                           , (b, ((10^6)^(1+10)),      un          ++ deCilJoen)
                           , (b, ((10^6)^(1+10)*10^3), un          ++ deCilJard)

                           , (b, ((10^6)^(2+10)),      duo         ++ deCilJoen)
                           , (b, ((10^6)^(2+10)*10^3), duo         ++ deCilJard)

                           , (b, ((10^6)^(3+10)),       tre         ++ deCilJoen)
                           , (b, ((10^6)^(3+10)*10^3),  tre         ++ deCilJard)

                           , (b, ((10^6)^(4+10)),       quattuor    ++ deCilJoen)
                           , (b, ((10^6)^(4+10)*10^3),  quattuor    ++ deCilJard)

                           , (b, ((10^6)^(5+10)),       quin        ++ deCilJoen)
                           , (b, ((10^6)^(5+10)*10^3),  quin        ++ deCilJard)

                           , (b, ((10^6)^(6+10)),       sex         ++ deCilJoen)
                           , (b, ((10^6)^(6+10)*10^3),  sex         ++ deCilJard)

                           , (b, ((10^6)^(7+10)),       sep ++ ten  ++ deCilJoen)
                           , (b, ((10^6)^(7+10)*10^3),  sep ++ ten  ++ deCilJard)

                           , (b, ((10^6)^(8+10)),       oc  ++ to   ++ deCilJoen)
                           , (b, ((10^6)^(8+10)*10^3),  oc  ++ to   ++ deCilJard)

                           , (b, ((10^6)^(9+10)),       no  ++ vem  ++ deCilJoen)
                           , (b, ((10^6)^(9+10)*10^3),  no  ++ vem  ++ deCilJard)
                           --------------------------------
                           , (b, ((10^6)^(2*10)),       vi          ++ ginTilJoen)
                           , (b, ((10^6)^(2*10)*10^3),  vi          ++ ginTilJoen)

                           , (b, ((10^6)^(3*10)),       tri         ++ ginTilJoen)
                           , (b, ((10^6)^(3*10)*10^3),  tri         ++ ginTilJard)

                           , (b, ((10^6)^(4*10)),       quad ++ ra  ++ ginTilJoen)
                           , (b, ((10^6)^(4*10)*10^3),  quad ++ ra  ++ ginTilJard)

                           , (b, ((10^6)^(5*10)),       quin ++ dra ++ ginTilJoen)
                           , (b, ((10^6)^(5*10)*10^3),  quin ++ dra ++ ginTilJard)

                           , (b, ((10^6)^(6*10)),       sex  ++ "a" ++ ginTilJoen)
                           , (b, ((10^6)^(6*10)*10^3),  sex  ++ "a" ++ ginTilJard)

                           , (b, ((10^6)^(7*10)),       sep  ++ tua ++ ginTilJoen)
                           , (b, ((10^6)^(7*10)*10^3),  sep  ++ tua ++ ginTilJard)

                           , (b, ((10^6)^(8*10)),       oc   ++ to  ++ ginTilJoen)
                           , (b, ((10^6)^(8*10)*10^3),  oc   ++ to  ++ ginTilJard)

                           , (b, ((10^6)^(9*10)),       no   ++ na  ++ ginTilJoen)
                           , (b, ((10^6)^(9*10)*10^3),  no   ++ na  ++ ginTilJard)
                           --------------------------------
                           , (b, ((10^6)^10^2),         cen ++ til  ++ joen)
                           , (b, ((10^6)^10^2*10^3),    cen ++ til  ++ jard)
                           --------------------------------
                           , (b, ((10^6)^10^3),         mil ++ lil  ++ joen)
                           , (b, ((10^6)^10^3*10^3),    mil ++ lil  ++ jard)
                           --------------------------------
                           -- , (10^10^100,            googol ++ "plex")
                           ]
               }
    where
      vijf  = "vijf"
      zes   = "zes"
      zeven = "zeven"
      acht  = "acht"
      negen = "negen"

      tien  = "tien"

      tig   = "tig"

      joen  = "joen"
      jard  = "jard"

      quad  = "quad"
      quin  = "quin"
      sex   = "sex"
      sep   = "sep"
      oc    = "oc"; to = "to"
      no    = "no"

      deCil = "de"   ++ cil

      cil   = "c"    ++ il
      mil   = "m"    ++ il
      bil   = "b"    ++ il
      ril   = "r"    ++ il
      til   = "t"    ++ il
      nil   = "n"    ++ il
      lil   = "l"    ++ il
      tril  = "tr"   ++ il

      il = "il"

      deCilJoen = deCil ++ joen
      deCilJard = deCil ++ jard

      un       = "un"
      duo      = "duo"
      tre      = "tre"
      quattuor = "quattuor"

      ten = "ten"
      vem = "vem"

      ginTil = "gin" ++ til

      ginTilJoen = ginTil ++ joen
      ginTilJard = ginTil ++ jard

      vi  = "vi"
      tri = "tri"

      ra  = "ra"
      dra = "dra"
      tua = "tua"
      na  = "na"

      cen = "cen"

      googol = "googol"
-}
