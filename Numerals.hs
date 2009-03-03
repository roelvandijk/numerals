-- -*- coding: utf-8 -*-

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Numerals
    ( SymbolType(..)
    , NumSymbol(..)
    , NumConfig(..)
    , term, add, mul

    , cardinal

    -- *West Germanic
    , nl, enShort, enLong, de
    -- *North Germanic
    , se, no
    -- *Romance
    , la, fr, it, sp, pt
    -- *Japonic
    , ja
    -- *Constructed
    , eo
    )
    where


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
import Data.Monoid
import qualified Data.DString as DS
import Data.String
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Text.PrettyPrint as PP

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data SymbolType = Terminal | Add | Mul deriving Show

data SymbolContext = O
                   | LA Integer
                   | RA Integer
                   | LM Integer
                   | RM Integer
                     deriving Show

data NumSymbol s = NumSym { symType  :: SymbolType
                          , symVal   :: Integer
                          , symScope :: Integer
                          , symRepr  :: SymbolContext -> s
                          }

-- Easy construction of NumSymbols
term :: (IsString s, Joinable s) => Integer -> s -> NumSymbol s
term val s = NumSym Terminal val 1 (const s)

term' :: (IsString s, Joinable s) => Integer -> (SymbolContext -> s) -> NumSymbol s
term' val fs = NumSym Terminal val 1 fs

add :: (IsString s, Joinable s) => Integer -> Integer -> s -> NumSymbol s
add scope val s = NumSym Add scope val (const s)

mul :: (IsString s, Joinable s) => Integer -> s -> NumSymbol s
mul val s = NumSym Mul val val (const s)

mul' :: (IsString s, Joinable s) => Integer -> (SymbolContext -> s) -> NumSymbol s
mul' val fs = NumSym Mul val val fs

-- Use 'foo' instead of 'const' in the previous functions to expose
-- some of the structure of numerals.
-- foo s = \ctx -> "(" <> fromString (show ctx) <+> s <> ")"


data NumConfig s = NumConfig { ncCardinal :: Integer -> Maybe (NumSymbol s)
                             , ncNeg      :: Neg s
                             , ncOne      :: One s
                             , ncAdd      :: Add s
                             , ncMul      :: Mul s
                             }

type Neg s = s -> s
type One s = (Integer, s) -> s
type Add s = (Integer, s) -> (Integer, s) -> s
type Mul s = (Integer, s) -> (Integer, s) -> s


----------------------------------------

class Joinable s where
    (<>)  :: s -> s -> s
    (<+>) :: s -> s -> s

infixr 5 <>, <+>, <-> -- Same as ++

(<->) :: (Joinable s, IsString s) => s -> s -> s
x <-> y = x <> "-" <> y

instance Joinable String where
    (<>)    = mappend
    x <+> y = x <> " " <> y

instance Joinable B.ByteString where
    (<>)    = mappend
    x <+> y = x <> " " <> y

instance Joinable T.Text where
    (<>)    = mappend
    x <+> y = x <> " " <> y

instance Joinable ShowS where
    (<>)    = mappend
    x <+> y = x <> " " <> y

instance Joinable DS.DString where
    (<>)    = mappend
    x <+> y = x <> " " <> y

instance Joinable PP.Doc where
    (<>)  = (PP.<>)
    (<+>) = (PP.<+>)

----------------------------------------

class Stringable s where
    toString :: s -> String

instance Stringable String where
    toString = id

instance Stringable B.ByteString where
    toString = B.unpack

instance Stringable T.Text where
    toString = T.unpack

instance Stringable ShowS where
    toString s = s []

instance Stringable DS.DString where
    toString = DS.toString

instance Stringable PP.Doc where
    toString = PP.render

----------------------------------------

instance IsString ShowS where
    fromString = showString

instance IsString PP.Doc where
    fromString = PP.text

----------------------------------------

d :: Integer -> Integer
d = (10 ^)

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

cardinal :: NumConfig s -> Integer -> Maybe s
cardinal NumConfig {..} 0 = fmap (\sym -> symRepr sym O) $ ncCardinal 0
cardinal NumConfig {..} x | x < 0     = fmap ncNeg $ go O $ abs x
                          | otherwise = go O x
    where go ctx n = do (NumSym _ v _ rv) <- ncCardinal n
                        let vs = rv ctx
                        case n `divMod` v of
                          (1, 0) -> return $ ncOne (v, vs)
                          (1, r) -> do rs <- go (RA v) r
                                       return $ (v, ncOne (v, rv (LA r))) `ncAdd` (r, rs)
                          (q, r) | q >= v    -> Nothing
                                 | otherwise -> do qs <- go (LM v) q
                                                   if r == 0
                                                     then return $ (q, qs) `ncMul` (v, rv (RM q))
                                                     else do let qv = q * v
                                                             rs <- go (RA qv) r
                                                             return $ (qv, (q, qs) `ncMul` (v, rv (RM q))) `ncAdd` (r, rs)

findSym :: [NumSymbol s] -> Integer -> Maybe (NumSymbol s)
findSym []     _ = Nothing
findSym (e:es) n = go e e es
    where go :: NumSymbol s -> NumSymbol s -> [NumSymbol s] -> Maybe (NumSymbol s)
          go a m [] = stop a m
          go a m (x@(NumSym t v _ _) : xs)
              | v == n    = Just x
              | otherwise = case t of
                              Terminal        -> go a m xs
                              Add | v > n     -> stop a m
                                  | otherwise -> go x m xs
                              Mul | v > n     -> stop a m
                                  | otherwise -> go a x xs

          stop :: NumSymbol s -> NumSymbol s -> Maybe (NumSymbol s)
          stop a@(NumSym {..}) m | n < symVal + symScope = return a
                                 | otherwise             = return m

-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------

type Test s = NumConfig s -> [Integer] -> IO ()

test :: Stringable s => Test s
test nc = mapM_ (putStrLn . pretty)
    where pretty n = show n ++ " == " ++ (maybe "-" id $ fmap toString $ cardinal nc n)

testS :: Test String
testS = test

testBS :: Test B.ByteString
testBS = test

testT :: Test T.Text
testT = test

testSS :: Test ShowS
testSS = test

testDS :: Test DS.DString
testDS = test

testDoc :: Test PP.Doc
testDoc = test

testDocWithStyle :: PP.Style -> NumConfig PP.Doc -> [Integer] -> IO ()
testDocWithStyle s nc = mapM_ (putStrLn . pretty)
    where pretty n = show n ++ " == " ++ (maybe "-" id $ fmap (PP.renderStyle s) $ cardinal nc n)

-- test data
test1 = [0, 1, 10, 11, 10, 11, 100, 101, 111, 110, 111, 100, 101, 111, 110, 111, 1000, 1111]
test2 = [0, 2, 10, 12, 20, 22, 100, 102, 112, 120, 122, 200, 202, 212, 220, 222, 2000, 2222]
test3 = [0, 3, 10, 13, 30, 33, 100, 103, 113, 130, 133, 300, 303, 313, 330, 333, 3000, 3333]
test4 = [0, 4, 10, 14, 40, 44, 100, 104, 114, 140, 144, 400, 404, 414, 440, 444, 4000, 4444]
test5 = [0, 5, 10, 15, 50, 55, 100, 105, 115, 150, 155, 500, 505, 515, 550, 555, 5000, 5555]
test6 = [0, 6, 10, 16, 60, 66, 100, 106, 116, 160, 166, 600, 606, 616, 660, 666, 6000, 6666]
test7 = [0, 7, 10, 17, 70, 77, 100, 107, 117, 170, 177, 700, 707, 717, 770, 777, 7000, 7777]
test8 = [0, 8, 10, 18, 80, 88, 100, 108, 118, 180, 188, 800, 808, 818, 880, 888, 8000, 8888]
test9 = [0, 9, 10, 19, 90, 99, 100, 109, 119, 190, 199, 900, 909, 919, 990, 999, 9000, 9999]

-------------------------------------------------------------------------------
-- Numeral configurations
-------------------------------------------------------------------------------

nlNeg :: (IsString s, Joinable s) => Neg s
nlNeg s = "min" <+> s

nlOne :: One s
nlOne = snd

nlAdd :: (IsString s, Joinable s) => Add s
nlAdd (x, x') (y, y') | x < 20    = y' <> x'
                      | x < 100   = y' <> (if y == 2 || y == 3
                                           then "ën"
                                           else "en")
                                       <> x'
                      | otherwise = x' <+> y'

nlMul :: (IsString s, Joinable s) => Mul s
nlMul (_, x') (y, y') | y <= 10   = x' <> y'
                      | otherwise = x' <+> y'

nlTable :: (IsString s, Joinable s) => [NumSymbol s]
nlTable = [ term  0       "nul"
          , term  1       "één"
          , term' 2       $ \ctx -> case ctx of
                                      LM 10 -> "twin"
                                      _     -> "twee"
          , term' 3       $ \ctx -> case ctx of
                                      RA 10 -> "der"
                                      LM 10 -> "der"
                                      _     -> "drie"
          , term' 4       $ \ctx -> case ctx of
                                      RA 10 -> "veer"
                                      LM 10 -> "veer"
                                      _     -> "vier"
          , term  5       "vijf"
          , term  6       "zes"
          , term  7       "zeven"
          , term' 8       $ \ctx -> case ctx of
                                      LM 10 -> "tach"
                                      _     -> "acht"
          , term  9       "negen"
          , mul'  10      $ \ctx -> case ctx of
                                      RM _ -> "tig"
                                      _    -> "tien"
          , term 11       "elf"
          , term 12       "twaalf"
          , mul  (d 2)    "honderd"
          , mul  (d 3)    "duizend"
          , mul  (d 6)    "miljoen"              -- m 1         where m = (d 6 ^)
          , mul  (d 9)    "miljard"              -- m 1  * d 3
          , mul  (d 12)   "biljoen"              -- m 2
          , mul  (d 15)   "biljard"              -- m 2  * d 3
          , mul  (d 18)   "triljoen"             -- m 3
          , mul  (d 21)   "triljard"             -- m 3  * d 3
          , mul  (d 24)   "quadriljoen"          -- m 4
          , mul  (d 27)   "quadriljard"          -- m 4  * d 3
          , mul  (d 30)   "quintiljoen"          -- m 5
          , mul  (d 33)   "quintiljard"          -- m 5  * d 3
          , mul  (d 36)   "sextiljoen"           -- m 6
          , mul  (d 39)   "sextiljard"           -- m 6  * d 3
          , mul  (d 42)   "septiljoen"           -- m 7
          , mul  (d 45)   "septiljard"           -- m 7  * d 3
          , mul  (d 48)   "octiljoen"            -- m 8
          , mul  (d 51)   "octiljard"            -- m 8  * d 3
          , mul  (d 54)   "noniljoen"            -- m 9
          , mul  (d 57)   "noniljard"            -- m 9  * d 3
          , mul  (d 60)   "deciljoen"            -- m 10
          , mul  (d 63)   "deciljard"            -- m 10  * d 3
          , mul  (d 66)   "undeciljoen"          -- m 11
          , mul  (d 69)   "undeciljard"          -- m 11  * d 3
          , mul  (d 72)   "duodeciljoen"         -- m 12
          , mul  (d 75)   "duodeciljard"         -- m 12  * d 3
          , mul  (d 78)   "tredeciljoen"         -- m 13
          , mul  (d 81)   "tredeciljard"         -- m 13  * d 3
          , mul  (d 84)   "quattuordeciljoen"    -- m 14
          , mul  (d 87)   "quattuordeciljard"    -- m 14  * d 3
          , mul  (d 90)   "quindeciljoen"        -- m 15
          , mul  (d 93)   "quindeciljard"        -- m 15  * d 3
          , mul  (d 96)   "sexdeciljoen"         -- m 16
          , mul  (d 99)   "sexdeciljard"         -- m 16  * d 3
          , mul  (d 102)  "septendeciljoen"      -- m 17
          , mul  (d 105)  "septendeciljard"      -- m 17  * d 3
          , mul  (d 108)  "octodeciljoen"        -- m 18
          , mul  (d 111)  "octodeciljard"        -- m 18  * d 3
          , mul  (d 114)  "novemdeciljoen"       -- m 19
          , mul  (d 117)  "novemdeciljard"       -- m 19  * d 3
          , mul  (d 120)  "vigintiljoen"         -- m 20
          , mul  (d 123)  "vigintiljard"         -- m 20  * d 3
          , mul  (d 180)  "trigintiljoen"        -- m 30
          , mul  (d 183)  "trigintiljard"        -- m 30  * d 3
          , mul  (d 240)  "quadragintiljoen"     -- m 40
          , mul  (d 243)  "quadragintiljard"     -- m 40  * d 3
          , mul  (d 300)  "quindragintiljoen"    -- m 50
          , mul  (d 303)  "quindragintiljard"    -- m 50  * d 3
          , mul  (d 360)  "sexagintiljoen"       -- m 60
          , mul  (d 363)  "sexagintiljard"       -- m 60  * d 3
          , mul  (d 420)  "septuagintiljoen"     -- m 70
          , mul  (d 423)  "septuagintiljard"     -- m 70  * d 3
          , mul  (d 480)  "octogintiljoen"       -- m 80
          , mul  (d 483)  "octogintiljard"       -- m 80  * d 3
          , mul  (d 540)  "nonagintiljoen"       -- m 90
          , mul  (d 543)  "nonagintiljard"       -- m 90  * d 3

          , mul  (d 600)  "centiljoen"           -- m (d 2)
          , mul  (d 603)  "centiljard"           -- m (d 2) * d 3

          , mul  (d 6000) "milliljoen"           -- m (d 3)
          , mul  (d 6003) "milliljard"           -- m (d 3) * d 3
          ]

nl :: (IsString s, Joinable s) => NumConfig s
nl = NumConfig { ncNeg      = nlNeg
               , ncOne      = nlOne
               , ncAdd      = nlAdd
               , ncMul      = nlMul
               , ncCardinal = findSym nlTable
               }

-------------------------------------------------------------------------------

enNeg :: (IsString s, Joinable s) => Neg s
enNeg s = "minus" <+> s

enOne :: (IsString s, Joinable s) => One s
enOne (v,  vs) | v >= 100  = "one" <+> vs
               | otherwise = vs

enAdd :: (IsString s, Joinable s) => Add s
enAdd (x, x') (_, y') | x < 20    = y' <> x'
                      | x < 100   = x' <-> y'
                      | otherwise = x' <+> y'

enMul :: (IsString s, Joinable s) => Mul s
enMul (_, x') (y, y') | y == 10   = x' <> y'
                      | otherwise = x' <+> y'

enTable :: (IsString s, Joinable s) => [NumSymbol s]
enTable = [ term  0       "zero"
          , term  1       "one"
          , term' 2       $ \ctx -> case ctx of
                                      LM 10   -> "twen"
                                      _       -> "two"
          , term' 3       $ \ctx -> case ctx of
                                      RA 10   -> "thir"
                                      LM 10   -> "thir"
                                      _       -> "three"
          , term' 4       $ \ctx -> case ctx of
                                      LM 10   -> "for"
                                      _       -> "four"
          , term' 5       $ \ctx -> case ctx of
                                      RA 10   -> "fif"
                                      LM 10   -> "fif"
                                      _       -> "five"
          , term  6       "six"
          , term  7       "seven"
          , term' 8       $ \ctx -> case ctx of
                                      RA 10   -> "eigh"
                                      LM 10   -> "eigh"
                                      _       -> "eight"
          , term  9       "nine"
          , mul'  10      $ \ctx -> case ctx of
                                      LA _    -> "teen"
                                      RM _    -> "ty"
                                      _       -> "ten"
          , term  11      "eleven"
          , term  12      "twelve"
          , mul   100     "hundred"
          , mul   1000    "thousand"
          ]

enShortTable :: (IsString s, Joinable s) => [NumSymbol s]
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

enShort :: (IsString s, Joinable s) => NumConfig s
enShort = NumConfig { ncNeg      = enNeg
                    , ncOne      = enOne
                    , ncAdd      = enAdd
                    , ncMul      = enMul
                    , ncCardinal = findSym enShortTable
                    }

enLongTable :: (IsString s, Joinable s) => [NumSymbol s]
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

enLong :: (IsString s, Joinable s) => NumConfig s
enLong = NumConfig { ncNeg      = enNeg
                   , ncOne      = enOne
                   , ncAdd      = enAdd
                   , ncMul      = enMul
                   , ncCardinal = findSym enLongTable
                   }

-------------------------------------------------------------------------------

deNeg :: (IsString s, Joinable s) => Neg s
deNeg s = "minus" <+> s

deOne :: (IsString s, Joinable s) => One s
deOne (v, vs) | v >= (d 6) = "eine" <+> vs
              | v >= 100   = "ein"  <>  vs
              | otherwise  = vs

deAdd :: (IsString s, Joinable s) => Add s
deAdd (x, x') (y, y') | x < 20    = y' <> x'
                      | x < 100   = (if y == 1
                                     then "ein"
                                     else y') <> "und" <> x'
                      | otherwise = x' <> y'

deMul :: (IsString s, Joinable s) => Mul s
deMul (_, x') (y, y') | y < (d 6) = x' <> y'
                      | otherwise = x' <> y'

deTable :: (IsString s, Joinable s) => [NumSymbol s]
deTable = [ term  0        "null"
          , term  1        "eins"
          , term' 2        $ \ctx -> case ctx of
                                       LM 10 -> "zwan"
                                       _     -> "zwei"
          , term  3        "drei"
          , term  4        "vier"
          , term  5        "fünf"
          , term  6        "sechs"
          , term' 7        $ \ctx -> case ctx of
                                       RA 10 -> "sieb"
                                       LM 10 -> "sieb"
                                       _     -> "sieben"
          , term  8        "acht"
          , term  9        "neun"
          , mul'  10       $ \ctx -> case ctx of
                                       RM _ -> "zig"
                                       _    -> "zehn"
          , term  11       "elf"
          , term  12       "zwölf"
          , add   30    10 "dreißig"
          , mul   100      "hundert"
          , mul   1000     "tausend"
          , mul   (d 6)    "million"
          , mul   (d 9)    "milliarde"
          ]

de :: (IsString s, Joinable s) => NumConfig s
de = NumConfig { ncNeg      = deNeg
               , ncOne      = deOne
               , ncAdd      = deAdd
               , ncMul      = deMul
               , ncCardinal = findSym deTable
               }

-------------------------------------------------------------------------------

-- Sources:
--   http://en.wikibooks.org/wiki/Swedish/Numerals
--   http://longstrom.com/swedishtoenglish.htm#numbers

seNeg :: (IsString s, Joinable s) => Neg s
seNeg = error "seNeg: not defined yet you fool!"

seOne :: One s
seOne = snd

seAdd :: (IsString s, Joinable s) => Add s
seAdd (x, x') (_, y') | x < 20    = y' <> x'
                      | otherwise = x' <> y'

seMul :: (IsString s, Joinable s) => Mul s
seMul (_, x') (_, y') = x' <> y'

seTable :: (IsString s, Joinable s) => [NumSymbol s]
seTable = [ term  0           "noll"
          , term  1           "ett"
          , term  2           "två"
          , term' 3           $ \ctx -> case ctx of
                                          RA 10 -> "tret"
                                          LM 10 -> "tret"
                                          _     -> "tre"
          , term' 4           $ \ctx -> case ctx of
                                          RA 10 -> "fjor"
                                          LM 10 -> "fyr"
                                          _     -> "fyra"
          , term  5           "fem"
          , term  6           "sex"
          , term' 7           $ \ctx -> case ctx of
                                          RA 10 -> "sjut"
                                          LM 10 -> "sjut"
                                          _     -> "sju"
          , term' 8           $ \ctx -> case ctx of
                                          RA 10 -> "ar"
                                          LM 10 -> "åt"
                                          _     -> "åtta"
          , term' 9           $ \ctx -> case ctx of
                                          RA 10 -> "nit"
                                          LM 10 -> "nit"
                                          _     -> "nio"
          , mul'  10          $ \ctx -> case ctx of
                                          LA _  -> "ton"
                                          _     -> "tio"
          , term  11          "elva"
          , term  12          "tolv"
          , add   20    10    "tjugo"
          , mul   100         "hundra"
          , mul   (d 3)       "tusen"
          , add   (d 6) (d 6) "miljon"
          , mul   (d 6)       "miljoner"
          , add   (d 9) (d 9) "miljard"
          , mul   (d 9)       "miljarder"
          ]

se :: (IsString s, Joinable s) => NumConfig s
se = NumConfig { ncNeg      = seNeg
               , ncOne      = seOne
               , ncAdd      = seAdd
               , ncMul      = seMul
               , ncCardinal = findSym seTable
               }

-------------------------------------------------------------------------------

-- Sources:
--   http://en.wikibooks.org/wiki/Norwegian_Numbers

noNeg :: (IsString s, Joinable s) => Neg s
noNeg = error "noNeg: not defined yet you fool!"

noOne :: (IsString s, Joinable s) => One s
noOne (v, vs) | v >= (d 6) = "én" <+> vs
              | otherwise  = vs

noAdd :: (IsString s, Joinable s) => Add s
noAdd (x, x') (_, y') | x < 20    = y' <> x'
--                       | x == 100  = x' <+> "og" <+> y'
                      | otherwise = x' <> y'

noMul :: (IsString s, Joinable s) => Mul s
noMul (_, x') (_, y') = x' <> y'

noTable :: (IsString s, Joinable s) => [NumSymbol s]
noTable = [ term  0           "null"
          , term  1           "én"
          , term  2           "to"
          , term' 3           $ \ctx -> case ctx of
                                          RA 10 -> "tret"
                                          LM 10 -> "tret"
                                          _     -> "tre"
          , term' 4           $ \ctx -> case ctx of
                                          RA 10 -> "fjor"
                                          LM 10 -> "før"
                                          _     -> "fire"
          , term  5           "fem"
          , term  6           "seks"
          , term' 7           $ \ctx -> case ctx of
                                          RA 10 -> "syt"
                                          LM 10 -> "syt"
                                          _     -> "sju"
          , term' 8           $ \ctx -> case ctx of
                                          RA 10 -> "at"
                                          LM 10 -> "åt"
                                          _     -> "åtte"
          , term' 9           $ \ctx -> case ctx of
                                          RA 10 -> "nit"
                                          LM 10 -> "nit"
                                          _     -> "ni"
          , mul'  10          $ \ctx -> case ctx of
                                          LA _ -> "ten"
                                          _    -> "ti"
          , term  11          "elleve"
          , term  12          "tolv"
          , add   20    10    "tjue"
          , mul   100         "hundre"
          , mul   (d 3)       "tusen"
          , add   (d 6) (d 6) "million"
          , mul   (d 6)       "millioner"
          , add   (d 9) (d 9) "milliard"
          , mul   (d 9)       "milliarder"
          ]

no :: (IsString s, Joinable s) => NumConfig s
no = NumConfig { ncNeg      = noNeg
               , ncOne      = noOne
               , ncAdd      = noAdd
               , ncMul      = noMul
               , ncCardinal = findSym noTable
               }

-------------------------------------------------------------------------------

laNeg :: (IsString s, Joinable s) => Neg s
laNeg = error "laNeg: not defined yet you fool!"

laOne :: One s
laOne = snd

laAdd :: (IsString s, Joinable s) => Add s
laAdd (_, x') (_, y') = x' <+> y'

laMul :: (IsString s, Joinable s) => Mul s
laMul (_, x') (_, y') = x' <+> y'

laTable :: (IsString s, Joinable s) => [NumSymbol s]
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

la :: (IsString s, Joinable s) => NumConfig s
la = NumConfig { ncNeg      = laNeg
               , ncOne      = laOne
               , ncAdd      = laAdd
               , ncMul      = laMul
               , ncCardinal = findSym laTable
               }

-------------------------------------------------------------------------------

frNeg :: (IsString s, Joinable s) => Neg s
frNeg s = "moins" <+> s

frOne :: One s
frOne = snd

frAdd :: (IsString s, Joinable s) => Add s
frAdd (x, x') (y, y') | x < 80 && y == 1 = x' <+> "et" <+> y'
                      | x < 100          = x' <-> y'
                      | otherwise        = x' <+> y'

frMul :: (IsString s, Joinable s) => Mul s
frMul (_, x') (_, y') = x' <+> y'

frTable :: (IsString s, Joinable s) => [NumSymbol s]
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

fr :: (IsString s, Joinable s) => NumConfig s
fr = NumConfig { ncNeg      = frNeg
               , ncOne      = frOne
               , ncAdd      = frAdd
               , ncMul      = frMul
               , ncCardinal = findSym frTable
               }

-------------------------------------------------------------------------------

itNeg :: (IsString s, Joinable s) => Neg s
itNeg = error "itNeg: not defined yet you fool!"

itOne :: One s
itOne = snd

itAdd :: (IsString s, Joinable s) => Add s
itAdd (_, x') (y, y') | y == 3    = x' <> "tré"
                      | otherwise = x' <> y'

itMul :: (IsString s, Joinable s) => Mul s
itMul (_, x') (y, y') | y < d 6   = x' <> y'
                      | otherwise = x' <+> y'

itTable :: (IsString s, Joinable s) => [NumSymbol s]
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

it :: (IsString s, Joinable s) => NumConfig s
it = NumConfig { ncNeg      = itNeg
               , ncOne      = itOne
               , ncAdd      = itAdd
               , ncMul      = itMul
               , ncCardinal = findSym itTable
               }

-------------------------------------------------------------------------------

spNeg :: (IsString s, Joinable s) => Neg s
spNeg = error "spNeg: not defined yet you fool!"

spOne :: One s
spOne = snd

spAdd :: (IsString s, Joinable s) => Add s
spAdd (x, x') (_, y') | x < 100   =  x' <+> "y" <+> y'
                      | otherwise  = x' <+> y'

spMul :: (IsString s, Joinable s) => Mul s
spMul (_, x') (_, y') = x' <+> y'

spTable :: (IsString s, Joinable s) => [NumSymbol s]
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
          , add  100   100 "cien"
          , mul  100       "ciento"
          , add  500   100 "quinientos"
          , add  700   100 "setecientos"
          , add  900   100 "novocientos"
          , mul  1000      "mil"
          , mul  (d 6)     "un millón"
          ]

sp :: (IsString s, Joinable s) => NumConfig s
sp = NumConfig { ncNeg      = spNeg
               , ncOne      = spOne
               , ncAdd      = spAdd
               , ncMul      = spMul
               , ncCardinal = findSym spTable
               }

-------------------------------------------------------------------------------

-- Sources:
--   http://www.sonia-portuguese.com/text/numerals.htm
--   http://www.smartphrase.com/Portuguese/po_numbers_voc.shtml

ptNeg :: (IsString s, Joinable s) => Neg s
ptNeg = error "ptNeg: not defined yet you fool!"

ptOne :: (IsString s, Joinable s) => One s
ptOne (x, x') | x <= 1000 = x'
              | otherwise = "um" <+> x'

ptAdd :: (IsString s, Joinable s) => Add s
ptAdd (_, x') (_, y') =  x' <+> "e" <+> y'

ptMul :: (IsString s, Joinable s) => Mul s
ptMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

ptTable :: (IsString s, Joinable s) => [NumSymbol s]
ptTable = [ term 0            "zero"
          , term 1            "um"
          , term 2            "dois"
          , term 3            "três"
          , term 4            "quatro"
          , term 5            "cinco"
          , term 6            "seis"
          , term 7            "sete"
          , term 8            "oito"
          , term 9            "nove"
          , mul  10           "dez"
          , term 11           "onze"
          , term 12           "doze"
          , term 13           "treze"
          , term 14           "catorze"
          , term 15           "quinze"
          , term 16           "dezesseis"
          , term 17           "dezessete"
          , term 18           "dezoito"
          , term 19           "dezenove"
          , add  20    10     "vinte"
          , add  30    10     "trinta"
          , add  40    10     "quarenta"
          , add  50    10     "cinqüenta"
          , add  60    10     "sessenta"
          , add  70    10     "setenta"
          , add  80    10     "oitenta"
          , add  90    10     "noventa"
          , term 100          "cem"
          , add  100   100    "cento"
          , mul  100          "centos"
          , add  200   100    "duzentos"
          , add  300   100    "trezentos"
          , add  500   100    "quinhentos"
          , mul  1000         "mil"
          , add (d 6)  (d 6)  "milhão"
          , mul (d 6)         "milhões"
          , add (d 9)  (d 9)  "bilhão"
          , mul (d 9)         "bilhões"
          , add (d 12) (d 12) "trilhão"
          , mul (d 12)        "trilhões"
          ]

pt :: (IsString s, Joinable s) => NumConfig s
pt = NumConfig { ncNeg      = ptNeg
               , ncOne      = ptOne
               , ncAdd      = ptAdd
               , ncMul      = ptMul
               , ncCardinal = findSym ptTable
               }

-------------------------------------------------------------------------------

jaNeg :: (IsString s, Joinable s) => Neg s
jaNeg s = "mainasu" <+> s

jaOne :: (IsString s, Joinable s) => One s
jaOne (v, vs) | v < 100 || (300 >= v && v < 400) = vs
              | otherwise = "ichi" <-> vs

jaAdd :: (IsString s, Joinable s) => Add s
jaAdd (_, x') (_, y') = x' <+> y'

jaMul :: (IsString s, Joinable s) => Mul s
jaMul (_, x') (_, y') = x' <-> y'

jaTable :: (IsString s, Joinable s) => [NumSymbol s]
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

ja :: (IsString s, Joinable s) => NumConfig s
ja = NumConfig { ncNeg      = jaNeg
               , ncOne      = jaOne
               , ncAdd      = jaAdd
               , ncMul      = jaMul
               , ncCardinal = findSym jaTable
               }

-------------------------------------------------------------------------------

eoNeg :: (IsString s, Joinable s) => Neg s
eoNeg = error "eoNeg: not defined yet you fool!"

eoOne :: One s
eoOne = snd

eoAdd :: (IsString s, Joinable s) => Add s
eoAdd (_, x') (_, y') = x' <+> y'

eoMul :: (IsString s, Joinable s) => Mul s
eoMul (_, x') (_, y') = x' <> y'

eoTable :: (IsString s, Joinable s) => [NumSymbol s]
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

eo :: (IsString s, Joinable s) => NumConfig s
eo = NumConfig { ncNeg      = eoNeg
               , ncOne      = eoOne
               , ncAdd      = eoAdd
               , ncMul      = eoMul
               , ncCardinal = findSym eoTable
               }
