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
Also see: http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers1/

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

7654321 == 7    * 1000000 + (6  *100    + 5*10 + 4)    * 1000 +  3   *100 +   2*10 + 1
           zeven miljoen     zes honderd  vierenvijftig  duizend drie honderd eenentwintig
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

data SymbolContext = EmptyContext
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
term :: Integer -> s -> NumSymbol s
term val s = NumSym Terminal val 1 (const s)

term' :: Integer -> (SymbolContext -> s) -> NumSymbol s
term' val fs = NumSym Terminal val 1 fs

add :: Integer -> Integer -> s -> NumSymbol s
add scope val s = NumSym Add scope val (const s)

mul :: Integer -> s -> NumSymbol s
mul val s = NumSym Mul val val (const s)

mul' :: Integer -> (SymbolContext -> s) -> NumSymbol s
mul' val fs = NumSym Mul val val fs

-- |Constructs a symbol representation based on the relation of the
--  symbol with the number 10.
--  The chosen representation depends on the context in which the
--  symbol is used:
--    d) default: x
--    a) additive: 10 + x
--    m) multiplicative: x * 10
tenForms :: s -> s -> s -> (SymbolContext -> s)
tenForms d a m ctx = case ctx of
                       RA 10 -> a
                       LM 10 -> m
                       _     -> d

tenForms' :: s -> s -> s -> s -> (SymbolContext -> s)
tenForms' d a mt mh ctx = case ctx of
                            RA 10  -> a
                            LM 10  -> mt
                            LM 100 -> mh
                            _      -> d

mulForms :: s -> s -> (SymbolContext -> s)
mulForms _ p (RM _) = p
mulForms s _ _      = s


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

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

cardinal :: NumConfig s -> Integer -> Maybe s
cardinal NumConfig {..} 0 = fmap (\sym -> symRepr sym EmptyContext) $ ncCardinal 0
cardinal NumConfig {..} x | x < 0     = fmap ncNeg $ go EmptyContext $ abs x
                          | otherwise = go EmptyContext x
    where go ctx n = do (NumSym _ v _ rv) <- ncCardinal n
                        case n `divMod` v of
                          (1, 0) -> return $ ncOne (v, rv ctx)
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
--
-------------------------------------------------------------------------------

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
test1 = [0, 1, 10, 11, 100, 101, 110, 111, 1000, 1001, 1010, 1011, 1100, 1101, 1110, 1111]
test2 = [0, 2, 10, 12, 20, 22, 100, 102, 112, 120, 122, 200, 202, 212, 220, 222, 2000, 2222]
test3 = [0, 3, 10, 13, 30, 33, 100, 103, 113, 130, 133, 300, 303, 313, 330, 333, 3000, 3333]
test4 = [0, 4, 10, 14, 40, 44, 100, 104, 114, 140, 144, 400, 404, 414, 440, 444, 4000, 4444]
test5 = [0, 5, 10, 15, 50, 55, 100, 105, 115, 150, 155, 500, 505, 515, 550, 555, 5000, 5555]
test6 = [0, 6, 10, 16, 60, 66, 100, 106, 116, 160, 166, 600, 606, 616, 660, 666, 6000, 6666]
test7 = [0, 7, 10, 17, 70, 77, 100, 107, 117, 170, 177, 700, 707, 717, 770, 777, 7000, 7777]
test8 = [0, 8, 10, 18, 80, 88, 100, 108, 118, 180, 188, 800, 808, 818, 880, 888, 8000, 8888]
test9 = [0, 9, 10, 19, 90, 99, 100, 109, 119, 190, 199, 900, 909, 919, 990, 999, 9000, 9999]

-------------------------------------------------------------------------------
-- Miscellaneous functions
-------------------------------------------------------------------------------

withSnd :: (a -> b -> c) -> (d, a) -> (e, b) -> c
withSnd f (_, x) (_, y) = f x y

d :: Integer -> Integer
d = (10 ^)

-------------------------------------------------------------------------------
-- Numeral configurations
-------------------------------------------------------------------------------

nummify :: Num n => NumConfig s -> NumConfig n
nummify nc@(NumConfig {..}) = NumConfig { ncCardinal = fmap transformSym . ncCardinal
                                        , ncNeg      = negate
                                        , ncOne      = snd
                                        , ncAdd      = withSnd (+)
                                        , ncMul      = withSnd (*)
                                        }
    where
      transformSym :: (Num n) => NumSymbol s -> NumSymbol n
      transformSym sym = sym { symRepr = const . fromInteger $ symVal sym}

-------------------------------------------------------------------------------

newtype NS s = NS {unS :: s} deriving (Show,  Eq)

instance (Eq s, Show s, IsString s, Joinable s) => Num (NS s) where
    fromInteger = NS . fromString . show

    (+) = bin "+"
    (-) = bin "-"
    (*) = bin "*"

    negate = un "negate"
    abs    = un "abs"
    signum = un "signum"

un :: (IsString s, Joinable s) => s -> (NS s -> NS s)
un fun x = NS (fun <+> unS x)

bin :: (IsString s, Joinable s) => s -> (NS s -> NS s -> NS s)
bin op x y = NS (p (unS x <> op <> unS y))

p s = "(" <> s <> ")"

instance Stringable s => Stringable (NS s) where
    toString = toString . unS

testNSS :: Test (NS String)
testNSS = test

testNumS :: Test String
testNumS = testNSS . nummify

-------------------------------------------------------------------------------

newtype Lst s = Lst {unLst :: [s]}

instance Joinable s => Joinable (Lst s) where
    x <>  y = Lst $ appendUnionWith (<>) (unLst x) (unLst y)
    x <+> y = Lst $ unLst x ++ unLst y

-- | appendUnionWith f [a, b, c] [d, e, f] => [a, b, c `f` d, e, f]
appendUnionWith :: (a -> a -> a) -> [a] -> [a] -> [a]
appendUnionWith _ []     ys     = ys
appendUnionWith _ xs     []     = xs
appendUnionWith f [x]    (y:ys) = x `f` y : ys
appendUnionWith f (x:xs) ys     = x : appendUnionWith f xs ys

instance IsString s => IsString (Lst s) where
    fromString s = Lst [fromString s]

instance Stringable s => Stringable (Lst s) where
    toString = show . map toString . unLst

testLst :: Test (Lst String)
testLst = test

-------------------------------------------------------------------------------

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
          , term' 2       $ tenForms "twee" "twin" "twee"
          , term' 3       $ tenForms "drie" "der"  "der"
          , term' 4       $ tenForms "vier" "veer" "veer"
          , term  5       "vijf"
          , term  6       "zes"
          , term  7       "zeven"
          , term' 8       $ tenForms "acht" "tach" "acht"
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
nl = NumConfig { ncNeg      = ("min" <+>)
               , ncOne      = snd
               , ncAdd      = nlAdd
               , ncMul      = nlMul
               , ncCardinal = findSym nlTable
               }

-------------------------------------------------------------------------------

enNeg :: (IsString s, Joinable s) => Neg s
enNeg = ("minus" <+>)

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
          , term' 2       $ tenForms "two"   "twen" "two"
          , term' 3       $ tenForms "three" "thir" "thir"
          , term' 4       $ tenForms "four"  "for"  "four"
          , term' 5       $ tenForms "five"  "fif"  "fif"
          , term  6       "six"
          , term  7       "seven"
          , term' 8       $ tenForms "eight" "eigh" "eigh"
          , term  9       "nine"
          , mul'  10      $ \ctx -> case ctx of
                                      LA _ -> "teen"
                                      RM _ -> "ty"
                                      _    -> "ten"
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

deTable :: (IsString s, Joinable s) => [NumSymbol s]
deTable = [ term  0        "null"
          , term  1        "eins"
          , term' 2        $ tenForms "zwei" "zwei" "zwan"
          , term  3        "drei"
          , term  4        "vier"
          , term  5        "fünf"
          , term  6        "sechs"
          , term' 7        $ tenForms "sieben" "sieb" "sieb"
          , term  8        "acht"
          , term  9        "neun"
          , mul'  10       $ mulForms "zehn" "zig"
          , term  11       "elf"
          , term  12       "zwölf"
          , add   30    10 "dreißig"
          , mul   100      "hundert"
          , mul   1000     "tausend"
          , mul   (d 6)    "million"
          , mul   (d 9)    "milliarde"
          ]

de :: (IsString s, Joinable s) => NumConfig s
de = NumConfig { ncNeg      = ("minus" <+>)
               , ncOne      = deOne
               , ncAdd      = deAdd
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym deTable
               }

-------------------------------------------------------------------------------

-- Sources:
--   http://en.wikibooks.org/wiki/Swedish/Numerals
--   http://longstrom.com/swedishtoenglish.htm#numbers

seAdd :: (IsString s, Joinable s) => Add s
seAdd (x, x') (_, y') | x < 20    = y' <> x'
                      | otherwise = x' <> y'

seTable :: (IsString s, Joinable s) => [NumSymbol s]
seTable = [ term  0        "noll"
          , term  1        "ett"
          , term  2        "två"
          , term' 3        $ tenForms "tre"  "tret" "tret"
          , term' 4        $ tenForms "fyra" "fjor" "fyr"
          , term  5        "fem"
          , term  6        "sex"
          , term' 7        $ tenForms "sju"  "sjut" "sjut"
          , term' 8        $ tenForms "åtta" "ar"   "åt"
          , term' 9        $ tenForms "nio"  "nit"  "nit"
          , mul'  10       $ \ctx -> case ctx of
                                       LA _  -> "ton"
                                       _     -> "tio"
          , term  11       "elva"
          , term  12       "tolv"
          , add   20    10 "tjugo"
          , mul   100      "hundra"
          , mul   (d 3)    "tusen"
          , mul'  (d 6)    $ mulForms "miljon" "miljoner"
          , mul'  (d 9)    $ mulForms "miljard" "miljarder"
          ]

se :: (IsString s, Joinable s) => NumConfig s
se = NumConfig { ncNeg      = error "seNeg: undefined"
               , ncOne      = snd
               , ncAdd      = seAdd
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym seTable
               }

-------------------------------------------------------------------------------

-- Sources:
--   http://en.wikibooks.org/wiki/Norwegian_Numbers

noOne :: (IsString s, Joinable s) => One s
noOne (v, vs) | v >= (d 6) = "én" <+> vs
              | otherwise  = vs

-- TODO: What are the rules for conjunction in Norse? When do you put
-- "og" between numbers?
noAdd :: (IsString s, Joinable s) => Add s
noAdd (x, x') (_, y') | x < 20    = y' <> x'
                      | otherwise = x' <> y'

noTable :: (IsString s, Joinable s) => [NumSymbol s]
noTable = [ term  0        "null"
          , term  1        "én"
          , term  2        "to"
          , term' 3        $ tenForms "tre"  "tret" "tret"
          , term' 4        $ tenForms "fire" "fjor" "før"
          , term  5        "fem"
          , term  6        "seks"
          , term' 7        $ tenForms "sju"  "syt" "syt"
          , term' 8        $ tenForms "åtte" "at"  "åt"
          , term' 9        $ tenForms "ni"   "nit" "nit"
          , mul'  10       $ \ctx -> case ctx of
                                       LA _ -> "ten"
                                       _    -> "ti"
          , term  11       "elleve"
          , term  12       "tolv"
          , add   20    10 "tjue"
          , mul   100      "hundre"
          , mul   (d 3)    "tusen"
          , mul'  (d 6)    $ mulForms "million"  "millioner"
          , mul'  (d 9)    $ mulForms "milliard" "milliarder"
          ]

no :: (IsString s, Joinable s) => NumConfig s
no = NumConfig { ncNeg      = error "noNeg: undefined"
               , ncOne      = noOne
               , ncAdd      = noAdd
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym noTable
               }

-------------------------------------------------------------------------------

laAdd :: (IsString s, Joinable s) => Add s
laAdd (x, x') (_, y') | x == 10   = y' <> x'
                      | otherwise = x' <+> y'

laMul :: (IsString s, Joinable s) => Mul s
laMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

laTable :: (IsString s, Joinable s) => [NumSymbol s]
laTable = [ term  0         "nulla"
          , term' 1         $ tenForms  "unus"     "un"       "unus"
          , term' 2         $ tenForms' "duo"      "duo"      "vi"      "du"
          , term' 3         $ tenForms' "tres"     "tre"      "tri"     "tre"
          , term' 4         $ tenForms' "quattuor" "quattuor" "quadra"  "quadri"
          , term' 5         $ tenForms' "quinque"  "quin"     "quinqua" "quin"
          , term' 6         $ tenForms' "sex"      "se"       "sexa"    "ses"
          , term' 7         $ tenForms' "septem"   "septen"   "septua"  "septin"
          , term  8                     "octo"
          , term' 9         $ tenForms' "novem"    "novem"    "nona"    "non"
          , mul'  10        $ \ctx -> case ctx of
                                        LA _ -> "decim"
                                        RM 2 -> "ginti"
                                        RM _ -> "ginta"
                                        _    -> "decem"
          , term  18        "duodeviginti"
          , term  19        "undeviginti"
          , term  28        "duodetriginta"
          , term  29        "undetriginta"
          , term  38        "duodequadraginta"
          , term  39        "undequadraginta"
          , term  48        "duodequinquaginta"
          , term  49        "undequinquaginta"
          , term  58        "duodesexaginta"
          , term  59        "undesexaginta"
          , term  68        "duodeseptuaginta"
          , term  69        "undeseptuaginta"
          , term  78        "duodeoctoginta"
          , term  79        "undeoctoginta"
          , term  88        "duodenonaginta"
          , term  89        "undenonaginta"
          , term  98        "duodecentum"
          , term  99        "undecentum"
          , mul'  100       $ \ctx -> case ctx of
                                        RM n | n `elem` [2, 3, 6] -> "centi"
                                             | otherwise          -> "genti"
                                        _                         -> "centum"
          , mul'  1000      $ mulForms "mille" "millia"
          , term  (d 6)     "decies centena milia"
          ]

la :: (IsString s, Joinable s) => NumConfig s
la = NumConfig { ncNeg      = error "laNeg: undefined"
               , ncOne      = snd
               , ncAdd      = laAdd
               , ncMul      = laMul
               , ncCardinal = findSym laTable
               }

-------------------------------------------------------------------------------

frAdd :: (IsString s, Joinable s) => Add s
frAdd (x, x') (y, y') | x == 10 && y < 7 = y' <> x'
                      | x < 80 && y == 1 = x' <+> "et" <+> y'
                      | x < 100          = x' <-> y'
                      | otherwise        = x' <+> y'

frMul :: (IsString s, Joinable s) => Mul s
frMul (_, x') (y, y') | y == 10   = x' <> y'
                      | otherwise = x' <+> y'

frTable :: (IsString s, Joinable s) => [NumSymbol s]
frTable = [ term  0         "zéro"
          , term' 1         $ tenForms "un"     "un"     "on"
          , term' 2         $ tenForms "deux"   "deux"   "dou"
          , term' 3         $ tenForms "trois"  "trei"   "tren"
          , term' 4         $ tenForms "quatre" "quator" "quar"
          , term' 5         $ tenForms "cinq"   "quin"   "cinqu"
          , term' 6         $ tenForms "six"    "sei"    "soix"
          , term  7         "sept"
          , term  8         "huit"
          , term  9         "neuf"
          , mul'  10        $ \ctx -> case ctx of
                                        LA n | n < 7     -> "ze"
                                             | otherwise -> "dix"
                                        RM 3 -> "te"
                                        RM _ -> "ante"
                                        _    -> "dix"
          , add   20    10  "vingt"
          , add   60    20  "soixante"
          , term  71        "soixante et onze"
          , term  80        "quatre-vingts"
          , add   80    20  "quatre-vingt"
          , mul'  100       $ mulForms "cent" "cents"
          , mul   1000      "mille"
          , mul   (d 6)     "million"
          , mul   (d 9)     "millard"
          ]

fr :: (IsString s, Joinable s) => NumConfig s
fr = NumConfig { ncNeg      = ("moins" <+>)
               , ncOne      = snd
               , ncAdd      = frAdd
               , ncMul      = frMul
               , ncCardinal = findSym frTable
               }

-------------------------------------------------------------------------------

-- Sources:
--   http://italian.about.com/library/weekly/aa042600a.htm

itAdd :: (IsString s, Joinable s) => Add s
itAdd (x, x') (y, y') | x == 10 && y < 7 = y' <> x'
                      | y == 3    = x' <> "tré"
                      | otherwise = x' <> y'

itMul :: (IsString s, Joinable s) => Mul s
itMul (_, x') (y, y') | y < d 6   = x' <> y'
                      | otherwise = x' <+> y'

itTable :: (IsString s, Joinable s) => [NumSymbol s]
itTable = [ term  0           "zero"
          , term' 1           $ tenForms "uno"     "un"      "uno"
          , term' 2           $ tenForms "due"     "do"      "due"
          , term' 3           $ tenForms "tre"     "tre"     "ten"
          , term' 4           $ tenForms "quattro" "quattor" "quar"
          , term' 5           $ tenForms "cinque"  "quin"    "cinqu"
          , term' 6           $ tenForms "sei"     "se"      "sess"
          , term' 7           $ tenForms "sette"   "assette" "sett"
          , term' 8           $ tenForms "otto"    "otto"    "ott"
          , term' 9           $ tenForms "nove"    "annove"  "nove"
          , mul'  10          $ \ctx -> case ctx of
                                          LA _ -> "dici"
                                          RM 3 -> "ta"
                                          RM _ -> "anta"
                                          _    -> "dieci"
          , add   20    10    "venti"
          , term  21          "ventuno"
          , term  28          "ventotto"
          , term  31          "trentuno"
          , term  38          "trentotto"
          , term  41          "quarantuno"
          , term  48          "quarantotto"
          , term  51          "cinquantuno"
          , term  58          "cinquantotto"
          , term  61          "sessantuno"
          , term  68          "sessantotto"
          , term  71          "settantuno"
          , term  78          "settantotto"
          , term  81          "ottantuno"
          , term  88          "ottantotto"
          , term  91          "novantuno"
          , term  98          "novantotto"
          , mul   100         "cento"
          , mul'  1000        $ mulForms "mille"    "mila"
          , mul'  (d 6)       $ mulForms "milione"  "milioni"
          , mul'  (d 9)       $ mulForms "miliardo" "miliardi"
          ]

it :: (IsString s, Joinable s) => NumConfig s
it = NumConfig { ncNeg      = error "itNeg: undefined"
               , ncOne      = snd
               , ncAdd      = itAdd
               , ncMul      = itMul
               , ncCardinal = findSym itTable
               }

-------------------------------------------------------------------------------

-- Sources:
--   http://spanish.about.com/cs/forbeginners/a/cardinalnum_beg.htm

spAdd :: (IsString s, Joinable s) => Add s
spAdd (x, x') (y, y') | x == 10 && y < 6 = y' <> x'
                      | x == 10    = x' <> y'
                      | x < 100    = x' <+> "y" <+> y'
                      | otherwise  = x' <+> y'

spMul :: (IsString s, Joinable s) => Mul s
spMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

spTable :: (IsString s, Joinable s) => [NumSymbol s]
spTable = [ term  0         "cero"
          , term' 1         $ tenForms  "uno"    "on"    "uno"
          , term' 2         $ tenForms  "dos"    "do"    "dos"
          , term' 3         $ tenForms  "tres"   "tre"   "trein"
          , term' 4         $ tenForms  "cuatro" "cator" "cuaren"
          , term' 5         $ tenForms  "cinco"  "quin"  "cincuen"
          , term' 6         $ tenForms  "seis"   "seis"  "sesen"
          , term' 7         $ tenForms' "siete"  "siete" "seten" "sete"
          , term' 8         $ tenForms  "ocho"   "ocho"  "ochen"
          , term' 9         $ tenForms' "nueve"  "nueve" "noven" "novo"
          , mul'  10        $ \ctx -> case ctx of
                                        LA n | n < 6     -> "ce"
                                             | otherwise -> "dieci"
                                        RM _ -> "ta"
                                        _    -> "diez"
          , add   20    10  "veinte"
          , mul'  100       $ mulForms "ciento" "cientos"
          , add   500   100 "quinientos"
          , mul   1000      "mil"
          , mul'  (d 6)     $ mulForms "millón" "millones"
          ]

sp :: (IsString s, Joinable s) => NumConfig s
sp = NumConfig { ncNeg      = error "spNeg: undefined"
               , ncOne      = snd
               , ncAdd      = spAdd
               , ncMul      = spMul
               , ncCardinal = findSym spTable
               }

-------------------------------------------------------------------------------

-- Sources:
--   http://www.sonia-portuguese.com/text/numerals.htm
--   http://www.smartphrase.com/Portuguese/po_numbers_voc.shtml

ptOne :: (IsString s, Joinable s) => One s
ptOne (x, x') | x <= 1000 = x'
              | otherwise = "um" <+> x'

-- TODO: When to use "e" is still unclear.
ptAdd :: (IsString s, Joinable s) => Mul s
ptAdd (x, x') (_, y') | x == 10   = y' <> x'
                      | otherwise = x' <+> "e" <+> y'

ptMul :: (IsString s, Joinable s) => Mul s
ptMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

ptTable :: (IsString s, Joinable s) => [NumSymbol s]
ptTable = [ term  0             "zero"
          , term' 1             $ tenForms "um"     "on"    "um"
          , term' 2             $ tenForms "dois"   "do"    "dois"
          , term' 3             $ tenForms "três"   "tre"   "trin"
          , term' 4             $ tenForms "quatro" "cator" "quar"
          , term' 5             $ tenForms "cinco"  "quin"  "cinqü"
          , term' 6             $ tenForms "seis"   "seis"  "sess"
          , term' 7             $ tenForms "sete"   "sete"  "set"
          , term' 8             $ tenForms "oito"   "oito"  "oit"
          , term' 9             $ tenForms "nove"   "nove"  "nov"
          , mul'  10            $ \ctx -> case ctx of
                                            LA _ -> "ze"
                                            RM 3 -> "ta"
                                            RM _ -> "enta"
                                            _    -> "dez"
          , term  16            "dezesseis"
          , term  17            "dezessete"
          , term  18            "dezoito"
          , term  19            "dezenove"
          , add   20    10      "vinte"
          , mul'  100           $ \ctx -> case ctx of
                                            RM _ -> "centos"
                                            LA _ -> "cento"
                                            _    -> "cem"
          , add   200   100     "duzentos"
          , add   300   100     "trezentos"
          , add   500   100     "quinhentos"
          , mul   1000          "mil"
          , mul'  (d 6)         $ mulForms "milhão"  "milhões"
          , mul'  (d 9)         $ mulForms "bilhão"  "bilhões"
          , mul'  (d 12)        $ mulForms "trilhão" "trilhões"
          ]

pt :: (IsString s, Joinable s) => NumConfig s
pt = NumConfig { ncNeg      = error "ptNeg: undefined"
               , ncOne      = ptOne
               , ncAdd      = ptAdd
               , ncMul      = ptMul
               , ncCardinal = findSym ptTable
               }

-------------------------------------------------------------------------------

jaOne :: (IsString s, Joinable s) => One s
jaOne (v, vs) | v < 100 || (300 >= v && v < 400) = vs
              | otherwise = "ichi" <-> vs

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
ja = NumConfig { ncNeg      = ("mainasu" <+>)
               , ncOne      = jaOne
               , ncAdd      = withSnd (<+>)
               , ncMul      = withSnd (<->)
               , ncCardinal = findSym jaTable
               }

-------------------------------------------------------------------------------

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
eo = NumConfig { ncNeg      = error "eoNeg: undefined"
               , ncOne      = snd
               , ncAdd      = withSnd (<+>)
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym eoTable
               }
