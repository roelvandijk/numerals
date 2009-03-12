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
      en = in pairs (2), by the dozen (12)

Other counting bases
  * duodecimal
    12^1 = dozen
    12^2 = gross
    12^3 = great gross

Negative numbers:

nl: min
en: minus
fr: moins
de: minus
sp:
it:
se: minus
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

-- | Grammatical gender
data Gender = Masculine | Feminine deriving Show

data SymbolType = Terminal | Add | Mul deriving Show

data SymbolContext = EmptyContext
                   | LA Integer SymbolContext
                   | RA Integer SymbolContext
                   | LM Integer SymbolContext
                   | RM Integer SymbolContext
                     deriving Show

data NumSymbol s = NumSym { symType  :: SymbolType
                          , symVal   :: Integer
                          , symScope :: Integer
                          , symRepr  :: Gender -> SymbolContext -> s
                          }

-- Easy construction of NumSymbols
termG :: Integer -> (Gender -> SymbolContext -> s) -> NumSymbol s
termG val fs = NumSym Terminal val 1 fs

addG :: Integer -> Integer -> (Gender -> SymbolContext -> s) -> NumSymbol s
addG scope val fs = NumSym Add scope val fs

mulG :: Integer -> (Gender -> SymbolContext -> s) -> NumSymbol s
mulG val fs = NumSym Mul val val fs

term :: Integer -> (SymbolContext -> s) -> NumSymbol s
term val fs = termG val $ const fs

add :: Integer -> Integer -> (SymbolContext -> s) -> NumSymbol s
add scope val fs = addG scope val $ const fs

mul :: Integer -> (SymbolContext -> s) -> NumSymbol s
mul val fs = mulG val $ const fs

gender :: s -> s -> (Gender -> s)
gender m _ Masculine = m
gender _ f Feminine  = f



-- |Constructs a symbol representation based on the relation of the
--  symbol with the number 10.
--  The chosen representation depends on the context in which the
--  symbol is used:
--    d) default: x
--    a) additive: 10 + x
--    m) multiplicative: x * 10
tenForms :: s -> s -> s -> (SymbolContext -> s)
tenForms d a m ctx = case ctx of
                       RA 10 _ -> a
                       LM 10 _ -> m
                       _       -> d

tenFormsG :: (Gender -> s) -> (Gender -> s) -> (Gender -> s) -> (Gender -> SymbolContext -> s)
tenFormsG d a m g ctx = case ctx of
                          RA 10 _ -> a g
                          LM 10 _ -> m g
                          _       -> d g

tenForms' :: s -> s -> s -> s -> (SymbolContext -> s)
tenForms' d a mt mh ctx = case ctx of
                            RA 10  _ -> a
                            LM 10  _ -> mt
                            LM 100 _ -> mh
                            _        -> d

mulForms :: s -> s -> (SymbolContext -> s)
mulForms _ p (RM _ _) = p
mulForms s _ _        = s


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

cardinal :: NumConfig s -> Gender -> Integer -> Maybe s
cardinal NumConfig {..} g x | x < 0     = fmap ncNeg $ go EmptyContext $ abs x
                            | x == 0    = fmap (\sym -> symRepr sym g EmptyContext) $ ncCardinal 0
                            | otherwise = go EmptyContext x
    where go ctx n = do (NumSym _ v _ rv) <- ncCardinal n
                        case n `divMod` v of
                          (1, 0) -> return $ ncOne (v, rv g ctx)
                          (1, r) -> do rs <- go (RA v ctx) r
                                       return $ (v, ncOne (v, rv g (LA r ctx))) `ncAdd` (r, rs)
                          (q, r) | q >= v    -> Nothing
                                 | otherwise -> do qs <- go (LM v ctx) q
                                                   if r == 0
                                                     then return $ (q, qs) `ncMul` (v, rv g (RM q ctx))
                                                     else do let qv = q * v
                                                             rs <- go (RA qv ctx) r
                                                             return $ (qv, (q, qs) `ncMul` (v, rv g (RM q (LA qv ctx)))) `ncAdd` (r, rs)

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

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

infixr 5 <>, <+>, <->

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

type Test s = NumConfig s -> Gender -> [Integer] -> IO ()

test :: Stringable s => Test s
test nc g = mapM_ (putStrLn . pretty)
    where pretty n = show n ++ " == " ++ (maybe "-" id $ fmap toString $ cardinal nc g n)

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

testDocWithStyle :: PP.Style -> NumConfig PP.Doc -> Gender -> [Integer] -> IO ()
testDocWithStyle s nc g = mapM_ (putStrLn . pretty)
    where pretty n = show n ++ " == " ++ (maybe "-" id $ fmap (PP.renderStyle s) $ cardinal nc g n)

-------------------------------------------------------------------------------
-- Miscellaneous functions
-------------------------------------------------------------------------------

withSnd :: (a -> b -> c) -> (d, a) -> (e, b) -> c
withSnd f (_, x) (_, y) = f x y

d :: Integer -> Integer
d = (10 ^)

const2 :: a -> b -> c -> a
const2 = const . const

weave :: [a] -> [a] -> [a]
weave []     ys = ys
weave (x:xs) ys = x : weave ys xs

untilNothing :: [Maybe a] -> [a]
untilNothing []             = []
untilNothing (Just x  : xs) = x : untilNothing xs
untilNothing (Nothing : _)  = []

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
      transformSym sym = sym { symRepr = const2 . fromInteger . symVal $ sym}

-------------------------------------------------------------------------------

-- | 'NS' is used
newtype NS s = NS {unNS :: Precedence -> s}

type Precedence = Int

-- The following bogus Show and Eq instances are needed for Num :-(

instance Show (NS s) where
    show _ = "NS <function>"

instance Eq (NS s) where
    _ == _ = False

instance (IsString s, Joinable s) => Num (NS s) where
    fromInteger = NS . const . fromString . show

    (+) = bin "+" 6
    (-) = bin "-" 6
    (*) = bin "*" 7

    negate = un "negate"
    abs    = un "abs"
    signum = un "signum"

un :: (IsString s, Joinable s) => s -> (NS s -> NS s)
un sFun x = NS $ \p -> paren (p > precApp)
                             (sFun <+> unNS x (precApp+1))
    where
      precApp  = 10

bin :: (IsString s, Joinable s) => s -> Precedence -> (NS s -> NS s -> NS s)
bin sOp d x y = NS $ \p -> paren (p > d) $
                let p' = d+1
                in unNS x p' <+> sOp <+> unNS y p'

paren :: (IsString s, Joinable s) => Bool -> s -> s
paren True  s = "(" <> s <> ")"
paren False s = s

instance Stringable s => Stringable (NS s) where
    toString (NS f) = toString $ f 0

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
nlTable = [ term 0    $ const "nul"
          , term 1    $ const "één"
          , term 2    $ tenForms "twee" "twin" "twin"
          , term 3    $ tenForms "drie" "der"  "der"
          , term 4    $ tenForms "vier" "veer" "veer"
          , term 5    $ const "vijf"
          , term 6    $ const "zes"
          , term 7    $ const "zeven"
          , term 8    $ tenForms "acht" "tach" "acht"
          , term 9    $ const "negen"
          , mul  10   $ \ctx -> case ctx of
                                   RM _ _ -> "tig"
                                   _      -> "tien"
          , term 11   $ const "elf"
          , term 12   $ const "twaalf"
          , mul  100  $ const "honderd"
          , mul  1000 $ const "duizend"
          ] ++ (longScale "iljoen" "iljard")

nl :: (IsString s, Joinable s) => NumConfig s
nl = NumConfig { ncNeg      = ("min" <+>)
               , ncOne      = snd
               , ncAdd      = nlAdd
               , ncMul      = nlMul
               , ncCardinal = findSym nlTable
               }

-------------------------------------------------------------------------------

bigCardinal :: (IsString s, Joinable s) => Integer -> Maybe s
bigCardinal = cardinal bigNum Masculine

longScale :: (IsString s, Joinable s) => s -> s -> [NumSymbol s]
longScale a b = longScale' a a b b

longScale' :: (IsString s, Joinable s) => s -> s -> s -> s -> [NumSymbol s]
longScale' a as b bs = untilNothing $ weave (map illion [1..]) (map illiard [1..])
    where illion  n = fmap (\s -> mul (d 6 ^ n)       $ mulForms (s <> a) (s <> as)) $ bigCardinal n
          illiard n = fmap (\s -> mul (d 6 ^ n * d 3) $ mulForms (s <> b) (s <> bs)) $ bigCardinal n

shortScale :: (IsString s, Joinable s) => s -> [NumSymbol s]
shortScale a = shortScale' a a

shortScale' :: (IsString s, Joinable s) => s -> s -> [NumSymbol s]
shortScale' a as = untilNothing $ map illion [1..]
    where illion n = fmap (\s -> mul (d 3 * (d 3 ^ n)) $ mulForms (s <> a) (s <> as)) $ bigCardinal n

-------------------------------------------------------------------------------

bigNumTable :: (IsString s, Joinable s) => [NumSymbol s]
bigNumTable = [ term 0     $ const "nulla"
              , term 1     $ forms "m"     "un"       "un"       "mi"      "mi"
              , term 2     $ forms "b"     "duo"      "duo"      "vi"      "du"
              , term 3     $ forms "tr"    "tre"      "tres"     "tri"     "tre"
              , term 4     $ forms "quadr" "quattuor" "quattuor" "quadra"  "quadrin"
              , term 5     $ forms "quint" "quin"     "quinqua"  "quinqua" "quin"
              , term 6     $ forms "sext"  "sex"      "ses"      "sexa"    "ses"
              , term 7     $ forms "sept"  "septen"   "septem"   "septua"  "septin"
              , term 8     $ forms "oct"   "octo"     "octo"     "octo"    "octin"
              , term 9     $ forms "non"   "novem"    "novem"    "nona"    "non"
              , mul  10    $ \ctx -> case ctx of
                                        RM _ _ -> "gint"
                                        _      -> "dec"
              , mul  100   $ \ctx -> case ctx of
                                        RM n _ | n `elem` [2, 3, 6] -> "cent"
                                               | otherwise          -> "gent"
                                        _                           -> "cent"
              , mul  1000  $ const "mill"
              , mul  10000 $ const "myr"
              ]
    where forms d a1 a2 m1 m2 ctx = case ctx of
                                      RA 10  _ -> a1
                                      RA _   _ -> a2
                                      LM 100 _ -> m2
                                      LM _   _ -> m1
                                      _        -> d

bigNum :: (IsString s, Joinable s) => NumConfig s
bigNum = NumConfig { ncNeg      = error "bigNumNeg: undefined"
                   , ncOne      = snd
                   , ncAdd      = withSnd . flip $ (<>)
                   , ncMul      = withSnd (<>)
                   , ncCardinal = findSym bigNumTable
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
enTable = [ term 0    $ const "zero"
          , term 1    $ const "one"
          , term 2    $ tenForms "two"   "twen" "twen"
          , term 3    $ tenForms "three" "thir" "thir"
          , term 4    $ tenForms "four"  "for"  "four"
          , term 5    $ tenForms "five"  "fif"  "fif"
          , term 6    $ const "six"
          , term 7    $ const "seven"
          , term 8    $ tenForms "eight" "eigh" "eigh"
          , term 9    $ const "nine"
          , mul  10   $ \ctx -> case ctx of
                                   LA _ _ -> "teen"
                                   RM _ _ -> "ty"
                                   _      -> "ten"
          , term 11   $ const "eleven"
          , term 12   $ const "twelve"
          , mul  100  $ const "hundred"
          , mul  1000 $ const "thousand"
          ]

enShort :: (IsString s, Joinable s) => NumConfig s
enShort = NumConfig { ncNeg      = enNeg
                    , ncOne      = enOne
                    , ncAdd      = enAdd
                    , ncMul      = enMul
                    , ncCardinal = findSym $ enTable ++ shortScale "illion"
                    }

enLong :: (IsString s, Joinable s) => NumConfig s
enLong = NumConfig { ncNeg      = enNeg
                   , ncOne      = enOne
                   , ncAdd      = enAdd
                   , ncMul      = enMul
                   , ncCardinal = findSym $ enTable ++ longScale "illion" "illiard"
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
deTable = [ term 0        $ const "null"
          , term 1        $ const "eins"
          , term 2        $ tenForms "zwei" "zwei" "zwan"
          , term 3        $ const "drei"
          , term 4        $ const "vier"
          , term 5        $ const "fünf"
          , term 6        $ const "sechs"
          , term 7        $ tenForms "sieben" "sieb" "sieb"
          , term 8        $ const "acht"
          , term 9        $ const "neun"
          , mul  10       $ mulForms "zehn" "zig"
          , term 11       $ const "elf"
          , term 12       $ const "zwölf"
          , add  30    10 $ const "dreißig"
          , mul  100      $ const "hundert"
          , mul  1000     $ const "tausend"
          , mul  (d 6)    $ const "million"
          , mul  (d 9)    $ const "milliarde"
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
--   http://www.lysator.liu.se/language/Languages/Swedish/Grammar.html
--   http://en.wikibooks.org/wiki/Swedish/Numerals
--   http://longstrom.com/swedishtoenglish.htm#numbers

--   http://www.cs.chalmers.se/~aarne/GF/
--   http://www.cs.chalmers.se/~aarne/GF/lib/resource/norwegian/NumeralNor.gf

seOne :: (IsString s, Joinable s) => One s
seOne (v, vs) | v >= 100   = "ett" <> vs
              | otherwise  = vs

seAdd :: (IsString s, Joinable s) => Add s
seAdd (x, x') (_, y') | x < 20    = y' <> x'
                      | otherwise = x' <> y'

seTable :: (IsString s, Joinable s) => [NumSymbol s]
seTable = [ term 0        $ const "noll"
          , term 1        $ const "ett"
          , term 2        $ const "två"
          , term 3        $ tenForms "tre"  "tret" "tret"
          , term 4        $ tenForms "fyra" "fjor" "fyr"
          , term 5        $ const "fem"
          , term 6        $ const "sex"
          , term 7        $ tenForms "sju"  "sjut" "sjut"
          , term 8        $ tenForms "åtta" "ar"   "åt"
          , term 9        $ tenForms "nio"  "nit"  "nit"
          , mul  10       $ \ctx -> case ctx of
                                       LA _ _ -> "ton"
                                       _      -> "tio"
          , term 11       $ const "elva"
          , term 12       $ const "tolv"
          , add  20    10 $ const "tjugo"
          , mul  100      $ const "hundra"
          , mul  (d 3)    $ const "tusen"
          , mul  (d 6)    $ mulForms "miljon"       "miljoner"
          , mul  (d 9)    $ mulForms "miljard"      "miljarder"
          , mul  (d 12)   $ mulForms "biljon"       "biljoner"
          , mul  (d 15)   $ mulForms "biljard"      "biljarder"
          , mul  (d 18)   $ mulForms "triljon"      "triljoner"
          , mul  (d 21)   $ mulForms "triljard"     "triljarder"
          , mul  (d 24)   $ mulForms "kvadriljon"   "kvadriljoner"
          , mul  (d 27)   $ mulForms "kvadriljard"  "kvadriljarder"
          , mul  (d 30)   $ mulForms "kvintriljon"  "kvintriljoner"
          , mul  (d 33)   $ mulForms "kvintriljard" "kvintriljarder"
          ]

se :: (IsString s, Joinable s) => NumConfig s
se = NumConfig { ncNeg      = ("minus" <+>)
               , ncOne      = seOne
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
noTable = [ term 0        $ const "null"
          , term 1        $ const "én"
          , term 2        $ const "to"
          , term 3        $ tenForms "tre"  "tret" "tret"
          , term 4        $ tenForms "fire" "fjor" "før"
          , term 5        $ const "fem"
          , term 6        $ const "seks"
          , term 7        $ tenForms "sju"  "syt" "syt"
          , term 8        $ tenForms "åtte" "at"  "åt"
          , term 9        $ tenForms "ni"   "nit" "nit"
          , mul  10       $ \ctx -> case ctx of
                                       LA _ _ -> "ten"
                                       _      -> "ti"
          , term 11       $ const "elleve"
          , term 12       $ const "tolv"
          , add  20    10 $ const "tjue"
          , mul  100      $ const "hundre"
          , mul  (d 3)    $ const "tusen"
          , mul  (d 6)    $ mulForms "million"  "millioner"
          , mul  (d 9)    $ mulForms "milliard" "milliarder"
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
laTable = [ term 0     $ const "nulla"
          , term 1     $ tenForms  "unus"     "un"       "unus"
          , term 2     $ tenForms' "duo"      "duo"      "vi"      "du"
          , term 3     $ tenForms' "tres"     "tre"      "tri"     "tre"
          , term 4     $ tenForms' "quattuor" "quattuor" "quadra"  "quadri"
          , term 5     $ tenForms' "quinque"  "quin"     "quinqua" "quin"
          , term 6     $ tenForms' "sex"      "se"       "sexa"    "ses"
          , term 7     $ tenForms' "septem"   "septen"   "septua"  "septin"
          , term 8     $ const "octo"
          , term 9     $ tenForms' "novem"    "novem"    "nona"    "non"
          , mul  10    $ \ctx -> case ctx of
                                    LA _ _ -> "decim"
                                    RM 2 _ -> "ginti"
                                    RM _ _ -> "ginta"
                                    _      -> "decem"
          , term 18    $ const "duodeviginti"
          , term 19    $ const "undeviginti"
          , term 28    $ const "duodetriginta"
          , term 29    $ const "undetriginta"
          , term 38    $ const "duodequadraginta"
          , term 39    $ const "undequadraginta"
          , term 48    $ const "duodequinquaginta"
          , term 49    $ const "undequinquaginta"
          , term 58    $ const "duodesexaginta"
          , term 59    $ const "undesexaginta"
          , term 68    $ const "duodeseptuaginta"
          , term 69    $ const "undeseptuaginta"
          , term 78    $ const "duodeoctoginta"
          , term 79    $ const "undeoctoginta"
          , term 88    $ const "duodenonaginta"
          , term 89    $ const "undenonaginta"
          , term 98    $ const "duodecentum"
          , term 99    $ const "undecentum"
          , mul  100   $ \ctx -> case ctx of
                                    RM n _ | n `elem` [2, 3, 6] -> "centi"
                                           | otherwise          -> "genti"
                                    _                           -> "centum"
          , mul  1000  $ mulForms "mille" "millia"
          , term (d 6) $ const "decies centena milia"
          ]

la :: (IsString s, Joinable s) => NumConfig s
la = NumConfig { ncNeg      = error "laNeg: undefined"
               , ncOne      = snd
               , ncAdd      = laAdd
               , ncMul      = laMul
               , ncCardinal = findSym laTable
               }

-------------------------------------------------------------------------------

-- Sources:
--  http://www.cliffsnotes.com/WileyCDA/CliffsReviewTopic/Numbers.topicArticleId-25559,articleId-25469.html

frAdd :: (IsString s, Joinable s) => Add s
frAdd (x, x') (y, y') | x == 10 && y < 7 = y' <> x'
                      | x < 80 && y == 1 = x' <+> "et" <+> y'
                      | x < 100          = x' <-> y'
                      | otherwise        = x' <+> y'

frMul :: (IsString s, Joinable s) => Mul s
frMul (_, x') (y, y') | y == 10   = x' <> y'
                      | otherwise = x' <+> y'

frTable :: (IsString s, Joinable s) => [NumSymbol s]
frTable = [ term  0        $ const "zéro"
          , termG 1        $ tenFormsG (gender "un" "une") (const "on") (const "un")
          , term  2        $ tenForms "deux"   "deux"   "dou"
          , term  3        $ tenForms "trois"  "trei"   "tren"
          , term  4        $ tenForms "quatre" "quator" "quar"
          , term  5        $ tenForms "cinq"   "quin"   "cinqu"
          , term  6        $ tenForms "six"    "sei"    "soix"
          , term  7        $ const "sept"
          , term  8        $ const "huit"
          , term  9        $ const "neuf"
          , mul   10       $ \ctx -> case ctx of
                                        LA n _ | n < 7     -> "ze"
                                               | otherwise -> "dix"
                                        RM 3 _ -> "te"
                                        RM _ _ -> "ante"
                                        _      -> "dix"
          , add   20    10 $ const "vingt"
          , add   60    20 $ const "soixante"
          , term  71       $ const "soixante et onze"
          , term  80       $ const "quatre-vingts"
          , add   80    20 $ const "quatre-vingt"
          , mul   100      $ let c = "cent"
                             in \ctx -> case ctx of
                                          RM _ (LA _ _) -> c
                                          RM _ (LM _ _) -> c
                                          RM _ _        -> c <> "s"
                                          _             -> c
          , mul   1000     $ const "mille"
          ] ++ longScale' "illion" "illions" "illiard" "illiards"

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
itTable = [ term 0       $ const "zero"
          , term 1       $ tenForms "uno"     "un"      "uno"
          , term 2       $ tenForms "due"     "do"      "due"
          , term 3       $ tenForms "tre"     "tre"     "ten"
          , term 4       $ tenForms "quattro" "quattor" "quar"
          , term 5       $ tenForms "cinque"  "quin"    "cinqu"
          , term 6       $ tenForms "sei"     "se"      "sess"
          , term 7       $ tenForms "sette"   "assette" "sett"
          , term 8       $ tenForms "otto"    "otto"    "ott"
          , term 9       $ tenForms "nove"    "annove"  "nove"
          , mul  10      $ \ctx -> case ctx of
                                      LA _ _ -> "dici"
                                      RM 3 _ -> "ta"
                                      RM _ _ -> "anta"
                                      _      -> "dieci"
          , add  20   10 $ const "venti"
          , term 21      $ const "ventuno"
          , term 28      $ const "ventotto"
          , term 31      $ const "trentuno"
          , term 38      $ const "trentotto"
          , term 41      $ const "quarantuno"
          , term 48      $ const "quarantotto"
          , term 51      $ const "cinquantuno"
          , term 58      $ const "cinquantotto"
          , term 61      $ const "sessantuno"
          , term 68      $ const "sessantotto"
          , term 71      $ const "settantuno"
          , term 78      $ const "settantotto"
          , term 81      $ const "ottantuno"
          , term 88      $ const "ottantotto"
          , term 91      $ const "novantuno"
          , term 98      $ const "novantotto"
          , mul  100     $ const "cento"
          , mul  1000    $ mulForms "mille" "mila"
          ] ++ longScale' "ilione" "ilioni" "iliardo" "iliardi"

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
--   http://www.learn-spanish-help.com/count-in-spanish.html
--   http://www.donquijote.org/spanishlanguage/numbers/numbers1.asp

spAdd :: (IsString s, Joinable s) => Add s
spAdd (x, x') (y, y') | x == 10 && y < 6 = y' <> x'
                      | x == 10    = x' <> y'
                      | x < 30     = x' <>  "i" <>  y'
                      | x < 100    = x' <+> "y" <+> y'
                      | otherwise  = x' <+> y'

spMul :: (IsString s, Joinable s) => Mul s
spMul (_, x') (y, y') | y < 1000  = x' <> y'
                      | otherwise = x' <+> y'

spTable :: (IsString s, Joinable s) => [NumSymbol s]
spTable = [ term 0         $ const "cero"
          , term 1         $ tenForms  "uno"    "on"    "uno"
          , term 2         $ \ctx -> case ctx of
                                        RA 10 _ -> "do"
                                        RA 20 _ -> "dós"
                                        _       -> "dos"
          , term 3         $ \ctx -> case ctx of
                                        RA n _ | n == 10 -> "tre"
                                               | n < 100 -> "trés"
                                        LM 10 _ -> "trein"
                                        _       -> "tres"
          , term 4         $ tenForms  "cuatro" "cator" "cuaren"
          , term 5         $ tenForms  "cinco"  "quin"  "cincuen"
          , term 6         $ \ctx -> case ctx of
                                        RA n _ | n <= 20 -> "séis"
                                        LM 10 _ -> "sesen"
                                        _       -> "seis"
          , term 7         $ tenForms' "siete"  "siete" "seten" "sete"
          , term 8         $ tenForms  "ocho"   "ocho"  "ochen"
          , term 9         $ tenForms' "nueve"  "nueve" "noven" "novo"
          , mul  10        $ \ctx -> case ctx of
                                        LA n _ | n < 6     -> "ce"
                                               | otherwise -> "dieci"
                                        RM _ _ -> "ta"
                                        _      -> "diez"
          , add  20    10  $ const "veint"
          , mul  100       $ \ctx -> case ctx of
                                        RM _ _ -> "cientos"
                                        LA _ _ -> "ciento"
                                        _      -> "cien"
          , add  500   100 $ const "quinientos"
          , mul  1000      $ const "mil"
          , mul  (d 6)     $ mulForms "millón" "millones"
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
ptTable = [ term 0         $ const "zero"
          , term 1         $ tenForms "um"     "on"    "um"
          , term 2         $ tenForms "dois"   "do"    "dois"
          , term 3         $ tenForms "três"   "tre"   "trin"
          , term 4         $ tenForms "quatro" "cator" "quar"
          , term 5         $ tenForms "cinco"  "quin"  "cinqü"
          , term 6         $ tenForms "seis"   "seis"  "sess"
          , term 7         $ tenForms "sete"   "sete"  "set"
          , term 8         $ tenForms "oito"   "oito"  "oit"
          , term 9         $ tenForms "nove"   "nove"  "nov"
          , mul  10        $ \ctx -> case ctx of
                                        LA _ _ -> "ze"
                                        RM 3 _ -> "ta"
                                        RM _ _ -> "enta"
                                        _      -> "dez"
          , term 16        $ const "dezesseis"
          , term 17        $ const "dezessete"
          , term 18        $ const "dezoito"
          , term 19        $ const "dezenove"
          , add  20    10  $ const "vinte"
          , mul  100       $ \ctx -> case ctx of
                                        RM _ _ -> "centos"
                                        LA _ _ -> "cento"
                                        _      -> "cem"
          , add  200   100 $ const "duzentos"
          , add  300   100 $ const "trezentos"
          , add  500   100 $ const "quinhentos"
          , mul  1000      $ const "mil"
          ] ++ shortScale' "ilhão" "ilhões"

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
jaTable = [ term 0         $ const "rei"
          , term 1         $ const "ichi"
          , term 2         $ const "ni"
          , term 3         $ const "san"
          , term 4         $ const "yon"
          , term 5         $ const "go"
          , term 6         $ const "roku"
          , term 7         $ const "nana"
          , term 8         $ const "hachi"
          , term 9         $ const "kyū"
          , mul 10         $ const "jū"
          , mul 100        $ const "hyaku"
          , add 300    100 $ const "san-byaku" -- rendaku
          , mul 1000       $ const "sen"
          , mul (d 4)      $ const "man"
          , mul (d 8)      $ const "oku"
          , mul (d 12)     $ const "chō"
          , mul (d 16)     $ const "kei"
          , mul (d 20)     $ const "gai"
          , mul (d 24)     $ const "jo"
          , mul (d 28)     $ const "jō"
          , mul (d 32)     $ const "kō"
          , mul (d 36)     $ const "kan"
          , mul (d 40)     $ const "sei"
          , mul (d 44)     $ const "sai"
          , mul (d 48)     $ const "goku"
          , mul (d 52)     $ const "gōgasha"
          , mul (d 56)     $ const "asōgi"
          , mul (d 60)     $ const "nayuta"
          , mul (d 64)     $ const "fukashigi"
          , mul (d 68)     $ const "muryōtaisū"
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
eoTable = [ term 0    $ const "nulo"
          , term 1    $ const "unu"
          , term 2    $ const "du"
          , term 3    $ const "tri"
          , term 4    $ const "kvar"
          , term 5    $ const "kvin"
          , term 6    $ const "ses"
          , term 7    $ const "sep"
          , term 8    $ const "ok"
          , term 9    $ const "naŭ"
          , mul 10    $ const "dek"
          , mul 100   $ const "cent"
          , mul 1000  $ const "mil"
          , mul (d 6) $ const "miliono"
          ]

eo :: (IsString s, Joinable s) => NumConfig s
eo = NumConfig { ncNeg      = error "eoNeg: undefined"
               , ncOne      = snd
               , ncAdd      = withSnd (<+>)
               , ncMul      = withSnd (<>)
               , ncCardinal = findSym eoTable
               }
