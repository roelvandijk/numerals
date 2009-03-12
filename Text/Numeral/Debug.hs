{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.Numeral.Debug where

import Data.String
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Misc (const2, withSnd)

import qualified Data.DString          as DS
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Text.PrettyPrint      as PP

-------------------------------------------------------------------------------

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

-- | @nummify@ transforms the given NumConfig to a numeric NumConfig
-- such that 'prop_cardinal_nummify' holds.  @nummify@ is thus usefull
-- as a testing aid.  @nummify@ is also usefull as a debugging aid
-- when you use a suitable numeric type such as 'NS' which renders a
-- numeric expression to a string representing the same expression.
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

-- | 'prop_cardinal_nummify' specifies the correctness of 'cardinal'.
prop_cardinal_nummify :: NumConfig String -> Gender -> Integer -> Bool
prop_cardinal_nummify nc g n = maybe True (== n) $ cardinal (nummify nc) g n

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
