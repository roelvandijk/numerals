{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , TypeSynonymInstances
           , UnicodeSyntax
  #-}

module Text.Numeral.Debug where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.String ( IsString, fromString )
import Data.Monoid ( Monoid )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Eq.Unicode       ( (≡) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc (const2, withSnd)
import Text.Numeral.Language

-- from string-combinators:
import Data.String.Combinators ( (<>), (<+>) )

-- from various other packages:
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T


--------------------------------------------------------------------------------
-- Debug
--------------------------------------------------------------------------------


class Stringable s where
    toString ∷ s → String

instance Stringable String where
    toString = id

instance Stringable B.ByteString where
    toString = B.unpack

instance Stringable T.Text where
    toString = T.unpack

-------------------------------------------------------------------------------

type Test s = NumConfig s → Gender → [Integer] → IO ()

test ∷ Stringable s ⇒ Test s
test nc g = mapM_ (putStrLn ∘ pretty)
    where pretty n = show n ++ " = " ++ (maybe "-" id $ fmap toString $ cardinal nc g n)

testS ∷ Test String
testS = test

testBS ∷ Test B.ByteString
testBS = test

testT ∷ Test T.Text
testT = test


-------------------------------------------------------------------------------

-- | @nummify@ transforms the given NumConfig to a numeric NumConfig
-- such that 'prop_cardinal_nummify' holds.  @nummify@ is thus usefull
-- as a testing aid.  @nummify@ is also usefull as a debugging aid
-- when you use a suitable numeric type such as 'NS' which renders a
-- numeric expression to a string representing the same expression.
nummify ∷ Num n ⇒ NumConfig s → NumConfig n
nummify (NumConfig {..}) = NumConfig { ncCardinal = fmap transformSym ∘ ncCardinal
                                     , ncNeg      = negate
                                     , ncOne      = snd
                                     , ncAdd      = withSnd (+)
                                     , ncMul      = withSnd (*)
                                     }
    where
      -- Create a new symbol who's representation is its value.
      transformSym ∷ (Num n) ⇒ NumSymbol s → NumSymbol n
      transformSym sym = sym { symRepr = const2 ∘ fromInteger ∘ symVal $ sym}

-- | 'prop_cardinal_nummify' specifies the correctness of 'cardinal'.
prop_cardinal_nummify ∷ NumConfig String → Gender → Integer → Bool
prop_cardinal_nummify nc g n = maybe True (≡ n) $ cardinal (nummify nc) g n

-------------------------------------------------------------------------------

-- | 'NS' is used
newtype NS s = NS {unNS ∷ Precedence → s}

type Precedence = Int

-- The following bogus Show and Eq instances are needed for Num :-(

instance Show (NS s) where
    show _ = "NS <function>"

instance Eq (NS s) where
    _ == _ = False

instance (Monoid s, IsString s) ⇒ Num (NS s) where
    fromInteger = NS ∘ const ∘ fromString ∘ show

    (+) = bin "+" 6
    (-) = bin "-" 6
    (*) = bin "*" 7

    negate = un "negate"
    abs    = un "abs"
    signum = un "signum"

un ∷ (Monoid s, IsString s) ⇒ s → (NS s → NS s)
un sFun x = NS $ \p → paren (p > precApp)
                             (sFun <+> unNS x (precApp+1))
    where
      precApp  = 10

bin ∷ (Monoid s, IsString s) ⇒ s → Precedence → (NS s → NS s → NS s)
bin sOp d x y = NS $ \p → paren (p > d) $
                let p' = d + 1
                in unNS x p' <+> sOp <+> unNS y p'

paren ∷ (Monoid s, IsString s) ⇒ Bool → s → s
paren True  s = "(" <> s <> ")"
paren False s = s

instance Stringable s ⇒ Stringable (NS s) where
    toString (NS f) = toString $ f 0

testNSS ∷ Test (NS String)
testNSS = test

testNumS ∷ Test String
testNumS = testNSS ∘ nummify

-------------------------------------------------------------------------------

{-
newtype Lst s = Lst {unLst ∷ [s]}

instance Joinable s ⇒ Joinable (Lst s) where
    x <>  y = Lst $ appendUnionWith (<>) (unLst x) (unLst y)
    x <+> y = Lst $ unLst x ++ unLst y

-- | appendUnionWith f [a, b, c] [d, e, f] ⇒ [a, b, c `f` d, e, f]
appendUnionWith ∷ (a → a → a) → [a] → [a] → [a]
appendUnionWith _ []     ys     = ys
appendUnionWith _ xs     []     = xs
appendUnionWith f [x]    (y:ys) = x `f` y : ys
appendUnionWith f (x:xs) ys     = x : appendUnionWith f xs ys

instance IsString s ⇒ IsString (Lst s) where
    fromString s = Lst [fromString s]

instance Stringable s ⇒ Stringable (Lst s) where
    toString = show ∘ map toString ∘ unLst

testLst ∷ Test (Lst String)
testLst = test
-}
