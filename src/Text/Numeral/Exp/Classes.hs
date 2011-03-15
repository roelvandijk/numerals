{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Text.Numeral.Exp.Classes
    ( Lit(lit)
    , Neg(neg)
    , Add(add)
    , Mul(mul)
    , Sub(sub)
    , Scale(scale)
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Prelude ( (+), (*), (^), subtract, negate, Integer, fromInteger )

-- from base-unicode-symbols:
import Prelude.Unicode ( (⋅) )


-------------------------------------------------------------------------------
-- Exp classes
-------------------------------------------------------------------------------

class Lit α where lit ∷ Integer → α
class Neg α where neg ∷ α → α
class Add α where add ∷ α → α → α
class Mul α where mul ∷ α → α → α
class Sub α where sub ∷ α → α → α
class Scale α where scale ∷ Integer → Integer → α → α

infixl 6 `add`
infixl 6 `sub`
infixl 7 `mul`


-------------------------------------------------------------------------------
-- Integer instances
-------------------------------------------------------------------------------

instance Lit Integer where lit = fromInteger
instance Neg Integer where neg = negate
instance Add Integer where add = (+)
instance Mul Integer where mul = (*)
instance Sub Integer where sub = subtract
instance Scale Integer where scale b o r = 10 ^ (r⋅b + o)
