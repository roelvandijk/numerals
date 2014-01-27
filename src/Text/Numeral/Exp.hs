{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

module Text.Numeral.Exp
    ( Unknown(unknown, isUnknown)
    , Lit(lit)
    , Neg(neg)
    , Add(add)
    , Mul(mul)
    , Sub(sub)
    , Frac(frac)
    , Scale(scale)
    , Inflection(..)
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool ( Bool(False) )
import "base" Data.Function ( const )
import "base" Prelude ( (+), (*), (^), subtract, negate, fromInteger, error )
import "base-unicode-symbols" Prelude.Unicode ( ℤ, (⋅) )


-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

-- | An unknown value. This is used to signal that a value can not be
-- represented in the expression language.
--
-- Law: isUnknown unknown == True
class Unknown α where
    unknown ∷ α
    isUnknown ∷ α → Bool

-- | A literal value.
--
-- Example in English:
--
-- > "three" = lit 3
class Lit α where lit ∷ ℤ → α

-- | Negation of a value.
--
-- Example in English:
--
-- > "minus two" = neg (lit 2)
class Neg α where neg ∷ α → α

-- | Addition of two values.
--
-- Example in English:
--
-- > "fifteen" = lit 5 `add` lit 10
class Add α where add ∷ α → α → α

-- | Multiplication of two values.
--
-- Example in English:
--
-- > "thirty" = lit 3 `mul` lit 10
class Mul α where mul ∷ α → α → α

-- | One value subtracted from another value.
--
-- Example in Latin:
--
-- > "duodēvīgintī" = lit 2 `sub` (lit 2 `mul` lit 10)
class Sub α where sub ∷ α → α → α

-- | A fraction.
--
-- Example in English:
--
-- > "two thirds" = `frac` (lit 2) (lit 3)
class Frac α where frac ∷ α → α → α

-- | A step in a scale of large values.
--
-- Should be interpreted as @10 ^ (rank * base + offset)@.
--
-- Example in English:
--
-- > "quadrillion" = scale 3 3 4
class Scale α where
    scale ∷ ℤ -- ^ Base.
          → ℤ -- ^ Offset.
          → α -- ^ Rank.
          → α

-- | A change of inflection.
--
-- This is used in a language like Spanish where the inflection of a
-- number word is not always constant. Specifically, in Spanish, large
-- number names always have the masculine gender. So 'millón',
-- 'billón' and the like are all masculine. This can result in the
-- following number word: 10000001 = "un (masculine) millón una
-- (feminine)"
class Inflection α where
    type Inf α
    inflection ∷ (Inf α → Inf α) → α → α

infixl 6 `add`
infixl 6 `sub`
infixl 7 `mul`


-------------------------------------------------------------------------------
-- Integer instances
-------------------------------------------------------------------------------

instance Unknown ℤ where
    unknown   = error "unknown"
    isUnknown = const False
instance Lit ℤ where lit = fromInteger
instance Neg ℤ where neg = negate
instance Add ℤ where add = (+)
instance Mul ℤ where mul = (*)
instance Sub ℤ where sub = subtract
instance Scale ℤ where scale b o r = 10 ^ (r⋅b + o)

-- TODO: instances for ℚ?
