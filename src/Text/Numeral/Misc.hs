{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Text.Numeral.Misc where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude ( Integral, (^) )
import "base" Data.Function ( (.) )
import "base" Prelude       ( fromIntegral, toInteger )
import "this" Math.NumberTheory.Logarithms ( integerLog10' )

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- ^ Raise 10 to some power.
dec ∷ (Integral α) ⇒ α → α
dec = (10 ^)

-- ^ The (base 10) logarithm of an integral value. Note that the
-- result must be able to fit in an ordinary Int value. This means the
-- maximum input value is 10 ^ (maxBound ∷ Int).
intLog ∷ (Integral α) ⇒ α → α
intLog = fromIntegral . integerLog10' . toInteger
