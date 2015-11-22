module Text.Numeral.Misc where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "this" Math.NumberTheory.Logarithms ( integerLog10' )

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- ^ Raise 10 to some power.
dec :: (Integral a) => a -> a
dec = (10 ^)

-- ^ The (base 10) logarithm of an integral value. Note that the
-- result must be able to fit in an ordinary Int value. This means the
-- maximum input value is 10 ^ (maxBound :: Int).
intLog :: (Integral a) => a -> a
intLog = fromIntegral . integerLog10' . toInteger
