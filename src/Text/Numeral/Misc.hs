{-# LANGUAGE CPP
           , MagicHash
           , NoImplicitPrelude
           , PackageImports
           , UnicodeSyntax
  #-}

module Text.Numeral.Misc where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Prelude ( Integral, (^) )
#if __GLASGOW_HASKELL__ >= 702
import "base" Data.Function ( ($) )
import "base" GHC.Exts      ( Int(I#) )
import "base" Prelude       ( fromIntegral, toInteger )
import "integer-gmp" GHC.Integer.Logarithms ( integerLogBase# )
#else
import "base" Data.Bool ( otherwise )
import "base" Data.Ord  ( (<) )
import "base" Prelude   ( div, (+), ($!), error )
#endif

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
-- GHC >= 7.2.x
#if __GLASGOW_HASKELL__ >= 702
intLog x = fromIntegral $ I# (integerLogBase# 10 (toInteger x))
#else
intLog x | x < 0 = error "intLog: undefined for negative numbers"
         | otherwise = go x 0
    where
      go n acc = case n `div` 10 of
                   0 → acc
                   1 → acc + 1
                   q → go q $! acc + 1
#endif
