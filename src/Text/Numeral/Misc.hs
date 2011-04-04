{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Misc where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

dec ∷ (Integral α) ⇒ α → α
dec = (10 ^)

intLog ∷ (Integral α) ⇒ α → α
intLog x | x < 0 = error "intLog: undefined for negative numbers"
         | otherwise = go x 0
    where
      go n acc = case n `div` 10 of
                   0 → acc
                   1 → acc + 1
                   q → go q $! acc + 1

-- prop_intLog e = intLog (10^e) ≡ e

