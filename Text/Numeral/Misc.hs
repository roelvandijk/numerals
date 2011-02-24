{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Misc where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- base
-- import Data.Maybe    ( Maybe(Nothing, Just) )
import Prelude       ( (^), Integral )


--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- withSnd ∷ (a → b → c) → (d, a) → (e, b) → c
-- withSnd f (_, x) (_, y) = f x y

dec ∷ (Integral α) ⇒ α → α
dec = (10 ^)

-- weave ∷ [a] → [a] → [a]
-- weave []     ys = ys
-- weave (x:xs) ys = x : weave ys xs

-- untilNothing ∷ [Maybe a] → [a]
-- untilNothing []             = []
-- untilNothing (Just x  : xs) = x : untilNothing xs
-- untilNothing (Nothing : _)  = []
