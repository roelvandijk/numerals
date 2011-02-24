{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
  #-}

module Text.Numeral.Rules
  ( Side(L, R)
  , atom, atom1
  , add
  , mul, mul1
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative ( liftA2 )
import Data.Function       ( ($), id, const, flip )
import Data.Functor        ( (<$>) )
import Data.Maybe          ( Maybe(Just) )
import Prelude             ( Integral, fromIntegral, Num, (+), (-), divMod )
import Text.Show           ( Show )

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Prelude.Unicode       ( (⋅) )

-- from numerals:
import Text.Numeral ( Rule )


--------------------------------------------------------------------------------
-- Side
--------------------------------------------------------------------------------

data Side = L | R deriving Show

flipIfR ∷ Side → (α → α → α) → (α → α → α)
flipIfR L = id
flipIfR R = flip


--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

atom ∷ (Integral α, Num β) ⇒ Rule α β
atom = const $ Just ∘ fromIntegral

atom1 ∷ (Integral α, Num β) ⇒ Rule α β
atom1 = const $ \n → Just $ 1 ⋅ fromIntegral n

add ∷ (Num α, Num β) ⇒ α → Side → Rule α β
add val s = \f n → liftA2 (flipIfR s (+)) (f $ n - val) (f val)

mul ∷ (Integral α, Num β) ⇒ α → Side → Side → Rule α β
mul val aSide mSide =
    \f n → let (q, r) = n `divMod` val
               qval = liftA2 (flipIfR mSide (⋅)) (f q) (f val)
           in if r ≡ 0
              then qval
              else liftA2 (flipIfR aSide (+)) (f r) qval

mul1 ∷ (Integral α, Num β) ⇒ α → Side → Side → Rule α β
mul1 val aSide mSide =
    \f n → let (q, r) = n `divMod` val
               qval = if q ≡ 1
                      then Just $ 1 ⊡ fromIntegral val
                      else (⊡ fromIntegral val) <$> f q
           in if r ≡ 0
              then qval
              else liftA2 (flipIfR aSide (+)) (f r) qval
  where
     (⊡) = flipIfR mSide (⋅)
