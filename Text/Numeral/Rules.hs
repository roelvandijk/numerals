{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , TypeSynonymInstances
  #-}

module Text.Numeral.Rules
  ( Rule
  , emptyRule, combineRules

  , Rules
  , findRule
  , positive

  , Side(L, R)
  , atom, atom1
  , add
  , mul, mul1
  , sub

  , mulScale
  , shortScale

  , scaleRules, scale1Rules
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative ( liftA2 )
import Data.Bool           ( otherwise )
import Data.Function       ( ($), id, const, flip )
import Data.Functor        ( (<$>) )
import Data.List           ( foldr, concatMap )
import Data.Maybe          ( Maybe(Nothing, Just) )
import Data.Monoid         ( Monoid, mempty, mappend )
import Data.Ord            ( Ord, (<), (>) )
import Prelude             ( Integral, fromIntegral, fromInteger
                           , Num, (+), (-), abs, negate, divMod, div
                           )
import Text.Show           ( Show )

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Prelude.Unicode       ( (⋅) )

-- from numerals:
import Text.Numeral.Exp  ( Subtract, subtract, Scale, scale )
import Text.Numeral.Misc ( dec, intLog )

-- from fingertree:
import qualified Data.IntervalMap.FingerTree as FT
    ( Interval(Interval)
    , IntervalMap, empty, insert
    , search
    )


--------------------------------------------------------------------------------
-- Rule type
--------------------------------------------------------------------------------

type Rule α β = (α → Maybe β) → (α → Maybe β)

instance Monoid (Rule α β) where
  mempty  = emptyRule
  mappend = combineRules


--------------------------------------------------------------------------------
-- Rule combinators
--------------------------------------------------------------------------------

emptyRule ∷ Rule α β
emptyRule _ _ = Nothing

combineRules ∷ Rule α β → Rule α β → Rule α β
combineRules r1 r2 = \f n → case r1 f n of
                              Nothing → r2 f n
                              x       → x

-- idRule ∷ Rule α β
-- idRule f n = f n

type Rules α β = [((α, α), Rule α β)]

findRule ∷ (Ord α) ⇒ Rules α β → Rule α β
findRule xs = \f n → case FT.search n xm of
                       [] → Nothing
                       (_,r):_ → r f n
    where
      xm = mkIntervalMap xs


--------------------------------------------------------------------------------
-- Useful rules
--------------------------------------------------------------------------------

positive ∷ (Ord α, Num α, Num β) ⇒ Rule α β
positive f n | n < 0 = negate <$> f (abs n)
             | n > 0 = f n
             | otherwise = Just 0

atom ∷ (Integral α, Num β) ⇒ Rule α β
atom = const $ Just ∘ fromIntegral

atom1 ∷ (Integral α, Num β) ⇒ Rule α β
atom1 = const $ \n → Just $ 1 ⋅ fromIntegral n

add ∷ (Num α, Num β) ⇒ α → Side → Rule α β
add val s = \f n → liftA2 (flipIfR s (+)) (f $ n - val) (f val)

mul ∷ (Integral α, Num β) ⇒ α → Side → Side → Rule α β
mul val aSide mSide =
    \f n → let (m, a) = n `divMod` val
               mval = liftA2 (flipIfR mSide (⋅)) (f m) (f val)
           in if a ≡ 0
              then mval
              else liftA2 (flipIfR aSide (+)) (f a) mval

mul1 ∷ (Integral α, Num β) ⇒ α → Side → Side → Rule α β
mul1 val aSide mSide =
    \f n → let (m, a) = n `divMod` val
               mval = if m ≡ 1
                      then Just $ 1 ⊡ fromIntegral val
                      else (⊡ fromIntegral val) <$> f m
           in if a ≡ 0
              then mval
              else liftA2 (flipIfR aSide (+)) (f a) mval
  where
     (⊡) = flipIfR mSide (⋅)

sub ∷ (Integral α, Subtract β) ⇒ α → Rule α β
sub val = \f n → liftA2 subtract (f $ val - n) (f val)

-- See: http://en.wikipedia.org/wiki/Names_of_large_numbers
mulScale ∷ (Integral α, Num β, Scale β) ⇒ α → α → Side → Side → Rule α β
mulScale base offset aSide mSide =
    \f n → let rank = (intLog n - offset) `div` base
               big ∷ Scale γ ⇒ γ
               big = scale (fromIntegral base)
                           (fromIntegral offset)
                           (fromIntegral rank)
               (m, a) = n `divMod` fromInteger big
               mval = liftA2 (flipIfR mSide (⋅)) (f m) (Just big)
           in if a ≡ 0
              then mval
              else liftA2 (flipIfR aSide (+)) (f a) mval

shortScale ∷ (Integral α, Num β, Scale β) ⇒ Side → Side → Rule α β
shortScale = mulScale 3 3

-- longScale1 ∷ (Scale α) ⇒ Integer → α
-- longScale1 = scale 6 0

-- longScale2 ∷ (Scale α) ⇒ Integer → α
-- longScale2 = scale 6 3

--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

data Side = L | R deriving Show

flipIfR ∷ Side → (α → α → α) → (α → α → α)
flipIfR L = id
flipIfR R = flip

mkIntervalMap ∷ (Ord v) ⇒ [((v, v), α)] → FT.IntervalMap v α
mkIntervalMap = foldr ins FT.empty
  where ins ((lo, hi), n) = FT.insert (FT.Interval lo hi) n

scaleRules ∷ (Integral α, Num β) ⇒ α → Side → Side → Rules α β
scaleRules g aSide mSide = concatMap step [g, 2⋅g ..]
    where
      step n = [ ((s, s), atom)
               , ((s+1, 2⋅s - 1), add s aSide)
               , ((2⋅s, s ⋅ dec g - 1), mul s aSide mSide)
               ]
          where
            s = dec n

scale1Rules ∷ (Integral α, Num β) ⇒ α → Side → Side → Rules α β
scale1Rules g aSide mSide = concatMap step [g, 2⋅g ..]
    where
      step n = [ ((s, s), atom1)
               , ((s+1, 2⋅s - 1), add s aSide)
               , ((2⋅s, s ⋅ dec g - 1), mul1 s aSide mSide)
               ]
          where
            s = dec n
