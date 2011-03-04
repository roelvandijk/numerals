{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , TypeSynonymInstances
  #-}

module Text.Numeral.Rules
  ( Rule
  , empty, combine
  , when, conditional

  , Rules
  , findRule
  , positive

  , Side(L, R)
  , atom, atom1
  , add
  , mul, mul1
  , sub

  , mulScale, mulScale1
  , shortScale,  longScale,  pelletierScale
  , shortScale1, longScale1, pelletierScale1

  , scaleRules, scale1Rules
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative ( liftA2 )
import Control.Monad       ( (>>=) )
import Data.Bool           ( Bool, otherwise )
import Data.Function       ( ($), id, const, flip, fix )
import Data.Functor        ( (<$>) )
import Data.List           ( foldr, concatMap )
import Data.Maybe          ( Maybe(Nothing, Just) )
import Data.Ord            ( Ord, (<), (>) )
import Prelude             ( Integral, fromIntegral
                           , Num, (+), (-), abs, negate, divMod, div, even
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


--------------------------------------------------------------------------------
-- Rule combinators
--------------------------------------------------------------------------------

-- Law: r `combine` empty ≡ r

-- idRule ∷ Rule α β
-- idRule f n = f n

empty ∷ Rule α β
empty _ _ = Nothing

combine ∷ Rule α β → Rule α β → Rule α β
combine r1 r2 = \f n → case r1 f n of
                         Nothing → r2 f n
                         x       → x

when ∷ (α → Bool) → Rule α β → Rule α β
when pred r = \f n → if pred n
                     then r f n
                     else Nothing

conditional ∷ (α → Bool) → Rule α β → Rule α β → Rule α β
conditional p t e = when p t `combine` e

type Rules α β = [((α, α), Rule α β)]

findRule ∷ (Ord α, Num α) ⇒ (α, Rule α β) → [(α, Rule α β)] → α → Rule α β
findRule x xs end = \f n → case FT.search n xm of
                             [] → Nothing
                             (_,r):_ → r f n
    where
      xm = mkIntervalMap $ mkIntervalList x xs end


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
mulScale ∷ (Integral α, Scale α, Num β, Scale β)
         ⇒ α → α → Side → Side → Rule α β → Rule α β
mulScale base offset aSide mSide bigNumRule =
    \f n → let rank    = (intLog n - offset) `div` base
               base'   = fromIntegral base
               offset' = fromIntegral offset
               rank'   = fromIntegral rank
           in (fix bigNumRule) rank >>= \rankExp →
              let (m, a) = n `divMod` scale base' offset' rank'
                  scale' = Just $ scale base' offset' rankExp
                  mval | m ≡ 1     = scale'
                       | otherwise = liftA2 (flipIfR mSide (⋅))
                                     (f m)
                                     scale'
              in if a ≡ 0
                 then mval
                 else liftA2 (flipIfR aSide (+)) (f a) mval

mulScale1 ∷ (Integral α, Scale α, Num β, Scale β)
          ⇒ α → α → Side → Side → Rule α β → Rule α β
mulScale1 base offset aSide mSide bigNumRule =
    \f n → let rank    = (intLog n - offset) `div` base
               base'   = fromIntegral base
               offset' = fromIntegral offset
               rank'   = fromIntegral rank
           in (fix bigNumRule) rank >>= \rankExp →
              let (m, a) = n `divMod` scale base' offset' rank'
                  mval = liftA2 (flipIfR mSide (⋅))
                                (f m)
                                (Just $ scale base' offset' rankExp)
              in if a ≡ 0
                 then mval
                 else liftA2 (flipIfR aSide (+)) (f a) mval

shortScale ∷ (Integral α, Scale α, Num β, Scale β)
           ⇒ Side → Side → Rule α β → Rule α β
shortScale  = mulScale 3 3

shortScale1 ∷ (Integral α, Scale α, Num β, Scale β)
            ⇒ Side → Side → Rule α β → Rule α β
shortScale1 = mulScale1 3 3

longScale ∷ (Integral α, Scale α, Num β, Scale β)
          ⇒ Side → Side → Rule α β → Rule α β
longScale = mulScale 6 0

longScale1 ∷ (Integral α, Scale α, Num β, Scale β)
           ⇒ Side → Side → Rule α β → Rule α β
longScale1 = mulScale1 6 0

pelletierScale ∷ (Integral α, Scale α, Num β, Scale β)
                ⇒ Side → Side → Rule α β → Rule α β
pelletierScale aSide mSide bigNumRule =
    conditional (\n → even $ intLog n `div` 3)
                (mulScale 6 0 aSide mSide bigNumRule)
                (mulScale 6 3 aSide mSide bigNumRule)

pelletierScale1 ∷ (Integral α, Scale α, Num β, Scale β)
                ⇒ Side → Side → Rule α β → Rule α β
pelletierScale1 aSide mSide bigNumRule =
    conditional (\n → even $ intLog n `div` 3)
                (mulScale1 6 0 aSide mSide bigNumRule)
                (mulScale1 6 3 aSide mSide bigNumRule)


--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

data Side = L | R deriving Show

flipIfR ∷ Side → (α → α → α) → (α → α → α)
flipIfR L = id
flipIfR R = flip

mkIntervalList ∷ (Num a) ⇒ (a, b) → [(a, b)] → a → [((a, a), b)]
mkIntervalList (k, r) krs end = go k r krs
    where
      go k1 r1 []            = [((k1, end), r1)]
      go k1 r1 ((k2, r2):xs) = ((k1, k2-1), r1) : go k2 r2 xs

mkIntervalMap ∷ (Ord v) ⇒ [((v, v), α)] → FT.IntervalMap v α
mkIntervalMap = foldr ins FT.empty
  where ins ((lo, hi), n) = FT.insert (FT.Interval lo hi) n

scaleRules ∷ (Integral α, Num β) ⇒ α → Side → Side → [(α, Rule α β)]
scaleRules g aSide mSide = concatMap step [g, 2⋅g ..]
    where
      step n = [ (s,   atom)
               , (s+1, add s aSide)
               , (2⋅s, mul s aSide mSide)
               ]
          where
            s = dec n

scale1Rules ∷ (Integral α, Num β) ⇒ α → Side → Side → [(α, Rule α β)]
scale1Rules g aSide mSide = concatMap step [g, 2⋅g ..]
    where
      step n = [ (s,   atom1)
               , (s+1, add s aSide)
               , (2⋅s, mul1 s aSide mSide)
               ]
          where
            s = dec n
