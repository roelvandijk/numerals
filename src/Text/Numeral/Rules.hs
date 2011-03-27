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
  , pos, checkPos

  , lit, lit1
  , add
  , mul, mul1
  , sub

  , mulScale, mulScale1
  , shortScale,  longScale,  pelletierScale
  , shortScale1, longScale1, pelletierScale1

  , mkStep, step, step1
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
import Data.List           ( foldr )
import Data.Maybe          ( Maybe(Nothing, Just) )
import Data.Ord            ( Ord, (<), (>) )
import Prelude             ( Integral, fromIntegral
                           , Num, (-), abs, divMod, div, even
                           )

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Prelude.Unicode       ( (⋅) )

-- from numerals:
import qualified Text.Numeral.Exp.Classes as C
import Text.Numeral.Exp ( Side(L, R) )
import Text.Numeral.Misc ( intLog )

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

pos ∷ (Ord α, Num α, C.Lit β, C.Neg β) ⇒ Rule α β
pos f n | n < 0     = C.neg <$> f (abs n)
        | n > 0     = f n
        | otherwise = Just $ C.lit 0

checkPos ∷ (Ord α, Num α, C.Lit β) ⇒ Rule α β
checkPos f n | n < 0     = Nothing
             | n > 0     = f n
             | otherwise = Just $ C.lit 0

lit ∷ (Integral α, C.Lit β) ⇒ Rule α β
lit = const $ Just ∘ C.lit ∘ fromIntegral

lit1 ∷ (Integral α, C.Lit β, C.Mul β) ⇒ Rule α β
lit1 = const $ \n → Just $ C.lit 1 `C.mul` C.lit (fromIntegral n)

add ∷ (Num α, C.Add β) ⇒ α → Side → Rule α β
add val s = \f n → liftA2 (flipIfR s C.add) (f $ n - val) (f val)

mul ∷ (Integral α, C.Add β, C.Mul β) ⇒ α → Side → Side → Rule α β
mul val aSide mSide =
    \f n → let (m, a) = n `divMod` val
               mval = liftA2 (flipIfR mSide C.mul) (f m) (f val)
           in if a ≡ 0
              then mval
              else liftA2 (flipIfR aSide C.add) (f a) mval

mul1 ∷ (Integral α, C.Lit β, C.Add β, C.Mul β)
     ⇒ α → Side → Side → Rule α β
mul1 val aSide mSide =
    \f n → let (m, a) = n `divMod` val
               mval = if m ≡ 1
                      then Just $ C.lit 1 ⊡ C.lit (fromIntegral val)
                      else (⊡ C.lit (fromIntegral val)) <$> f m
           in if a ≡ 0
              then mval
              else liftA2 (flipIfR aSide C.add) (f a) mval
  where
     (⊡) = flipIfR mSide C.mul

sub ∷ (Integral α, C.Sub β) ⇒ α → Rule α β
sub val = \f n → liftA2 C.sub (f $ val - n) (f val)

mkStep ∷ (Integral α, C.Lit β, C.Add β, C.Mul β)
       ⇒ Rule α β -- ^ lit rule
       → (α → Side → Rule α β) -- ^ add rule
       → (α → Side → Side → Rule α β) -- ^ mul rule
       → α → α → Side → Side → Rule α β
mkStep lr ar mr val r aSide mSide
       f n | n < val   = Nothing
           | n ≡ val   = lr                 f n
           | n < val⋅2 = ar val aSide       f n
           | n < val⋅r = mr val aSide mSide f n
           | otherwise = Nothing

step ∷ (Integral α, C.Lit β, C.Add β, C.Mul β)
     ⇒ α → α → Side → Side → Rule α β
step = mkStep lit add mul

step1 ∷ (Integral α, C.Lit β, C.Add β, C.Mul β)
      ⇒ α → α → Side → Side → Rule α β
step1 = mkStep lit1 add mul1

-- See: http://en.wikipedia.org/wiki/Names_of_large_numbers
mulScale ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
         ⇒ α → α → Side → Side → Rule α β → Rule α β
mulScale base offset aSide mSide bigNumRule =
    \f n → let rank    = (intLog n - offset) `div` base
               base'   = fromIntegral base
               offset' = fromIntegral offset
               rank'   = fromIntegral rank
           in (fix bigNumRule) rank >>= \rankExp →
              let (m, a) = n `divMod` C.scale base' offset' rank'
                  scale' = Just $ C.scale base' offset' rankExp
                  mval | m ≡ 1     = scale'
                       | otherwise = liftA2 (flipIfR mSide C.mul)
                                     (f m)
                                     scale'
              in if a ≡ 0
                 then mval
                 else liftA2 (flipIfR aSide C.add) (f a) mval

mulScale1 ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
          ⇒ α → α → Side → Side → Rule α β → Rule α β
mulScale1 base offset aSide mSide bigNumRule =
    \f n → let rank    = (intLog n - offset) `div` base
               base'   = fromIntegral base
               offset' = fromIntegral offset
               rank'   = fromIntegral rank
           in (fix bigNumRule) rank >>= \rankExp →
              let (m, a) = n `divMod` C.scale base' offset' rank'
                  mval = liftA2 (flipIfR mSide C.mul)
                                (f m)
                                (Just $ C.scale base' offset' rankExp)
              in if a ≡ 0
                 then mval
                 else liftA2 (flipIfR aSide C.add) (f a) mval

shortScale ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
           ⇒ Side → Side → Rule α β → Rule α β
shortScale  = mulScale 3 3

shortScale1 ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
            ⇒ Side → Side → Rule α β → Rule α β
shortScale1 = mulScale1 3 3

longScale ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
          ⇒ Side → Side → Rule α β → Rule α β
longScale = mulScale 6 0

longScale1 ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
           ⇒ Side → Side → Rule α β → Rule α β
longScale1 = mulScale1 6 0

pelletierScale ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
                ⇒ Side → Side → Rule α β → Rule α β
pelletierScale aSide mSide bigNumRule =
    conditional (\n → even $ intLog n `div` 3)
                (mulScale 6 0 aSide mSide bigNumRule)
                (mulScale 6 3 aSide mSide bigNumRule)

pelletierScale1 ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
                ⇒ Side → Side → Rule α β → Rule α β
pelletierScale1 aSide mSide bigNumRule =
    conditional (\n → even $ intLog n `div` 3)
                (mulScale1 6 0 aSide mSide bigNumRule)
                (mulScale1 6 3 aSide mSide bigNumRule)


--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

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

