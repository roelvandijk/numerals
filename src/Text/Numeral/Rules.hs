{-# LANGUAGE NoImplicitPrelude
           , PackageImports
           , UnicodeSyntax
  #-}

{-|

Rules to convert numbers to an expression language.

-}
module Text.Numeral.Rules
  ( -- * The Rule type
    Rule

    -- * Rule combinators
  , conditional
  , combine
  , mapRule
  , findRule

    -- * Rules
  , unknown

  , lit, lit1

  , pos, checkPos
  , inflection

  , add
  , mul, mul1
  , sub

  , mulScale_, mulScale, mulScale1
  , shortScale,  longScale,  pelletierScale
  , shortScale1, longScale1, pelletierScale1

  , mkStep, step, step1
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool           ( Bool, otherwise )
import "base" Data.Function       ( ($), id, const, flip, fix )
import "base" Data.List           ( foldr )
import "base" Data.Ord            ( Ord, (<), (>) )
import "base" Prelude             ( Integral, fromIntegral
                                  , Num, (-), abs, divMod, div, even
                                  )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Prelude.Unicode       ( (⋅) )
import "this"                 Text.Numeral.Exp.Reified ( Side(L, R) )
import "this"                 Text.Numeral.Misc     ( intLog )
import qualified "this"       Text.Numeral.Exp as E
import qualified "fingertree" Data.IntervalMap.FingerTree as FT
    ( Interval(Interval)
    , IntervalMap, empty, insert
    , search
    )


--------------------------------------------------------------------------------
-- The Rule type
--------------------------------------------------------------------------------

-- | A rule on how to convert a number into an expression
-- language. Notice how this type is equal to the type of the '$'
-- operator.
type Rule α β = (α → β) → (α → β)


--------------------------------------------------------------------------------
-- Rule combinators
--------------------------------------------------------------------------------


-- | The \'if-then-else\' concept for rules. Applies the first rule if
-- the predicate holds on the input value, otherwise applies the
-- second rule.
conditional ∷ (α → Bool) -- ^ Predicate on input value (\"if\").
            → Rule α β -- ^ Rule to apply when predicate holds (\"then\").
            → Rule α β -- ^ Rule to apply when predicate does not hold (\"else\").
            → Rule α β
conditional p t e = \f n → if p n
                           then t f n
                           else e f n

-- | Tries to apply the first rule, if that produces an 'E.unknown'
-- value it applies the second rule.
combine ∷ (E.Unknown β)
        ⇒ Rule α β
        → Rule α β
        → Rule α β
combine r1 r2 = \f n → case r1 f n of
                         x | E.isUnknown x → r2 f n
                           | otherwise     → x

-- | Transform a value before it is given to a rule.
mapRule ∷ (α → α) → Rule α β → Rule α β
mapRule g r = \f n → r f (g n)

-- | Chooses which rule to apply to an input value based on a interval
-- list of rules.
findRule ∷ (Ord α, Num α, E.Unknown β)
         ⇒ (α, Rule α β)   -- ^ First interval rule.
         → [(α, Rule α β)] -- ^ Interval rule list.
         → α               -- ^ Upper bound of the last interval.
         → Rule α β
findRule x xs end = \f n → case FT.search n xm of
                             [] → E.unknown
                             (_,r):_ → r f n
    where
      xm = mkIntervalMap $ mkIntervalList x xs end


--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

-- | A rule that always fails to convert a value. It constantly
-- produces the 'E.unknown' value.
--
-- >>> (fix unknown) (3 :: Integer) :: Exp
-- Unknown
unknown ∷ (E.Unknown β) ⇒ Rule α β
unknown _ _ = E.unknown

-- | The literal rule. Converts its argument into a 'E.lit'eral
-- expression.
--
-- >>> lit (fix unknown) (3 :: Integer) :: Exp
-- Lit 3
--
-- In this example lit is applied to the nonsense rule \"'fix'
-- 'unknown'\". Lit ignores that function, which is why we can pass it
-- anything we want, including itself.
--
-- >>> lit (fix undefined) (3 :: Integer) :: Exp
-- Lit 3
-- >>> (fix lit) (3 :: Integer) :: Exp
-- Lit 3
lit ∷ (Integral α, E.Lit β) ⇒ Rule α β
lit = const $ E.lit ∘ fromIntegral

-- | A variant on the 'lit' rule which always multiplies its argument
-- with 1. Useful for languages which have numerals of the form \"one
-- hundred and three\" as opposed to \"hundred and three\".
--
-- >>> lit1 (fix unknown) (3 :: Integer) :: Exp
-- Mul (Lit 1) (Lit 3)
lit1 ∷ (Integral α, E.Lit β, E.Mul β) ⇒ Rule α β
lit1 = const $ \n → E.lit 1 `E.mul` E.lit (fromIntegral n)

-- |
--
-- >>> (pos $ lit $ fix unknown) (3 :: Integer) :: Exp
-- Lit 3
-- >>> (pos $ lit $ fix unknown) (-3 :: Integer) :: Exp
-- Neg (Lit 3)
pos ∷ (Ord α, Num α, E.Lit β, E.Neg β) ⇒ Rule α β
pos f n | n < 0     = E.neg $ f (abs n)
        | n > 0     = f n
        | otherwise = E.lit 0

-- |
--
-- >>> (checkPos $ lit $ fix unknown) (3 :: Integer) :: Exp
-- Lit 3
-- >>> (checkPos $ lit $ fix unknown) (-3 :: Integer) :: Exp
-- Unknown
checkPos ∷ (Ord α, Num α, E.Unknown β, E.Lit β) ⇒ Rule α β
checkPos f n | n < 0     = E.unknown
             | n > 0     = f n
             | otherwise = E.lit 0

-- | Changes the inflection of a subexpression.
inflection ∷ (E.Inflection β) ⇒ (E.Inf β → E.Inf β) → Rule α β
inflection changeInf = \f n → E.inflection changeInf $ f n

-- |
--
-- >>> (add 10 L $ lit $ fix unknown) (13 :: Integer) :: Exp
-- Add (Lit 3) (Lit 10)
add ∷ (Num α, E.Add β) ⇒ α → Side → Rule α β
add val s = \f n → (flipIfR s E.add) (f $ n - val) (f val)

-- |
--
-- >>> (mul 10 R L $ lit $ fix unknown) (42 :: Integer) :: Exp
-- Add (Mul (Lit 4) (Lit 10)) (Lit 2)
mul ∷ (Integral α, E.Add β, E.Mul β) ⇒ α → Side → Side → Rule α β
mul val aSide mSide =
    \f n → let (m, a) = n `divMod` val
               mval = (flipIfR mSide E.mul) (f m) (f val)
           in if a ≡ 0
              then mval
              else (flipIfR aSide E.add) (f a) mval

mul1 ∷ (Integral α, E.Lit β, E.Add β, E.Mul β)
     ⇒ α → Side → Side → Rule α β
mul1 val aSide mSide =
    \f n → let (m, a) = n `divMod` val
               mval = if m ≡ 1
                      then E.lit 1 ⊡ E.lit (fromIntegral val)
                      else f m ⊡ E.lit (fromIntegral val)
           in if a ≡ 0
              then mval
              else (flipIfR aSide E.add) (f a) mval
  where
     (⊡) = flipIfR mSide E.mul

-- |
--
-- >>> (sub 20 $ lit $ fix unknown) (18 :: Integer) :: Exp
-- Sub (Lit 2) (Lit 20)
sub ∷ (Integral α, E.Sub β) ⇒ α → Rule α β
sub val = \f n → E.sub (f $ val - n) (f val)

mkStep ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β)
       ⇒ Rule α β                     -- ^ lit rule
       → (α → Side → Rule α β)        -- ^ add rule
       → (α → Side → Side → Rule α β) -- ^ mul rule
       → α → α → Side → Side → Rule α β
mkStep lr ar mr val r aSide mSide
       f n | n < val   = E.unknown
           | n ≡ val   = lr                 f n
           | n < val⋅2 = ar val aSide       f n
           | n < val⋅r = mr val aSide mSide f n
           | otherwise = E.unknown

step ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β)
     ⇒ α → α → Side → Side → Rule α β
step = mkStep lit add mul

step1 ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β)
      ⇒ α → α → Side → Side → Rule α β
step1 = mkStep lit1 add mul1

mulScale_ ∷ (Integral α, E.Scale α, E.Unknown β, E.Add β, E.Mul β, E.Scale β)
          ⇒ ( (α → β) -- Parent rule.
            → α       -- First multiplication value (not converted).
            → β       -- Second multiplication value (scale step,
                      -- already converted).
            → Side    -- Multiplication side.
            → β
            )        -- ^ Performs the multiplication.
          → α        -- ^ Base.
          → α        -- ^ Offset.
          → Side     -- ^ Add side.
          → Side     -- ^ Mul side.
          → Rule α β -- ^ Big num rule.
          → Rule α β
mulScale_ doMul base offset aSide mSide bigNumRule =
    \f n → let rank    = (intLog n - offset) `div` base
               base'   = fromIntegral base
               offset' = fromIntegral offset
               rank'   = fromIntegral rank
               rankExp = (fix bigNumRule) rank
               (m, a)  = n `divMod` E.scale base' offset' rank'
               scale'  = E.scale base' offset' rankExp
               mval    = doMul f m scale' mSide
           in if E.isUnknown rankExp
              then E.unknown
              else if a ≡ 0
                   then mval
                   else (flipIfR aSide E.add) (f a) mval

mulScale ∷ (Integral α, E.Scale α, E.Unknown β, E.Add β, E.Mul β, E.Scale β)
         ⇒ α        -- ^ Base.
         → α        -- ^ Offset.
         → Side     -- ^ Add side.
         → Side     -- ^ Mul side.
         → Rule α β -- ^ Big num rule.
         → Rule α β
mulScale = mulScale_ $ \f m scale' mSide →
                         case m of
                           1 → scale'
                           _ → (flipIfR mSide E.mul) (f m) scale'

mulScale1 ∷ (Integral α, E.Scale α, E.Unknown β, E.Add β, E.Mul β, E.Scale β)
          ⇒ α        -- ^ Base.
          → α        -- ^ Offset.
          → Side     -- ^ Add side.
          → Side     -- ^ Mul side.
          → Rule α β -- ^ Big num rule.
          → Rule α β
mulScale1 = mulScale_ $ \f m scale' mSide → (flipIfR mSide E.mul) (f m) scale'

shortScale ∷ (Integral α, E.Scale α, E.Unknown β, E.Add β, E.Mul β, E.Scale β)
           ⇒ Side     -- ^ Add side.
           → Side     -- ^ Mul side.
           → Rule α β -- ^ Big num rule.
           → Rule α β
shortScale = mulScale 3 3

shortScale1 ∷ (Integral α, E.Scale α, E.Unknown β, E.Add β, E.Mul β, E.Scale β)
            ⇒ Side     -- ^ Add side.
            → Side     -- ^ Mul side.
            → Rule α β -- ^ Big num rule.
            → Rule α β
shortScale1 = mulScale1 3 3

longScale ∷ (Integral α, E.Scale α, E.Unknown β, E.Add β, E.Mul β, E.Scale β)
          ⇒ Side     -- ^ Add side.
          → Side     -- ^ Mul side.
          → Rule α β -- ^ Big num rule.
          → Rule α β
longScale = mulScale 6 0

longScale1 ∷ (Integral α, E.Scale α, E.Unknown β, E.Add β, E.Mul β, E.Scale β)
           ⇒ Side     -- ^ Add side.
           → Side     -- ^ Mul side.
           → Rule α β -- ^ Big num rule.
           → Rule α β
longScale1 = mulScale1 6 0

pelletierScale ∷ (Integral α, E.Scale α, E.Unknown β, E.Add β, E.Mul β, E.Scale β)
               ⇒ Side     -- ^ Add side.
               → Side     -- ^ Mul side.
               → Rule α β -- ^ Big num rule.
               → Rule α β
pelletierScale aSide mSide bigNumRule =
    conditional (\n → even $ intLog n `div` 3)
                (mulScale 6 0 aSide mSide bigNumRule)
                (mulScale 6 3 aSide mSide bigNumRule)

pelletierScale1 ∷ (Integral α, E.Scale α, E.Unknown β, E.Add β, E.Mul β, E.Scale β)
                ⇒ Side     -- ^ Add side.
                → Side     -- ^ Mul side.
                → Rule α β -- ^ Big num rule.
                → Rule α β
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

