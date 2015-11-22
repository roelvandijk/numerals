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

  , add
  , mul, mul1
  , sub

  , mulScale_, mulScale, mulScale1
  , shortScale,  longScale,  pelletierScale
  , shortScale1, longScale1, pelletierScale1

  , mkStep, step, step1

    -- ** Grammar Rules
  , changeCase
  , changeGender
  , changeNumber
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import           "this" Text.Numeral.Exp ( Side(L, R) )
import qualified "this" Text.Numeral.Exp as E
import qualified "this" Text.Numeral.Grammar as G
import           "this" Text.Numeral.Misc ( intLog )
import qualified "fingertree" Data.IntervalMap.FingerTree as FT
    ( Interval(Interval)
    , IntervalMap, empty, insert
    , search
    )


--------------------------------------------------------------------------------
-- The Rule type
--------------------------------------------------------------------------------

-- | A rule on how to convert a number into an expression
-- language. Notice how this type is similar to the type of the '$'
-- operator.
type Rule a = (a -> E.Exp) -> (a -> E.Exp)


--------------------------------------------------------------------------------
-- Rule combinators
--------------------------------------------------------------------------------


-- | The \'if-then-else\' concept for rules. Applies the first rule if
-- the predicate holds on the input value, otherwise applies the
-- second rule.
conditional :: (a -> Bool) -- ^ Predicate on input value (\"if\").
            -> Rule a -- ^ Rule to apply when predicate holds (\"then\").
            -> Rule a -- ^ Rule to apply when predicate does not hold (\"else\").
            -> Rule a
conditional p t e = \f n -> if p n
                           then t f n
                           else e f n

-- | Tries to apply the first rule, if that produces an 'E.Unknown'
-- value it applies the second rule.
combine :: Rule a
        -> Rule a
        -> Rule a
combine r1 r2 = \f n -> case r1 f n of
                          E.Unknown -> r2 f n
                          x         -> x

-- | Transform a value before it is given to a rule.
mapRule :: (a -> a) -> Rule a -> Rule a
mapRule g r = \f n -> r f (g n)

-- | Chooses which rule to apply to an input value based on a interval
-- list of rules.
findRule :: (Ord a, Num a)
         => (a, Rule a)   -- ^ First interval rule.
         -> [(a, Rule a)] -- ^ Interval rule list.
         -> a             -- ^ Upper bound of the last interval.
         -> Rule a
findRule x xs end = \f n -> case FT.search n xm of
                             [] -> E.Unknown
                             (_,r):_ -> r f n
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
unknown :: Rule a
unknown _ = const E.Unknown

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
lit :: (Integral a) => Rule a
lit = const $ E.Lit . fromIntegral

-- | A variant on the 'lit' rule which always multiplies its argument
-- with 1. Useful for languages which have numerals of the form \"one
-- hundred and three\" as opposed to \"hundred and three\".
--
-- >>> lit1 (fix unknown) (3 :: Integer) :: Exp
-- Mul (Lit 1) (Lit 3)
lit1 :: (Integral a) => Rule a
lit1 = const $ \n -> E.Lit 1 `E.Mul` E.Lit (fromIntegral n)

-- |
--
-- >>> (pos $ lit $ fix unknown) (3 :: Integer) :: Exp
-- Lit 3
-- >>> (pos $ lit $ fix unknown) (-3 :: Integer) :: Exp
-- Neg (Lit 3)
pos :: (Ord a, Num a) => Rule a
pos f n | n < 0     = E.Neg $ f (abs n)
        | n > 0     = f n
        | otherwise = E.Lit 0

-- |
--
-- >>> (checkPos $ lit $ fix unknown) (3 :: Integer) :: Exp
-- Lit 3
-- >>> (checkPos $ lit $ fix unknown) (-3 :: Integer) :: Exp
-- Unknown
checkPos :: (Ord a, Num a) => Rule a
checkPos f n | n < 0     = E.Unknown
             | n > 0     = f n
             | otherwise = E.Lit 0

-- -- | Changes the inflection of a subexpression.
-- inflection :: (E.Inflection b) => (Inflection -> Inflection) -> Rule a
-- inflection changeInf = \f n -> E.inflection changeInf $ f n

-- |
--
-- >>> (add 10 L $ lit $ fix unknown) (13 :: Integer) :: Exp
-- Add (Lit 3) (Lit 10)
add :: (Num a) => a -> Side -> Rule a
add val s = \f n -> (flipIfR s E.Add) (f $ n - val) (f val)

-- |
--
-- >>> (mul 10 R L $ lit $ fix unknown) (42 :: Integer) :: Exp
-- Add (Mul (Lit 4) (Lit 10)) (Lit 2)
mul :: (Integral a) => a -> Side -> Side -> Rule a
mul val aSide mSide =
    \f n -> let (m, a) = n `divMod` val
                mval = (flipIfR mSide E.Mul) (f m) (f val)
            in if a == 0
               then mval
               else (flipIfR aSide E.Add) (f a) mval

mul1 :: (Integral a) => a -> Side -> Side -> Rule a
mul1 val aSide mSide =
    \f n -> let (m, a) = n `divMod` val
                mval = if m == 1
                       then E.Lit 1 ⊡ E.Lit (fromIntegral val)
                       else f m     ⊡ E.Lit (fromIntegral val)
            in if a == 0
               then mval
               else (flipIfR aSide E.Add) (f a) mval
  where
     (⊡) = flipIfR mSide E.Mul

-- |
--
-- >>> (sub 20 $ lit $ fix unknown) (18 :: Integer) :: Exp
-- Sub (Lit 2) (Lit 20)
sub :: (Integral a) => a -> Rule a
sub val = \f n -> E.Sub (f $ val - n) (f val)

mkStep :: (Integral a)
       => Rule a                        -- ^ lit rule
       -> (a -> Side -> Rule a)         -- ^ add rule
       -> (a -> Side -> Side -> Rule a) -- ^ mul rule
       -> a -> a -> Side -> Side -> Rule a
mkStep lr ar mr val r aSide mSide
       f n | n <  val   = E.Unknown
           | n == val   = lr                 f n
           | n <  val*2 = ar val aSide       f n
           | n <  val*r = mr val aSide mSide f n
           | otherwise  = E.Unknown

step :: (Integral a) => a -> a -> Side -> Side -> Rule a
step = mkStep lit add mul

step1 :: (Integral a) => a -> a -> Side -> Side -> Rule a
step1 = mkStep lit1 add mul1

mulScale_ :: forall a. (Integral a)
          => ( (a -> E.Exp) -- Parent rule.
             -> a       -- First multiplication value (not converted).
             -> E.Exp   -- Second multiplication value (scale step,
                        -- already converted).
             -> Side    -- Multiplication side.
             -> E.Exp
             )      -- ^ Performs the multiplication.
          -> a      -- ^ Base.
          -> a      -- ^ Offset.
          -> Side   -- ^ Add side.
          -> Side   -- ^ Mul side.
          -> Rule a -- ^ Big num rule.
          -> Rule a
mulScale_ doMul base offset aSide mSide bigNumRule =
    \f n -> let rank = (intLog n - offset) `div` base

                base' :: Integer
                base' = fromIntegral base

                offset' :: Integer
                offset' = fromIntegral offset

                rankExp :: E.Exp
                rankExp = (fix bigNumRule) rank

                m, a :: a
                (m, a) = n `divMod` E.evalScale base offset rank

                scale' :: E.Exp
                scale' = E.Scale base' offset' rankExp

                mval = doMul f m scale' mSide

            in case rankExp of
                 E.Unknown -> E.Unknown
                 _ -> if a == 0
                      then mval
                      else (flipIfR aSide E.Add) (f a) mval

mulScale :: (Integral a)
         => a      -- ^ Base.
         -> a      -- ^ Offset.
         -> Side   -- ^ Add side.
         -> Side   -- ^ Mul side.
         -> Rule a -- ^ Big num rule.
         -> Rule a
mulScale = mulScale_ $ \f m scale' mSide ->
                         case m of
                           1 -> scale'
                           _ -> (flipIfR mSide E.Mul) (f m) scale'

mulScale1 :: (Integral a)
          => a      -- ^ Base.
          -> a      -- ^ Offset.
          -> Side   -- ^ Add side.
          -> Side   -- ^ Mul side.
          -> Rule a -- ^ Big num rule.
          -> Rule a
mulScale1 = mulScale_ $ \f m scale' mSide -> (flipIfR mSide E.Mul) (f m) scale'

shortScale :: (Integral a)
           => Side   -- ^ Add side.
           -> Side   -- ^ Mul side.
           -> Rule a -- ^ Big num rule.
           -> Rule a
shortScale = mulScale 3 3

shortScale1 :: (Integral a)
            => Side   -- ^ Add side.
            -> Side   -- ^ Mul side.
            -> Rule a -- ^ Big num rule.
            -> Rule a
shortScale1 = mulScale1 3 3

longScale :: (Integral a)
          => Side   -- ^ Add side.
          -> Side   -- ^ Mul side.
          -> Rule a -- ^ Big num rule.
          -> Rule a
longScale = mulScale 6 0

longScale1 :: (Integral a)
           => Side   -- ^ Add side.
           -> Side   -- ^ Mul side.
           -> Rule a -- ^ Big num rule.
           -> Rule a
longScale1 = mulScale1 6 0

pelletierScale :: (Integral a)
               => Side   -- ^ Add side.
               -> Side   -- ^ Mul side.
               -> Rule a -- ^ Big num rule.
               -> Rule a
pelletierScale aSide mSide bigNumRule =
    conditional (\n -> even $ intLog n `div` 3)
                (mulScale 6 0 aSide mSide bigNumRule)
                (mulScale 6 3 aSide mSide bigNumRule)

pelletierScale1 :: (Integral a)
                => Side   -- ^ Add side.
                -> Side   -- ^ Mul side.
                -> Rule a -- ^ Big num rule.
                -> Rule a
pelletierScale1 aSide mSide bigNumRule =
    conditional (\n -> even $ intLog n `div` 3)
                (mulScale1 6 0 aSide mSide bigNumRule)
                (mulScale1 6 3 aSide mSide bigNumRule)


--------------------------------------------------------------------------------
-- Grammar Rules
--------------------------------------------------------------------------------

changeCase :: Maybe G.Case -> Rule a
changeCase mbCase = \f n -> E.ChangeCase mbCase $ f n

changeGender :: Maybe G.Gender -> Rule a
changeGender mbGender = \f n -> E.ChangeGender mbGender $ f n

changeNumber :: Maybe G.Number -> Rule a
changeNumber mbNumber = \f n -> E.ChangeNumber mbNumber $ f n

--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

flipIfR :: Side -> (a -> a -> a) -> (a -> a -> a)
flipIfR L = id
flipIfR R = flip

mkIntervalList :: (Num a) => (a, b) -> [(a, b)] -> a -> [((a, a), b)]
mkIntervalList (k, r) krs end = go k r krs
    where
      go k1 r1 []            = [((k1, end), r1)]
      go k1 r1 ((k2, r2):xs) = ((k1, k2-1), r1) : go k2 r2 xs

mkIntervalMap :: (Ord v) => [((v, v), a)] -> FT.IntervalMap v a
mkIntervalMap = foldr ins FT.empty
  where ins ((lo, hi), n) = FT.insert (FT.Interval lo hi) n
