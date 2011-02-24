{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , PatternGuards
           , RecordWildCards
           , NamedFieldPuns
           , ScopedTypeVariables
           , OverloadedStrings
  #-}

module Text.Numeral
    ( -- * Intervals
    --   Interval
    -- , IntervalMap
    -- , lookupInterval

      -- * Rules
      Rule
    , Rules
    , FindRule
    , mkFindRule
    , Side(..)
    , atom, atom1
    , add
    , mul, mul1

      -- * Structure of numerals
    , deconstruct

      -- * Representation of numerals
    , Exp(..)
    , SymbolContext(..)
    , Repr(..)
    , textify
    )
    where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative ( liftA2 )
import Control.Monad       ( return )
import Data.Bool           ( Bool, otherwise )
import Data.Eq             ( Eq )
import Data.Function       ( ($), id, const, flip )
import Data.Functor        ( (<$>), fmap )
import Data.List           ( foldr, find )
import Data.Maybe          ( Maybe(Just) )
import Data.Monoid         ( Monoid )
import Data.Ord            ( Ord, (<), (>) )
import Data.String         ( IsString )
import Data.Tuple          ( fst, snd )
import Prelude             ( Num, (+), (*), (-), negate, abs, signum
                           , Integral, fromIntegral
                           , Integer, fromInteger, divMod, error, abs
                           )
import Text.Show           ( Show )

-- from base-unicode-symbols:
import Data.Bool.Unicode     ( (∧) )
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Data.Monoid.Unicode   ( (⊕) )
import Data.Ord.Unicode      ( (≤) )
import Prelude.Unicode       ( (⋅) )

-- from fingertree:
import qualified Data.IntervalMap.FingerTree as FT
    ( Interval(Interval)
    , IntervalMap, empty, insert
    , search
    )


--------------------------------------------------------------------------------
-- Intervals
--------------------------------------------------------------------------------

type Interval i = (i, i)
type IntervalMap i α = [(Interval i, α)]

lookupInterval ∷ (Ord i) ⇒ i → IntervalMap i α → Maybe α
lookupInterval x = fmap snd ∘ find (inInterval x ∘ fst)

inInterval ∷ (Ord i) ⇒ i → Interval i → Bool
inInterval x (lo, hi) = lo ≤ x ∧ x ≤ hi


--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

type Rule     α β = (α → Maybe β) → (α → Maybe β)
type Rules    α β =  [((α, α), Rule α β)]
type FindRule α β = α → Maybe (Rule α β)


-- Precondition: xs is finite
mkFindRule ∷ (Integral α, Num β) ⇒ Rules α β → Rules α β → FindRule α β
mkFindRule xs ys = \n → case FT.search n xm of
                          []  → lookupInterval n ys
                          x:_ → Just $ snd x
    where
      xm = mkIntervalMap xs

mkIntervalMap ∷ (Ord v) ⇒ [((v, v), α)] → FT.IntervalMap v α
mkIntervalMap = foldr ins FT.empty
  where ins ((lo, hi), n) = FT.insert (FT.Interval lo hi) n

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Side = L | R deriving Show

flipIfR ∷ Side → (α → α → α) → (α → α → α)
flipIfR L = id
flipIfR R = flip

--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
-- Structure of numerals
--------------------------------------------------------------------------------

deconstruct ∷ ∀ α β. (Integral α, Num β) ⇒ FindRule α β → α → Maybe β
deconstruct findRule n
    | n < 0 = negate <$> toExp (abs n)
    | n > 0 = toExp n
    | otherwise = Just 0
    where
      toExp ∷ α → Maybe β
      toExp x = do r ← findRule x
                   r toExp x


--------------------------------------------------------------------------------
-- Representation of numerals
--------------------------------------------------------------------------------

data Exp = Exp :+: Exp
         | Exp :*: Exp
         | Neg Exp
         | C Integer
           deriving (Eq, Show)

infixl 6 :+:
infixl 7 :*:

instance Num Exp where
    x + y       = x :+: y
    x * y       = x :*: y
    negate      = Neg
    fromInteger = C

    _ - _    = error "not implemented"
    abs    _ = error "not implemented"
    signum _ = error "not implemented"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data SymbolContext = Empty
                   | LA Exp SymbolContext
                   | RA Exp SymbolContext
                   | LM Exp SymbolContext
                   | RM Exp SymbolContext
                     deriving Show

data Repr s = Repr { reprValue ∷ Integer → Maybe (SymbolContext → s)
                   , reprAdd   ∷ Exp → Exp → s
                   , reprMul   ∷ Exp → Exp → s
                   , reprNeg   ∷ s
                   }


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


textify ∷ (Monoid s, IsString s) ⇒ Repr s → Exp → Maybe s
textify (Repr {..}) e = go Empty e
    where
      go ctx (C n) = ($ ctx) <$> reprValue n
      go ctx (Neg x) = (reprNeg ⊕) <$> go ctx x
      go ctx (x :+: y) = do x' ← go (LA y ctx) x
                            y' ← go (RA x ctx) y
                            return $ x' ⊕ reprAdd x y ⊕ y'
      go ctx (x :*: y) = do x' ← go (LM y ctx) x
                            y' ← go (RM x ctx) y
                            return $ x' ⊕ reprMul x y ⊕ y'
