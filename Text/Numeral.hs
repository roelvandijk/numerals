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

      -- * Structure of numerals
    , deconstruct

      -- * Representation of numerals
    , Exp(..)
    , Subtract(subtract)

    , SymbolContext(..)
    , Repr(..)
    , textify
    )
    where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad ( return )
import Data.Bool     ( Bool, otherwise )
import Data.Eq             ( Eq )
import Data.Function ( ($) )
import Data.Functor  ( (<$>), fmap )
import Data.List     ( foldr, find )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( Ord, (<), (>) )
import Data.String   ( IsString )
import Data.Tuple    ( fst, snd )
import Prelude       ( Num, Integral, Integer, fromInteger
                     , (+), (*), (-), signum, abs, negate, error
                     )
import Text.Show     ( Show )
import qualified Prelude as P ( subtract )

-- from base-unicode-symbols:
import Data.Bool.Unicode     ( (∧) )
import Data.Function.Unicode ( (∘) )
import Data.Monoid.Unicode   ( (⊕) )
import Data.Ord.Unicode      ( (≤) )

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
-- Structure of numerals
--------------------------------------------------------------------------------


data Exp = Exp :+: Exp
         | Exp :*: Exp
         | Sub Exp Exp
         | Neg Exp
         | C Integer
           deriving (Eq, Show)

infixl 6 :+:
infixl 7 :*:

instance Num Exp where
    (+)         = (:+:)
    (*)         = (:*:)
    negate      = Neg
    fromInteger = C

    (-)    = error "not implemented"
    abs    = error "not implemented"
    signum = error "not implemented"

class (Num α) ⇒ Subtract α where
    subtract ∷ α → α → α
    subtract = P.subtract

instance Subtract Integer

instance Subtract Exp where
    subtract = Sub


--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------

data SymbolContext = Empty
                   | AddL Exp SymbolContext
                   | AddR Exp SymbolContext
                   | MulL Exp SymbolContext
                   | MulR Exp SymbolContext
                   | SubL Exp SymbolContext
                   | SubR Exp SymbolContext
                     deriving Show

data Repr s = Repr { reprValue ∷ Integer → Maybe (SymbolContext → s)
                   , reprAdd   ∷ Exp → Exp → s
                   , reprMul   ∷ Exp → Exp → s
                   , reprSub   ∷ Exp → Exp → s
                   , reprNeg   ∷ s
                   }


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

textify ∷ (Monoid s, IsString s) ⇒ Repr s → Exp → Maybe s
textify (Repr {..}) e = go Empty e
    where
      go ctx (C n) = ($ ctx) <$> reprValue n
      go ctx (Neg x) = (reprNeg ⊕) <$> go ctx x
      go ctx (x :+: y) = do x' ← go (AddL y ctx) x
                            y' ← go (AddR x ctx) y
                            return $ x' ⊕ reprAdd x y ⊕ y'
      go ctx (x :*: y) = do x' ← go (MulL y ctx) x
                            y' ← go (MulR x ctx) y
                            return $ x' ⊕ reprMul x y ⊕ y'
      go ctx (Sub x y) = do x' ← go (SubL y ctx) x
                            y' ← go (SubR x ctx) y
                            return $ x' ⊕ reprSub x y ⊕ y'
