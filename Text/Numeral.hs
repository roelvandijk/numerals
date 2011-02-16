{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , PatternGuards
           , RecordWildCards
           , NamedFieldPuns
           , ScopedTypeVariables
           , OverloadedStrings
  #-}

module Text.Numeral
    ( Val(..)
    , Exp(..)
    , Rule(..)
    , RuleType(..)
    , AddType(..)
    , Rules(..)
    , deconstruct
    , findRule

    , SymbolRepr
    , SymbolContext(..)
    , Repr(..)
    , cardinal

    , atom
    , add
    , mul
    )
    where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad ( return )
import Data.Bool     ( Bool(False, True), otherwise )
import Data.Eq       ( Eq )
import Data.Function ( ($)  )
import Data.Functor  ( (<$>) )
import Data.Maybe    ( Maybe(Nothing, Just) )
import Data.Monoid   ( Monoid )
import Data.Ord      ( (<), (>) )
import Data.String   ( IsString )
import Prelude       ( Num, (+), (*), (-), negate, abs, signum, fromInteger
                     , Integer, divMod, error, abs
                     )
import Text.Show     ( Show )

-- from base-unicode-symbols:
import Data.Bool.Unicode   ( (∧) )
import Data.Eq.Unicode     ( (≡) )
import Data.Monoid.Unicode ( (⊕) )
import Data.Ord.Unicode    ( (≥) )
import Prelude.Unicode     ( (⋅) )

-- from string-combinators:
import Data.String.Combinators ( (<+>) )


-------------------------------------------------------------------------------
-- Structure of numerals
-------------------------------------------------------------------------------

data Val α = Pos α
           | Neg α
           | Zero
             deriving Show

data Exp = Exp :+: Exp
         | Exp :⋅: Exp
         | C Integer -- must be > 0
           deriving (Eq, Show)

instance Num Exp where
    x + y = x :+: y
    x * y = x :⋅: y
    fromInteger = C

    _ - _    = error "not implemented"
    negate _ = error "not implemented"
    abs    _ = error "not implemented"
    signum _ = error "not implemented"

infixl 6 :+:
infixl 7 :⋅:


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Rule = Rule { rType    ∷ RuleType
                 , rVal     ∷ Integer
                 , rPos     ∷ Integer
                 , rScope   ∷ Integer
                 , rAddType ∷ AddType
                 , rTerm    ∷ Bool
                 } deriving Show

data RuleType = Atom | Add | Mul deriving Show

data AddType = LeftAdd
             | RightAdd
               deriving Show

data Rules = Rules { rsFindRule ∷ Maybe Integer → Integer → Rule
                   , rsMulOne   ∷ Integer → Bool
                   }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

deconstruct ∷ ∀ α. (Num α) ⇒ Rules → Integer → Val α
deconstruct (Rules {..}) i
    | i < 0 = Neg $ go Nothing True (abs i)
    | i > 0 = Pos $ go Nothing True i
    | otherwise = Zero
    where
      go ∷ Maybe Integer → Bool → Integer → α
      go m o n = let (Rule {rVal, rTerm, rAddType}) = rsFindRule m n
                     x `plus` y = case rAddType of
                                    RightAdd → x + y
                                    LeftAdd  → y + x
                     infixl 6 `plus`
                 in case n `divMod` rVal of
                      (1, r) → let one | rTerm     = fromInteger rVal
                                       | otherwise = go (Just rVal) True rVal
                                   one' | o ∧ rsMulOne rVal = 1 ⋅ one
                                        | otherwise         = one
                               in if r ≡ 0
                                  then one'
                                  else one' `plus` go Nothing True r

                      (q, 0) → go Nothing True q ⋅ go Nothing False rVal
                      (q, r) → go Nothing True q ⋅ go Nothing False rVal `plus` go Nothing True r

findRule ∷ [Rule] → Maybe Integer → Integer → Rule
findRule []     _   _ = error "empty rule table"
findRule (e:es) mmp n = go e e es
    where go ∷ Rule → Rule → [Rule] → Rule
          go a m [] = stop a m
          go a m (x@(Rule {..}) : xs)
              | Just mp ← mmp, rPos ≥ mp = stop a m
              | rPos ≡ n = x
              | otherwise = case rType of
                              Atom            → go a m xs
                              Add | rPos > n  → stop a m
                                  | otherwise → go x m xs
                              Mul | rPos > n  → stop a m
                                  | otherwise → go a x xs

          stop ∷ Rule → Rule → Rule
          stop a@(Rule {..}) m | n < rPos + rScope = a
                               | otherwise         = m


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

atom ∷ Integer → Rule
atom n = Rule Atom n n 1 RightAdd True

add ∷ Integer → Integer → Integer → AddType → Bool → Rule
add v p s a t = Rule Add v p s a t

mul ∷ Integer → Integer → Integer → AddType → Rule
mul v p s a = Rule Mul v p s a True


--------------------------------------------------------------------------------
-- Representation of numerals
--------------------------------------------------------------------------------

type SymbolRepr s = SymbolContext → s

data SymbolContext = Empty
                   | LA Exp SymbolContext
                   | RA Exp SymbolContext
                   | LM Exp SymbolContext
                   | RM Exp SymbolContext
                     deriving Show

data Repr s = Repr { reprValue ∷ Integer → Maybe (SymbolContext → s)
                   , reprAdd   ∷ Exp → Exp → s
                   , reprMul   ∷ Exp → Exp → s
                   , reprZero  ∷ s
                   , reprNeg   ∷ s
                   }


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


cardinal ∷ ∀ s. (Monoid s, IsString s) ⇒ Repr s → Val Exp → Maybe s
cardinal (Repr {reprZero}) Zero = Just reprZero
cardinal repr (Neg x) = (reprNeg repr <+>) <$> cardinal repr (Pos x)
cardinal (Repr {..}) (Pos e) = go Empty e
    where
      go ∷ SymbolContext → Exp → Maybe s
      go ctx (C n) = ($ ctx) <$> reprValue n
      go ctx (x :+: y) = do x' ← go (LA y ctx) x
                            y' ← go (RA x ctx) y
                            return $ x' ⊕ reprAdd x y ⊕ y'
      go ctx (x :⋅: y) = do x' ← go (LM y ctx) x
                            y' ← go (RM x ctx) y
                            return $ x' ⊕ reprMul x y ⊕ y'
