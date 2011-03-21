{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Text.Numeral.Exp
    ( Exp(..)
    , eval
    , Side(L, R)
    , Ctx(..)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Eq   ( Eq )
import Data.Ord  ( Ord )
import Prelude   ( Integer )
import Text.Show ( Show )

-- from numerals:
import qualified Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- Exp datatype
-------------------------------------------------------------------------------

data Exp = Lit Integer
         | Neg Exp
         | Add Exp Exp
         | Mul Exp Exp
         | Sub Exp Exp
         | Scale Integer Integer Exp
           deriving (Eq, Ord, Show)

infixl 6 `Add`
infixl 6 `Sub`
infixl 7 `Mul`

instance C.Lit Exp where lit = Lit
instance C.Neg Exp where neg = Neg
instance C.Add Exp where add = Add
instance C.Mul Exp where mul = Mul
instance C.Sub Exp where sub = Sub
instance C.Scale Exp where scale = Scale

eval ∷ (C.Lit α, C.Neg α, C.Add α, C.Mul α, C.Sub α, C.Scale α) ⇒ Exp → α
eval (Add x y)     = C.add (eval x) (eval y)
eval (Mul x y)     = C.mul (eval x) (eval y)
eval (Sub x y)     = C.sub (eval x) (eval y)
eval (Neg x)       = C.neg (eval x)
eval (Lit x)       = C.lit x
eval (Scale b o r) = C.scale b o (eval r)

-- prop_eval ∷ Exp → Bool
-- prop_eval e = e ≡ eval e


-------------------------------------------------------------------------------
-- Side
-------------------------------------------------------------------------------

data Side = L | R deriving Show


-------------------------------------------------------------------------------
-- Context of expressions
-------------------------------------------------------------------------------

data Ctx α = CtxEmpty
           | CtxNeg (Ctx α)
           | CtxAdd Side α (Ctx α)
           | CtxMul Side α (Ctx α)
           | CtxSub Side α (Ctx α)
           | CtxScale (Ctx α)
             deriving Show
