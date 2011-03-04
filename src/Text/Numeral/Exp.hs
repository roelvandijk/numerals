{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Text.Numeral.Exp
    ( Exp(..)
    , Subtract(subtract)
    , Scale(scale)
    , eval
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Eq   ( Eq )
import Data.Ord  ( Ord )
import Prelude   ( Num, Integer, fromInteger
                 , (+), (*), (-), (^)
                 , signum, abs, negate, error
                 )
import Text.Show ( Show )
import qualified Prelude as P ( subtract )

-- from base-unicode-symbols:
import Prelude.Unicode       ( (⋅) )


-------------------------------------------------------------------------------
-- Exp
-------------------------------------------------------------------------------

data Exp = Exp :+: Exp
         | Exp :*: Exp
         | Sub Exp Exp
         | Neg Exp
         | C Integer
         | Scale Integer Integer Exp
           deriving (Eq, Ord, Show)

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

class    Subtract α       where subtract ∷ α → α → α
instance Subtract Integer where subtract = P.subtract
instance Subtract Exp     where subtract = Sub

class    Scale α       where scale ∷ Integer → Integer → α → α
instance Scale Exp     where scale = Scale
instance Scale Integer where scale b o r = 10 ^ (r⋅b + o)


eval ∷ (Num α, Subtract α, Scale α) ⇒ Exp → α
eval (x :+: y)     = eval x + eval y
eval (x :*: y)     = eval x ⋅ eval y
eval (Sub x y)     = subtract (eval x) (eval y)
eval (Neg x)       = negate (eval x)
eval (C x)         = fromInteger x
eval (Scale b o r) = scale b o (eval r)

-- prop_eval ∷ Exp → Bool
-- prop_eval e = e ≡ eval e
