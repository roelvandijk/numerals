{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Text.Numeral.Exp
    ( Exp(..)
    , Subtract(subtract)
    , Scale(scale)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Eq   ( Eq )
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
         | Scale Integer Integer Integer
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

class    Subtract α       where subtract ∷ α → α → α
instance Subtract Integer where subtract = P.subtract
instance Subtract Exp     where subtract = Sub

class    Scale α       where scale ∷ Integer → Integer → Integer → α
instance Scale Exp     where scale = Scale
instance Scale Integer where scale b o n = 10 ^ (n⋅b + o)

