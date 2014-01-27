{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Text.Numeral.Exp.Reified ( Exp(..), showExp, Side(L, R) ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool ( Bool(False, True) )
import "base" Data.Eq   ( Eq )
import "base" Data.List ( (++) )
import "base" Prelude   ( String )
import "base" Text.Show ( Show, show )
import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import qualified "this" Text.Numeral.Exp as E


-------------------------------------------------------------------------------
-- Reified expression type
-------------------------------------------------------------------------------

-- | An expression that represents the structure of a numeral.
data Exp i
          -- | An unknown value.
         = Unknown
          -- | A literal value.
         | Lit ℤ
           -- | Negation of an expression.
         | Neg (Exp i)
           -- | Addition of two expressions.
         | Add (Exp i) (Exp i)
           -- | Multiplication of two expressions.
         | Mul (Exp i) (Exp i)
           -- | One expression subtracted from another expression.
         | Sub (Exp i) (Exp i)
           -- | A fraction.
         | Frac (Exp i) (Exp i)
           -- | A step in a scale of large values.
         | Scale ℤ ℤ (Exp i)
           -- | A change of inflection.
         | Inflection (i → i) (Exp i)

infixl 6 `Add`
infixl 6 `Sub`
infixl 7 `Mul`

showExp ∷ Exp i → String
showExp Unknown = "Unknown"
showExp (Lit n) = "Lit " ++ show n
showExp (Neg x) = "Neg (" ++ showExp x ++ ")"
showExp (Add  x y) = "Add ("  ++ showExp x ++ ") (" ++ showExp y ++ ")"
showExp (Mul  x y) = "Mul ("  ++ showExp x ++ ") (" ++ showExp y ++ ")"
showExp (Sub  x y) = "Sub ("  ++ showExp x ++ ") (" ++ showExp y ++ ")"
showExp (Frac x y) = "Frac (" ++ showExp x ++ ") (" ++ showExp y ++ ")"
showExp (Scale b o r) = "Scale " ++ show b ++ " " ++ show o ++ " (" ++ showExp r ++ ")"
showExp (Inflection _ x) = "Inflection <func> (" ++ showExp x ++ ")"


-- | Precisely the 'Unknown' constructor.
instance E.Unknown (Exp i) where
    unknown = Unknown
    isUnknown Unknown = True
    isUnknown _       = False
-- | Precisely the 'Lit' constructor.
instance E.Lit (Exp i) where lit = Lit
-- | Precisely the 'Neg' constructor.
instance E.Neg (Exp i) where neg = Neg
-- | Precisely the 'Add' constructor.
instance E.Add (Exp i) where add = Add
-- | Precisely the 'Mul' constructor.
instance E.Mul (Exp i) where mul = Mul
-- | Precisely the 'Sub' constructor.
instance E.Sub (Exp i) where sub = Sub
-- | Precisely the 'Frac' constructor.
instance E.Frac (Exp i) where frac = Frac
-- | Precisely the 'Scale' constructor.
instance E.Scale (Exp i) where scale = Scale
-- | Precisely the 'Inflection' constructor.
instance E.Inflection (Exp i) where
#if __GLASGOW_HASKELL__ < 704
    type E.Inf (Exp i) = i
#else
    type Inf (Exp i) = i
#endif
    inflection = Inflection


-------------------------------------------------------------------------------
-- Side
-------------------------------------------------------------------------------

-- | A side or direction, either 'L'eft or 'R'ight.
data Side = L -- ^ Left.
          | R -- ^ Right.
            deriving (Eq, Show)
