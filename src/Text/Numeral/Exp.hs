module Text.Numeral.Exp
    ( Exp(..)
    , showExp
    -- , eval
    , evalScale
    , Side(L, R)
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import qualified "this" Text.Numeral.Grammar as G

-------------------------------------------------------------------------------
-- Expression type
-------------------------------------------------------------------------------

-- | An expression that represents the structure of a numeral.
data Exp
      -- | An unknown value. This is used to signal that a value can
      -- not be represented in the expression language.
    = Unknown
      -- | A literal value.
      --
      -- Example in English:
      --
      -- > "three" = Lit 3
    | Lit Integer
      -- | Negation of an expression.
      --
      -- Example in English:
      --
      -- > "minus two" = Neg (Lit 2)
    | Neg Exp
      -- | Addition of two expressions.
      --
      -- Example in English:
      --
      -- > "fifteen" = Lit 5 `Add` Lit 10
    | Add Exp Exp
      -- | Multiplication of two expressions.
      --
      -- Example in English:
      --
      -- > "thirty" = Lit 3 `Mul` Lit 10
    | Mul Exp Exp
      -- | One expression subtracted from another expression.
      --
      -- Example in Latin:
      --
      -- > "duodēvīgintī" = Lit 2 `Sub` (Lit 2 `Mul` Lit 10)
    | Sub Exp Exp
      -- | A fraction.
      --
      -- Example in English:
      --
      -- > "two thirds" = `Frac` (Lit 2) (Lit 3)
    | Frac Exp Exp
      -- | A step in a scale of large values.
      --
      -- Should be interpreted as @10 ^ (rank * base + offset)@.
      --
      -- Example in English:
      --
      -- > "quadrillion" = Scale 3 3 4
    | Scale Integer Integer Exp
    | ChangeCase   (Maybe G.Case  ) Exp
    -- | A change of grammatical gender.
    --
    -- This is used in a language like Spanish where the inflection of
    -- a number word is not always constant. Specifically, in Spanish,
    -- large number names always have the masculine gender. So
    -- 'millón', 'billón' and the like are all masculine. This can
    -- result in the following number word: 10000001 = "un (masculine)
    -- millón una (feminine)"
    --
    -- Example in Spanish (with the context being Feminine):
    --
    -- > "un millón una" = ChangeGender (Just Masculine) (Lit 1) `Mul` Scale 3 3 1 `Add` Lit 1
    | ChangeGender (Maybe G.Gender) Exp
    | ChangeNumber (Maybe G.Number) Exp

infixl 6 `Add`
infixl 6 `Sub`
infixl 7 `Mul`

showExp :: Exp -> String
showExp Unknown = "Unknown"
showExp (Lit n) = "Lit " ++ show n
showExp (Neg x) = "Neg (" ++ showExp x ++ ")"
showExp (Add  x y) = "Add ("  ++ showExp x ++ ") (" ++ showExp y ++ ")"
showExp (Mul  x y) = "Mul ("  ++ showExp x ++ ") (" ++ showExp y ++ ")"
showExp (Sub  x y) = "Sub ("  ++ showExp x ++ ") (" ++ showExp y ++ ")"
showExp (Frac x y) = "Frac (" ++ showExp x ++ ") (" ++ showExp y ++ ")"
showExp (Scale b o r) = "Scale " ++ show b ++ " " ++ show o ++ " (" ++ showExp r ++ ")"
showExp (ChangeCase   mbC x) = "Case is " ++ show mbC ++ " in (" ++ showExp x ++ ")"
showExp (ChangeGender mbG x) = "Gender is " ++ show mbG ++ " in (" ++ showExp x ++ ")"
showExp (ChangeNumber mbN x) = "Number is " ++ show mbN ++ " in (" ++ showExp x ++ ")"

evalScale :: (Integral a) => a -> a -> a -> a
evalScale b o r = 10 ^ (r*b + o)

-- eval :: (Fractional a) => Exp -> Maybe a
-- eval (Lit x)            = pure x
-- eval (Add x y)          = (+) <$> eval x <*> eval y
-- eval (Mul x y)          = (*) <$> eval x <*> eval y
-- eval (Sub x y)          = subtract <$> eval x <*> eval y
-- eval (Neg x)            = negate <$> eval x
-- eval (Frac n d)         = (/) <$> eval n <*> eval d
-- eval (Scale b o r)      = evalScale (fromInteger b) (fromInteger o) <$> eval r
-- eval (ChangeCase   _ x) = eval x
-- eval (ChangeGender _ x) = eval x
-- eval (ChangeNumber _ x) = eval x
-- eval Unknown            = Nothing

-------------------------------------------------------------------------------
-- Side
-------------------------------------------------------------------------------

-- | A side or direction, either 'L'eft or 'R'ight.
data Side = L -- ^ Left.
          | R -- ^ Right.
            deriving (Eq, Show)
