{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Text.Numeral.Positional
    ( toPositional
    , digitSymbol

      -- *Positional numeral systems
    , binary
    , negabinary
    , ternary
    , octal
    , decimal
    , negadecimal
    , hexadecimal
    ) where

import qualified Data.List as L
import Data.Monoid
import Data.String

-------------------------------------------------------------------------------


toPolynomial :: Integer -> Integer -> [Integer]
toPolynomial b n | n == 0    = [0]
                 | b == 0    = error "toPolynomial: base 0"
                 | b == (-1) = case n of
                                 (-1) -> [0, 1]
                                 1    -> [1]
                                 _    -> error "toPolynomial: base (-1)"
                 | b == 1    = L.genericReplicate (abs n) (signum n)
                 | otherwise = toPolynomial_b $ n
      where toPolynomial_b 0 = []
            toPolynomial_b n = let (q, r) = n `qr` b
                         in r : toPolynomial_b q

            qr | b > 0     = quotRem
               | otherwise = quotRem'

            quotRem' :: Integral a => a -> a -> (a, a)
            quotRem' n d = let qr@(q, r) = n `quotRem` d
                           in if r < 0
                              then (q + 1, r - d)
                              else qr

fromPolynomial :: Integer -> [Integer] -> Integer
fromPolynomial b = sum' . zipWith (*) (iterate (* b) 1)
    where sum' = L.foldl' (+) 0

prop_polynomial :: Integer -> Integer -> Bool
prop_polynomial b n | b == 0    && n /= 0    = True
                    | b == (-1) && abs n > 1 = True
                    | otherwise              = n == (fromPolynomial b $ toPolynomial b n)

-------------------------------------------------------------------------------

toPositional :: (IsString s, Monoid s) => (Integer -> s) -> Integer -> Integer -> Maybe s
toPositional f b n | b == 0         = Nothing
                   | n < 0 && b > 0 = fmap (mappend "-") $ repr (abs n)
                   | otherwise      = repr n
    where repr x = fmap mconcat . mapM f' . reverse . toPolynomial b $ x
          f' n | n >= abs b = Nothing
               | otherwise  = Just $ f n

-------------------------------------------------------------------------------

-- Digit symbols up to base 62.
-- TODO: array for faster lookup
digitSymbols :: IsString s => [s]
digitSymbols = map (\x -> fromString [x]) $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

digitSymbol :: IsString s => Integer -> s
digitSymbol = (digitSymbols `L.genericIndex`)

-------------------------------------------------------------------------------

binary :: (Monoid s, IsString s) => Integer -> Maybe s
binary = toPositional digitSymbol 2

negabinary :: (Monoid s, IsString s) => Integer -> Maybe s
negabinary = toPositional digitSymbol (-2)

ternary :: (Monoid s, IsString s) => Integer -> Maybe s
ternary = toPositional digitSymbol 3

octal :: (Monoid s, IsString s) => Integer -> Maybe s
octal = toPositional digitSymbol 8

decimal :: (Monoid s, IsString s) => Integer -> Maybe s
decimal = toPositional digitSymbol 10

negadecimal :: (Monoid s, IsString s) => Integer -> Maybe s
negadecimal = toPositional digitSymbol (-10)

hexadecimal ::(Monoid s, IsString s) => Integer -> Maybe s
hexadecimal = toPositional digitSymbol 16
