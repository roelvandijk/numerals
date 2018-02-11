{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Text.Numeral.Debug where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( forM_ )
import Data.Function ( fix )
import Data.Maybe    ( fromMaybe )

-- from numerals-base:
import Text.Numeral
import Text.Numeral.Misc
import qualified Text.Numeral.Language.AMP as AMP
import qualified Text.Numeral.Language.CHN as CHN
import qualified Text.Numeral.Language.DEU  as DEU
import qualified Text.Numeral.Language.ENG  as ENG
import qualified Text.Numeral.Language.EPO  as EPO
import qualified Text.Numeral.Language.SPA  as SPA
import qualified Text.Numeral.Language.FRA  as FRA
import qualified Text.Numeral.Language.GLV  as GLV
import qualified Text.Numeral.Language.ITA  as ITA
import qualified Text.Numeral.Language.JPN  as JPN
import qualified Text.Numeral.Language.LAT  as LAT
import qualified Text.Numeral.Language.MLG  as MLG
import qualified Text.Numeral.Language.NLD  as NLD
import qualified Text.Numeral.Language.NOR  as NOR
import qualified Text.Numeral.Language.NQM as NQM
import qualified Text.Numeral.Language.OJI  as OJI
import qualified Text.Numeral.Language.PAA as PAA
import qualified Text.Numeral.Language.POR  as POR
import qualified Text.Numeral.Language.RUS  as RUS
import qualified Text.Numeral.Language.SWE  as SWE
import qualified Text.Numeral.Language.TUR  as TUR
import qualified Text.Numeral.Language.WOL  as WOL
import qualified Text.Numeral.Language.YOR as YOR
import qualified Text.Numeral.Language.ZHO  as ZHO

import qualified Text.Numeral.BigNum as BN

-- Stuff for the repr package
import qualified Text.Numeral.Exp.Classes as C
import qualified Text.Repr as R
import qualified Prelude.Repr as R
import Text.Show ( Show )

--------------------------------------------------------------------------------
-- Debug and test stuff
--------------------------------------------------------------------------------

test :: (Integer -> Maybe (R.Repr Integer)) -> (Integer -> Maybe String) -> [Integer] -> IO ()
test struct num xs =
    forM_ xs $ \x -> do putStr $ (show x) <> " = "
                       case struct x of
                         Nothing -> putStrLn "error"
                         Just e -> do putStr $ show e
                                     putStr " = "
                                     putStrLn $ fromMaybe "error" (num x)

-- | Like 'test' but doesn't print the numbers that are converted.
test2 :: (Integer -> Maybe (R.Repr Integer)) -> (Integer -> Maybe String) -> [Integer] -> IO ()
test2 struct num xs =
    forM_ xs $ \x -> case struct x of
                      Nothing -> putStrLn "error"
                      Just e -> do putStr $ show e
                                   putStr " = "
                                   putStrLn $ fromMaybe "error" (num x)

instance (C.Lit a, Show a) => C.Lit (R.Repr a) where lit = R.pure . C.lit
instance (C.Neg a) => C.Neg (R.Repr a) where neg = R.app C.neg "neg"
instance (C.Add a) => C.Add (R.Repr a) where add = R.infx R.L 6 C.add "+"
instance (C.Mul a) => C.Mul (R.Repr a) where mul = R.infx R.L 7 C.mul "×"
instance (C.Sub a) => C.Sub (R.Repr a) where sub = R.infx R.L 9 C.sub "`sub`"
instance (C.Scale a, Integral a) => C.Scale (R.Repr a) where
    scale b o r = R.repr (C.scale b o $ R.extract r) (R.renderer x)
        where
          x = 10 R.^ (r * fromInteger b + fromInteger o)
