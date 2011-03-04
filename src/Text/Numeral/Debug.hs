{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Debug where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( forM_ )
import Data.Function ( fix )
import Data.Maybe    ( fromMaybe )

-- from base-unicode-symbols:
import Data.Monoid.Unicode   ( (⊕) )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc
import qualified Text.Numeral.Language.CHN as CHN
import qualified Text.Numeral.Language.DE  as DE
import qualified Text.Numeral.Language.EN  as EN
import qualified Text.Numeral.Language.EO  as EO
import qualified Text.Numeral.Language.ES  as ES
import qualified Text.Numeral.Language.FR  as FR
import qualified Text.Numeral.Language.JA  as JA
import qualified Text.Numeral.Language.LA  as LA
import qualified Text.Numeral.Language.NL  as NL
import qualified Text.Numeral.Language.NO  as NO
import qualified Text.Numeral.Language.NQM as NQM
import qualified Text.Numeral.Language.PAA as PAA
import qualified Text.Numeral.Language.SV  as SV
import qualified Text.Numeral.Language.YOR as YOR

import qualified Text.Numeral.Language.BigNum as BN


--------------------------------------------------------------------------------
-- Debug and test stuff
--------------------------------------------------------------------------------

test ∷ (Integer → Maybe Exp) → (Exp → Maybe String) → [Integer] → IO ()
test struct repr xs =
    forM_ xs $ \x → do putStr $ (show x) ⊕ " - "
                       case struct x of
                         Nothing → putStrLn "error"
                         Just e → do putStr $ show e
                                     putStr " - "
                                     putStrLn $ fromMaybe "error" (repr e)

-- | Like 'test' but doesn't print the numbers that are converted.
test2 ∷ (Integer → Maybe Exp) → (Exp → Maybe String) → [Integer] → IO ()
test2 struct repr xs =
    forM_ xs $ \x → case struct x of
                      Nothing → putStrLn "error"
                      Just e → do putStr $ show e
                                  putStr " - "
                                  putStrLn $ fromMaybe "error" (repr e)
