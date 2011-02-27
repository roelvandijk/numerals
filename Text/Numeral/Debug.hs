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
import qualified Text.Numeral.Language.FR  as FR
import qualified Text.Numeral.Language.JA  as JA
import qualified Text.Numeral.Language.LA  as LA
import qualified Text.Numeral.Language.NL  as NL
import qualified Text.Numeral.Language.NO  as NO
import qualified Text.Numeral.Language.NQM as NQM
import qualified Text.Numeral.Language.SV  as SV

import qualified Text.Numeral.Language.BigNum as BN


--------------------------------------------------------------------------------
-- Debug and test stuff
--------------------------------------------------------------------------------


test ∷ Rule Integer Exp → Repr String → [Integer] → IO ()
test rule repr xs = forM_ xs $ \x → do putStr $ (show x) ⊕ " - "
                                       case (fix rule) x of
                                         Nothing → putStrLn "error"
                                         Just e → do putStr $ show e
                                                     putStr " - "
                                                     putStrLn $ fromMaybe "error" (textify repr e)

test2 ∷ Rule Integer Exp → Repr String → [Integer] → IO ()
test2 rule repr xs = forM_ xs $ \x → case (fix rule) x of
                                       Nothing → putStrLn "error"
                                       Just e → do putStr $ show e
                                                   putStr " - "
                                                   putStrLn $ fromMaybe "error" (textify repr e)
