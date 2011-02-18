{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Debug where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( forM_ )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Monoid.Unicode   ( (⊕) )

-- from numerals:
import Text.Numeral
import qualified Text.Numeral.Language.NL as NL
import qualified Text.Numeral.Language.DE as DE
import qualified Text.Numeral.Language.FR as FR
import qualified Text.Numeral.Language.EN as EN
import qualified Text.Numeral.Language.EO as EO
import qualified Text.Numeral.Language.JA as JA
import qualified Text.Numeral.Language.SV as SV
import qualified Text.Numeral.Language.NO as NO


--------------------------------------------------------------------------------
-- Debug and test stuff
--------------------------------------------------------------------------------

test ∷ (Integer → Maybe String) → [Integer] → IO ()
test cardinal xs = forM_ xs $ \x → do putStr $ (show x) ⊕ " - "
                                      maybe (putStrLn "error")
                                            putStrLn
                                            $ cardinal x
