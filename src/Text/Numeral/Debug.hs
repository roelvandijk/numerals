{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
This modules is used to debug the various languages.
-}
module Text.Numeral.Debug where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( forM_ )
import Data.Maybe    ( fromMaybe )

-- from base-unicode-symbols:
import Data.Monoid.Unicode   ( (⊕) )

-- from numerals-base:
import Text.Numeral
import Text.Numeral.Misc
import qualified Text.Numeral.BigNum as BN

-- from numerals:
import qualified Text.Numeral.Language.AMP as AMP
import qualified Text.Numeral.Language.CHN as CHN
import qualified Text.Numeral.Language.CHR as CHR
import qualified Text.Numeral.Language.DE  as DE
import qualified Text.Numeral.Language.EN  as EN
import qualified Text.Numeral.Language.EO  as EO
import qualified Text.Numeral.Language.ES  as ES
import qualified Text.Numeral.Language.FR  as FR
import qualified Text.Numeral.Language.GV  as GV
import qualified Text.Numeral.Language.HE  as HE
import qualified Text.Numeral.Language.IT  as IT
import qualified Text.Numeral.Language.JA  as JA
import qualified Text.Numeral.Language.LA  as LA
import qualified Text.Numeral.Language.MG  as MG
import qualified Text.Numeral.Language.NL  as NL
import qualified Text.Numeral.Language.NO  as NO
import qualified Text.Numeral.Language.NQM as NQM
import qualified Text.Numeral.Language.OJ  as OJ
import qualified Text.Numeral.Language.PAA as PAA
import qualified Text.Numeral.Language.PT  as PT
import qualified Text.Numeral.Language.RU  as RU
import qualified Text.Numeral.Language.SV  as SV
import qualified Text.Numeral.Language.TR  as TR
import qualified Text.Numeral.Language.WO  as WO
import qualified Text.Numeral.Language.YOR as YOR
import qualified Text.Numeral.Language.ZH  as ZH


--------------------------------------------------------------------------------
-- Debug and test stuff
--------------------------------------------------------------------------------

-- | Simple testing function. Quickly inspect the structure and
-- rendered result of some numbers.
--
-- Example:
-- >>> test NL.struct NL.cardinal [1..10]
test ∷ (Integer → Exp) → (Integer → Maybe String) → [Integer] → IO ()
test struct num xs =
    forM_ xs $ \x → do putStr $ (show x)
                       putStr " = "
                       putStr $ show (struct x)
                       putStr " = "
                       putStrLn $ fromMaybe "<error>" (num x)
