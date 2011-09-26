{-# LANGUAGE OverloadedStrings, PackageImports, UnicodeSyntax #-}

{-|
This modules is used to debug the various languages.
-}
module Text.Numeral.Debug where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Monad ( forM_ )
import "base" Data.Maybe    ( fromMaybe )
import "base-unicode-symbols" Data.Monoid.Unicode   ( (⊕) )
import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import "numerals-base" Text.Numeral
import "numerals-base" Text.Numeral.Misc
import qualified "numerals-base" Text.Numeral.BigNum as BN
import qualified "this" Text.Numeral.Language.AF  as AF
import qualified "this" Text.Numeral.Language.AMP as AMP
import qualified "this" Text.Numeral.Language.CHN as CHN
import qualified "this" Text.Numeral.Language.CHR as CHR
import qualified "this" Text.Numeral.Language.CS  as CS
import qualified "this" Text.Numeral.Language.DE  as DE
import qualified "this" Text.Numeral.Language.EN  as EN
import qualified "this" Text.Numeral.Language.EO  as EO
import qualified "this" Text.Numeral.Language.ES  as ES
import qualified "this" Text.Numeral.Language.FR  as FR
import qualified "this" Text.Numeral.Language.GSW as GSW
import qualified "this" Text.Numeral.Language.GV  as GV
import qualified "this" Text.Numeral.Language.HE  as HE
import qualified "this" Text.Numeral.Language.IT  as IT
import qualified "this" Text.Numeral.Language.JA  as JA
import qualified "this" Text.Numeral.Language.LA  as LA
import qualified "this" Text.Numeral.Language.MG  as MG
import qualified "this" Text.Numeral.Language.NL  as NL
import qualified "this" Text.Numeral.Language.NO  as NO
import qualified "this" Text.Numeral.Language.NQM as NQM
import qualified "this" Text.Numeral.Language.OJ  as OJ
import qualified "this" Text.Numeral.Language.PAA as PAA
import qualified "this" Text.Numeral.Language.PL  as PL
import qualified "this" Text.Numeral.Language.PT  as PT
import qualified "this" Text.Numeral.Language.RU  as RU
import qualified "this" Text.Numeral.Language.SV  as SV
import qualified "this" Text.Numeral.Language.TR  as TR
import qualified "this" Text.Numeral.Language.WO  as WO
import qualified "this" Text.Numeral.Language.YOR as YOR
import qualified "this" Text.Numeral.Language.ZH  as ZH


--------------------------------------------------------------------------------
-- Debug and test stuff
--------------------------------------------------------------------------------

-- | Simple testing function. Quickly inspect the structure and
-- rendered result of some numbers.
--
-- Example:
-- >>> test NL.struct NL.cardinal [1..10]
test ∷ (ℤ → Exp) → (ℤ → Maybe String) → [ℤ] → IO ()
test struct num xs =
    forM_ xs $ \x → do putStr $ (show x)
                       putStr " = "
                       putStr $ show (struct x)
                       putStr " = "
                       putStrLn $ fromMaybe "<error>" (num x)
