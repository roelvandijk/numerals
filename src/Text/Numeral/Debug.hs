{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Numeral.Debug where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( forM_ )
import Data.Function ( fix )
import Data.Maybe    ( fromMaybe )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )
import Prelude.Unicode     ( ℤ )

-- from numerals-base:
import Text.Numeral
import Text.Numeral.Misc
import qualified Text.Numeral.Language.AMP as AMP
import qualified Text.Numeral.Language.CHN as CHN
import qualified Text.Numeral.Language.DE  as DE
import qualified Text.Numeral.Language.EN  as EN
import qualified Text.Numeral.Language.EO  as EO
import qualified Text.Numeral.Language.ES  as ES
import qualified Text.Numeral.Language.FR  as FR
import qualified Text.Numeral.Language.GV  as GV
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

import qualified Text.Numeral.BigNum as BN

-- Stuff for the repr package
import qualified Text.Numeral.Exp.Classes as C
import qualified Text.Repr as R
import qualified Prelude.Repr as R
import Prelude.Unicode ( (⋅), (∘) )
import Text.Show ( Show )

--------------------------------------------------------------------------------
-- Debug and test stuff
--------------------------------------------------------------------------------

test ∷ (ℤ → Maybe (R.Repr ℤ)) → (ℤ → Maybe String) → [ℤ] → IO ()
test struct num xs =
    forM_ xs $ \x → do putStr $ (show x) ⊕ " = "
                       case struct x of
                         Nothing → putStrLn "error"
                         Just e → do putStr $ show e
                                     putStr " = "
                                     putStrLn $ fromMaybe "error" (num x)

-- | Like 'test' but doesn't print the numbers that are converted.
test2 ∷ (ℤ → Maybe (R.Repr ℤ)) → (ℤ → Maybe String) → [ℤ] → IO ()
test2 struct num xs =
    forM_ xs $ \x → case struct x of
                      Nothing → putStrLn "error"
                      Just e → do putStr $ show e
                                  putStr " = "
                                  putStrLn $ fromMaybe "error" (num x)

instance (C.Lit α, Show α) ⇒ C.Lit (R.Repr α) where lit = R.pure ∘ C.lit
instance (C.Neg α) ⇒ C.Neg (R.Repr α) where neg = R.app C.neg "neg"
instance (C.Add α) ⇒ C.Add (R.Repr α) where add = R.infx R.L 6 C.add "+"
instance (C.Mul α) ⇒ C.Mul (R.Repr α) where mul = R.infx R.L 7 C.mul "×"
instance (C.Sub α) ⇒ C.Sub (R.Repr α) where sub = R.infx R.L 9 C.sub "`sub`"
instance (C.Scale α, Integral α) ⇒ C.Scale (R.Repr α) where
    scale b o r = R.repr (C.scale b o $ R.extract r) (R.renderer x)
        where
          x = 10 R.^ (r ⋅ fromInteger b + fromInteger o)
