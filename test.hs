{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( forM_, when )
import Data.Char     ( String )
import Data.Function ( ($) )
import Data.List     ( map )
import Data.Maybe    ( Maybe(Just), maybe )
import Prelude       ( Integer )
import System.IO     ( IO )
import Text.Printf   ( printf )
import Text.Show     ( show )

-- from base-unicode-symbols:
import Data.Eq.Unicode ( (≢) )

-- from HUnit:
import Test.HUnit ( Assertion, assertFailure, (@?=) )

-- from test-framework:
import Test.Framework ( Test, defaultMain, testGroup )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )

-- from numerals:
import qualified Text.Numeral.Language.DE          as DE ( cardinal  )
import qualified Text.Numeral.Language.DE.TestData as DE ( cardinals )
import qualified Text.Numeral.Language.EO          as EO ( cardinal  )
import qualified Text.Numeral.Language.EO.TestData as EO ( cardinals )
import qualified Text.Numeral.Language.FR          as FR ( cardinal  )
import qualified Text.Numeral.Language.FR.TestData as FR ( cardinals )
import qualified Text.Numeral.Language.JA          as JA ( preferred_cardinal  )
import qualified Text.Numeral.Language.JA.TestData as JA ( preferred_cardinals )
import qualified Text.Numeral.Language.LA          as LA ( cardinal  )
import qualified Text.Numeral.Language.LA.TestData as LA ( cardinals )
import qualified Text.Numeral.Language.NL          as NL ( cardinal  )
import qualified Text.Numeral.Language.NL.TestData as NL ( cardinals )
import qualified Text.Numeral.Language.NO          as NO ( cardinal  )
import qualified Text.Numeral.Language.NO.TestData as NO ( cardinals )
import qualified Text.Numeral.Language.SV          as SV ( cardinal  )
import qualified Text.Numeral.Language.SV.TestData as SV ( cardinals )


--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------


main ∷ IO ()
main = defaultMain tests

testAsGroup ∷ (Integer → Maybe String) → [(Integer, String)] → Assertion
testAsGroup f xs = do
  forM_ xs $ \(n, s) →
    let r = f n
    in when (r ≢ Just s)
          $ assertFailure
          $ printf "Expected %i = %s but got %s"
                   n
                   (show s)
                   (maybe "no result" show r)

testIndividually ∷ (Integer → Maybe String) → [(Integer, String)] → [Test]
testIndividually f xs = map test xs
    where
      test (n, s) = testCase (show n) $ f n @?= Just s


mkTests ∷ String → (Integer → Maybe String) → [(Integer, String)] → Test
mkTests n f xs = testCase n $ testAsGroup f xs
--mkTests n f xs = testGroup n $ testIndividually f xs

tests ∷ [Test]
tests = [ testGroup "DE" [mkTests "cardinal" DE.cardinal DE.cardinals]
        , testGroup "EO" [mkTests "cardinal" EO.cardinal EO.cardinals]
        , testGroup "FR" [mkTests "cardinal" FR.cardinal FR.cardinals]
        , testGroup "JA"
          [ testGroup "preferred"
            [mkTests "cardinal" JA.preferred_cardinal JA.preferred_cardinals]
          ]
        , testGroup "LA" [mkTests "cardinal" LA.cardinal LA.cardinals]
        , testGroup "NL" [mkTests "cardinal" NL.cardinal NL.cardinals]
        , testGroup "NO" [mkTests "cardinal" NO.cardinal NO.cardinals]
        , testGroup "SV" [mkTests "cardinal" SV.cardinal SV.cardinals]
        ]
