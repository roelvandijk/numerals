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
import Data.Maybe    ( Maybe(Just), fromMaybe )
import Prelude       ( Integer )
import System.IO     ( IO )
import Text.Printf   ( printf )
import Text.Show     ( show )

-- from base-unicode-symbols:
import Data.Eq.Unicode ( (≢) )

-- from HUnit:
import Test.HUnit ( Assertion, assertFailure )

-- from test-framework:
import Test.Framework ( Test, defaultMain, testGroup )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )

-- from numerals:
import qualified Text.Numeral.Language.CHN          as CHN ( cardinal  )
import qualified Text.Numeral.Language.CHN.TestData as CHN ( cardinals )
import qualified Text.Numeral.Language.DE           as DE  ( cardinal  )
import qualified Text.Numeral.Language.DE.TestData  as DE  ( cardinals )
import qualified Text.Numeral.Language.EN           as EN  ( uk_cardinal
                                                           , us_cardinal
                                                           )
import qualified Text.Numeral.Language.EN.TestData  as EN  ( uk_cardinals
                                                           , us_cardinals
                                                           )
import qualified Text.Numeral.Language.EO           as EO  ( cardinal  )
import qualified Text.Numeral.Language.EO.TestData  as EO  ( cardinals )
import qualified Text.Numeral.Language.FR           as FR  ( cardinal  )
import qualified Text.Numeral.Language.FR.TestData  as FR  ( cardinals )
import qualified Text.Numeral.Language.JA           as JA  ( preferred_cardinal
                                                           , kanji_cardinal
                                                           , daiji_cardinal
                                                           )
import qualified Text.Numeral.Language.JA.TestData  as JA  ( preferred_cardinals
                                                           , kanji_cardinals
                                                           , daiji_cardinals
                                                           )
import qualified Text.Numeral.Language.LA           as LA  ( cardinal  )
import qualified Text.Numeral.Language.LA.TestData  as LA  ( cardinals )
import qualified Text.Numeral.Language.NL           as NL  ( cardinal  )
import qualified Text.Numeral.Language.NL.TestData  as NL  ( cardinals )
import qualified Text.Numeral.Language.NO           as NO  ( cardinal  )
import qualified Text.Numeral.Language.NO.TestData  as NO  ( cardinals )
import qualified Text.Numeral.Language.NQM          as NQM ( cardinal  )
import qualified Text.Numeral.Language.NQM.TestData as NQM ( cardinals )
import qualified Text.Numeral.Language.SV           as SV  ( cardinal  )
import qualified Text.Numeral.Language.SV.TestData  as SV  ( cardinals )
import qualified Text.Numeral.Language.YOR          as YOR ( cardinal  )
import qualified Text.Numeral.Language.YOR.TestData as YOR ( cardinals )



--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

testConversion ∷ (Integer → Maybe String) → Integer → String → Assertion
testConversion f n s =
  let r = f n
  in when (r ≢ Just s)
        $ assertFailure
        $ printf "Expected %i = \"%s\" but got \"%s\""
                 n
                 s
                 (fromMaybe "no result" r)

testAsGroup ∷ (Integer → Maybe String) → [(Integer, String)] → Assertion
testAsGroup f xs = forM_ xs $ \(n, s) → testConversion f n s

testIndividually ∷ (Integer → Maybe String) → [(Integer, String)] → [Test]
testIndividually f xs = let mkTest (n, s) = testCase (show n)
                                          $ testConversion f n s
                        in map mkTest xs

mkTests ∷ String → (Integer → Maybe String) → [(Integer, String)] → Test
mkTests n f xs = testCase n $ testAsGroup f xs
--mkTests n f xs = testGroup n $ testIndividually f xs

tests ∷ [Test]
tests = [ testGroup "CHN" [mkTests "cardinal" CHN.cardinal CHN.cardinals]
        , testGroup "DE" [mkTests "cardinal" DE.cardinal DE.cardinals]
        , testGroup "EN"
          [ testGroup "UK"
            [mkTests "cardinal" EN.uk_cardinal EN.uk_cardinals]
          , testGroup "US"
            [mkTests "cardinal" EN.us_cardinal EN.us_cardinals]
          ]
        , testGroup "EO" [mkTests "cardinal" EO.cardinal EO.cardinals]
        , testGroup "FR" [mkTests "cardinal" FR.cardinal FR.cardinals]
        , testGroup "JA"
          [ testGroup "preferred"
            [mkTests "cardinal" JA.preferred_cardinal JA.preferred_cardinals]
          , testGroup "kanji"
            [mkTests "cardinal" JA.kanji_cardinal JA.kanji_cardinals]
          , testGroup "daiji"
            [mkTests "cardinal" JA.daiji_cardinal JA.daiji_cardinals]
          ]
        , testGroup "LA"  [mkTests "cardinal" LA.cardinal  LA.cardinals]
        , testGroup "NL"  [mkTests "cardinal" NL.cardinal  NL.cardinals]
        , testGroup "NO"  [mkTests "cardinal" NO.cardinal  NO.cardinals]
        , testGroup "NQM" [mkTests "cardinal" NQM.cardinal NQM.cardinals]
        , testGroup "SV"  [mkTests "cardinal" SV.cardinal  SV.cardinals]
        , testGroup "YOR" [mkTests "cardinal" YOR.cardinal YOR.cardinals]
        ]
