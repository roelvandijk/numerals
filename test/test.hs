{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( when )
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
import qualified Text.Numeral.Language.AMP          as AMP ( cardinal  )
import qualified Text.Numeral.Language.AMP.TestData as AMP ( cardinals )
import qualified Text.Numeral.Language.CHN          as CHN ( cardinal  )
import qualified Text.Numeral.Language.CHN.TestData as CHN ( cardinals )
import qualified Text.Numeral.Language.CHR          as CHR ( cardinal  )
import qualified Text.Numeral.Language.CHR.TestData as CHR ( cardinals )
import qualified Text.Numeral.Language.DE           as DE  ( cardinal  )
import qualified Text.Numeral.Language.DE.TestData  as DE  ( cardinals )
import qualified Text.Numeral.Language.EN           as EN  ( uk_cardinal
                                                           , uk_ordinal
                                                           , us_cardinal
                                                           , us_ordinal
                                                           )
import qualified Text.Numeral.Language.EN.TestData  as EN  ( uk_cardinals
                                                           , uk_ordinals
                                                           , us_cardinals
                                                           , us_ordinals
                                                           )
import qualified Text.Numeral.Language.EO           as EO  ( cardinal  )
import qualified Text.Numeral.Language.EO.TestData  as EO  ( cardinals )
import qualified Text.Numeral.Language.ES           as ES  ( cardinal  )
import qualified Text.Numeral.Language.ES.TestData  as ES  ( cardinals )
import qualified Text.Numeral.Language.FR           as FR  ( cardinal  )
import qualified Text.Numeral.Language.FR.TestData  as FR  ( cardinals )
import qualified Text.Numeral.Language.GV           as GV  ( cardinal  )
import qualified Text.Numeral.Language.GV.TestData  as GV  ( cardinals )
import qualified Text.Numeral.Language.IT           as IT  ( cardinal  )
import qualified Text.Numeral.Language.IT.TestData  as IT  ( cardinals )
import qualified Text.Numeral.Language.JA           as JA
    ( preferred_cardinal
    , kanji_cardinal
    , daiji_cardinal
    )
import qualified Text.Numeral.Language.JA.TestData  as JA
    ( preferred_cardinals
    , kanji_cardinals
    , daiji_cardinals
    )
import qualified Text.Numeral.Language.LA           as LA  ( cardinal  )
import qualified Text.Numeral.Language.LA.TestData  as LA  ( cardinals )
import qualified Text.Numeral.Language.MG           as MG  ( cardinal  )
import qualified Text.Numeral.Language.MG.TestData  as MG  ( cardinals )
import qualified Text.Numeral.Language.NL           as NL  ( cardinal,  ordinal  )
import qualified Text.Numeral.Language.NL.TestData  as NL  ( cardinals, ordinals )
import qualified Text.Numeral.Language.NO           as NO  ( cardinal  )
import qualified Text.Numeral.Language.NO.TestData  as NO  ( cardinals )
import qualified Text.Numeral.Language.NQM          as NQM ( cardinal  )
import qualified Text.Numeral.Language.NQM.TestData as NQM ( cardinals )
import qualified Text.Numeral.Language.OJ           as OJ  ( cardinal  )
import qualified Text.Numeral.Language.OJ.TestData  as OJ  ( cardinals )
-- import qualified Text.Numeral.Language.PAA          as PAA ( cardinal  )
-- import qualified Text.Numeral.Language.PAA.TestData as PAA ( cardinals )
import qualified Text.Numeral.Language.PT           as PT  ( cardinal  )
import qualified Text.Numeral.Language.PT.TestData  as PT  ( cardinals )
import qualified Text.Numeral.Language.RU           as RU  ( cardinal  )
import qualified Text.Numeral.Language.RU.TestData  as RU  ( cardinals )
import qualified Text.Numeral.Language.SCO          as SCO ( cardinal  )
import qualified Text.Numeral.Language.SCO.TestData as SCO ( cardinals )
import qualified Text.Numeral.Language.SV           as SV  ( cardinal  )
import qualified Text.Numeral.Language.SV.TestData  as SV  ( cardinals )
import qualified Text.Numeral.Language.TR           as TR  ( cardinal  )
import qualified Text.Numeral.Language.TR.TestData  as TR  ( cardinals )
import qualified Text.Numeral.Language.WO           as WO  ( cardinal  )
import qualified Text.Numeral.Language.WO.TestData  as WO  ( cardinals )
import qualified Text.Numeral.Language.YOR          as YOR ( cardinal  )
import qualified Text.Numeral.Language.YOR.TestData as YOR ( cardinals )
import qualified Text.Numeral.Language.ZH           as ZH
    ( trad_cardinal
    , simpl_cardinal
    , finance_trad_cardinal
    , finance_simpl_cardinal
    , pinyin_cardinal
    )
import qualified Text.Numeral.Language.ZH.TestData  as ZH
    ( trad_cardinals
    , simpl_cardinals
    , finance_trad_cardinals
    , finance_simpl_cardinals
    , pinyin_cardinals
    )


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

mkTests ∷ String → (Integer → Maybe String) → [(Integer, String)] → Test
mkTests name f xs = testGroup name $ map mkTest xs
    where
      mkTest (n, s) = testCase (show n) $ testConversion f n s

tests ∷ [Test]
tests = [ testGroup "AMP" [mkTests "cardinal" AMP.cardinal AMP.cardinals]
        , testGroup "CHN" [mkTests "cardinal" CHN.cardinal CHN.cardinals]
        , testGroup "CHR" [mkTests "cardinal" CHR.cardinal CHR.cardinals]
        , testGroup "DE"  [mkTests "cardinal" DE.cardinal DE.cardinals]
        , testGroup "EN"
          [ testGroup "UK"
            [ mkTests "cardinal" EN.uk_cardinal EN.uk_cardinals
            , mkTests "ordinal"  EN.uk_ordinal  EN.uk_ordinals
            ]
          , testGroup "US"
            [ mkTests "cardinal" EN.us_cardinal EN.us_cardinals
            , mkTests "ordinal"  EN.us_ordinal  EN.us_ordinals
            ]
          ]
        , testGroup "EO"  [mkTests "cardinal" EO.cardinal EO.cardinals]
        , testGroup "ES"  [mkTests "cardinal" ES.cardinal  ES.cardinals]
        , testGroup "FR"  [mkTests "cardinal" FR.cardinal FR.cardinals]
        , testGroup "GV"  [mkTests "cardinal" GV.cardinal GV.cardinals]
        , testGroup "IT"  [mkTests "cardinal" IT.cardinal IT.cardinals]
        , testGroup "JA"
          [ testGroup "preferred"
            [mkTests "cardinal" JA.preferred_cardinal
                                JA.preferred_cardinals
            ]
          , testGroup "kanji"
            [mkTests "cardinal" JA.kanji_cardinal JA.kanji_cardinals]
          , testGroup "daiji"
            [mkTests "cardinal" JA.daiji_cardinal JA.daiji_cardinals]
          ]
        , testGroup "LA"  [mkTests "cardinal" LA.cardinal  LA.cardinals]
        , testGroup "MG"  [mkTests "cardinal" MG.cardinal  MG.cardinals]
        , testGroup "NL"  [ mkTests "cardinal" NL.cardinal  NL.cardinals
                          , mkTests "ordinal"  NL.ordinal   NL.ordinals
                          ]
        , testGroup "NO"  [mkTests "cardinal" NO.cardinal  NO.cardinals]
        , testGroup "NQM" [mkTests "cardinal" NQM.cardinal NQM.cardinals]
        , testGroup "OJ"  [mkTests "cardinal" OJ.cardinal  OJ.cardinals]
        -- , testGroup "PAA" [mkTests "cardinal" PAA.cardinal PAA.cardinals]
        , testGroup "PT"  [mkTests "cardinal" PT.cardinal  PT.cardinals]
        , testGroup "RU"  [mkTests "cardinal" RU.cardinal  RU.cardinals]
        , testGroup "SCO" [mkTests "cardinal" SCO.cardinal SCO.cardinals]
        , testGroup "SV"  [mkTests "cardinal" SV.cardinal  SV.cardinals]
        , testGroup "TR"  [mkTests "cardinal" TR.cardinal  TR.cardinals]
        , testGroup "WO"  [mkTests "cardinal" WO.cardinal  WO.cardinals]
        , testGroup "YOR" [mkTests "cardinal" YOR.cardinal YOR.cardinals]
        , testGroup "ZH"
          [ testGroup "characters (traditional)"
            [mkTests "cardinal" ZH.trad_cardinal
                                ZH.trad_cardinals
            ]
          , testGroup "characters (simplified)"
            [mkTests "cardinal" ZH.simpl_cardinal
                                ZH.simpl_cardinals
            ]
          , testGroup "financial characters (traditional)"
            [mkTests "cardinal" ZH.finance_trad_cardinal
                                ZH.finance_trad_cardinals
            ]
          , testGroup "financial characters (simplified)"
            [mkTests "cardinal" ZH.finance_simpl_cardinal
                                ZH.finance_simpl_cardinals
            ]
          , testGroup "pinyin"
            [mkTests "cardinal" ZH.pinyin_cardinal
                                ZH.pinyin_cardinals
            ]
          ]
        ]
