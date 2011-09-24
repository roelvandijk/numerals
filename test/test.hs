{-# LANGUAGE NoImplicitPrelude, PackageImports, UnicodeSyntax #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Monad ( when )
import "base" Data.Function ( ($) )
import "base" Data.List     ( map )
import "base" Data.Maybe    ( Maybe(Just), fromMaybe )
import "base" Prelude       ( String )
import "base" System.IO     ( IO )
import "base" Text.Printf   ( printf )
import "base" Text.Show     ( show )
import "base-unicode-symbols" Data.Eq.Unicode ( (≢) )
import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import "HUnit" Test.HUnit ( Assertion, assertFailure )
import "test-framework" Test.Framework ( Test, defaultMain, testGroup )
import "test-framework-hunit" Test.Framework.Providers.HUnit ( testCase )
import qualified "this" Text.Numeral.Language.AF           as AF
import qualified "this" Text.Numeral.Language.AF.TestData  as AF
import qualified "this" Text.Numeral.Language.AMP          as AMP
import qualified "this" Text.Numeral.Language.AMP.TestData as AMP
import qualified "this" Text.Numeral.Language.CHN          as CHN
import qualified "this" Text.Numeral.Language.CHN.TestData as CHN
import qualified "this" Text.Numeral.Language.CHR          as CHR
import qualified "this" Text.Numeral.Language.CHR.TestData as CHR
import qualified "this" Text.Numeral.Language.DE           as DE
import qualified "this" Text.Numeral.Language.DE.TestData  as DE
import qualified "this" Text.Numeral.Language.EN           as EN
import qualified "this" Text.Numeral.Language.EN.TestData  as EN
import qualified "this" Text.Numeral.Language.EO           as EO
import qualified "this" Text.Numeral.Language.EO.TestData  as EO
import qualified "this" Text.Numeral.Language.ES           as ES
import qualified "this" Text.Numeral.Language.ES.TestData  as ES
import qualified "this" Text.Numeral.Language.FR           as FR
import qualified "this" Text.Numeral.Language.FR.TestData  as FR
import qualified "this" Text.Numeral.Language.GSW          as GSW
import qualified "this" Text.Numeral.Language.GSW.TestData as GSW
import qualified "this" Text.Numeral.Language.GV           as GV
import qualified "this" Text.Numeral.Language.GV.TestData  as GV
import qualified "this" Text.Numeral.Language.HE           as HE
import qualified "this" Text.Numeral.Language.HE.TestData  as HE
import qualified "this" Text.Numeral.Language.IT           as IT
import qualified "this" Text.Numeral.Language.IT.TestData  as IT
import qualified "this" Text.Numeral.Language.JA           as JA
import qualified "this" Text.Numeral.Language.JA.TestData  as JA
import qualified "this" Text.Numeral.Language.LA           as LA
import qualified "this" Text.Numeral.Language.LA.TestData  as LA
import qualified "this" Text.Numeral.Language.MG           as MG
import qualified "this" Text.Numeral.Language.MG.TestData  as MG
import qualified "this" Text.Numeral.Language.NL           as NL
import qualified "this" Text.Numeral.Language.NL.TestData  as NL
import qualified "this" Text.Numeral.Language.NO           as NO
import qualified "this" Text.Numeral.Language.NO.TestData  as NO
import qualified "this" Text.Numeral.Language.NQM          as NQM
import qualified "this" Text.Numeral.Language.NQM.TestData as NQM
import qualified "this" Text.Numeral.Language.OJ           as OJ
import qualified "this" Text.Numeral.Language.OJ.TestData  as OJ
-- import qualified "this" Text.Numeral.Language.PAA          as PAA
-- import qualified "this" Text.Numeral.Language.PAA.TestData as PAA
import qualified "this" Text.Numeral.Language.PL           as PL
import qualified "this" Text.Numeral.Language.PL.TestData  as PL
import qualified "this" Text.Numeral.Language.PT           as PT
import qualified "this" Text.Numeral.Language.PT.TestData  as PT
import qualified "this" Text.Numeral.Language.RU           as RU
import qualified "this" Text.Numeral.Language.RU.TestData  as RU
import qualified "this" Text.Numeral.Language.SCO          as SCO
import qualified "this" Text.Numeral.Language.SCO.TestData as SCO
import qualified "this" Text.Numeral.Language.SV           as SV
import qualified "this" Text.Numeral.Language.SV.TestData  as SV
import qualified "this" Text.Numeral.Language.TR           as TR
import qualified "this" Text.Numeral.Language.TR.TestData  as TR
import qualified "this" Text.Numeral.Language.WO           as WO
import qualified "this" Text.Numeral.Language.WO.TestData  as WO
import qualified "this" Text.Numeral.Language.YOR          as YOR
import qualified "this" Text.Numeral.Language.YOR.TestData as YOR
import qualified "this" Text.Numeral.Language.ZH           as ZH
import qualified "this" Text.Numeral.Language.ZH.TestData  as ZH


--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

testConversion ∷ (ℤ → Maybe String) → ℤ → String → Assertion
testConversion f n s =
  let r = f n
  in when (r ≢ Just s)
        $ assertFailure
        $ printf "Expected %i = \"%s\" but got \"%s\""
                 n
                 s
                 (fromMaybe "no result" r)

mkTests ∷ String → (ℤ → Maybe String) → [(ℤ, String)] → Test
mkTests name f xs = testGroup name $ map mkTest xs
    where
      mkTest (n, s) = testCase (show n) $ testConversion f n s

tests ∷ [Test]
tests = [ testGroup "AF"
          [ mkTests "cardinal" AF.cardinal AF.cardinals
          , mkTests "ordinal"  AF.ordinal  AF.ordinals
          ]
        , testGroup "AMP" [mkTests "cardinal" AMP.cardinal AMP.cardinals]
        , testGroup "CHN" [mkTests "cardinal" CHN.cardinal CHN.cardinals]
        , testGroup "CHR" [mkTests "cardinal" CHR.cardinal CHR.cardinals]
        , testGroup "DE"
          [ mkTests "cardinal" DE.cardinal DE.cardinals
          , mkTests "ordinal"  DE.ordinal  DE.ordinals
          ]
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
        , testGroup "FR"
          [ mkTests "cardinal" FR.cardinal FR.cardinals
          , mkTests "ordinal"  FR.ordinal  FR.ordinals
          ]
        , testGroup "GSW" [mkTests "cardinal" GSW.cardinal GSW.cardinals]
        , testGroup "GV"  [mkTests "cardinal" GV.cardinal GV.cardinals]
        , testGroup "HE"
          [ testGroup "cardinal"
            [mkTests "feminine" HE.fem_cardinal HE.fem_cardinals]
          ]
        , testGroup "IT"
          [ mkTests "cardinal" IT.cardinal IT.cardinals
          , mkTests "ordinal"  IT.ordinal  IT.ordinals
          ]
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
        , testGroup "PL"  [mkTests "cardinal" PL.cardinal  PL.cardinals]
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
