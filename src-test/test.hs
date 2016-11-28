module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Monad      ( when )
import "base" Data.Function      ( on )
import "base" Data.List          ( groupBy, intercalate, sortBy )
import "base" Data.Maybe         ( fromMaybe )
import "base" System.Environment ( getArgs )
import "base" Text.Printf        ( printf )
import "HUnit" Test.HUnit ( Assertion, assertFailure, assertBool )
import "numerals" Text.Numeral.Grammar
import "numerals" Text.Numeral.Misc ( dec, intLog )
import "QuickCheck" Test.QuickCheck.Modifiers
    ( Positive(Positive), NonNegative(NonNegative) )
import "test-framework" Test.Framework ( defaultMainWithOpts
                                       , interpretArgsOrExit
                                       , ropt_hide_successes
                                       , Test, testGroup
                                       )
import "test-framework-hunit" Test.Framework.Providers.HUnit ( testCase )
import "test-framework-quickcheck2" Test.Framework.Providers.QuickCheck2 ( testProperty )
import           "this" Text.Numeral.Test ( TestData )
import "text" Data.Text ( Text )
import qualified "text" Data.Text as T ( unpack )

import qualified "numerals" Text.Numeral.Language.AFR as AFR
import qualified "numerals" Text.Numeral.Language.AMP as AMP
import qualified "numerals" Text.Numeral.Language.BUL as BUL
import qualified "numerals" Text.Numeral.Language.CES as CES
import qualified "numerals" Text.Numeral.Language.CHN as CHN
import qualified "numerals" Text.Numeral.Language.CHR as CHR
import qualified "numerals" Text.Numeral.Language.CLM as CLM
import qualified "numerals" Text.Numeral.Language.CRO as CRO
import qualified "numerals" Text.Numeral.Language.DEU as DEU
import qualified "numerals" Text.Numeral.Language.ENG as ENG
import qualified "numerals" Text.Numeral.Language.EPO as EPO
import qualified "numerals" Text.Numeral.Language.FIN as FIN
import qualified "numerals" Text.Numeral.Language.FRA as FRA
import qualified "numerals" Text.Numeral.Language.FUR as FUR
import qualified "numerals" Text.Numeral.Language.GLV as GLV
import qualified "numerals" Text.Numeral.Language.GSW as GSW
import qualified "numerals" Text.Numeral.Language.HEB as HEB
import qualified "numerals" Text.Numeral.Language.HOP as HOP
import qualified "numerals" Text.Numeral.Language.ITA as ITA
import qualified "numerals" Text.Numeral.Language.JPN as JPN
import qualified "numerals" Text.Numeral.Language.LAT as LAT
import qualified "numerals" Text.Numeral.Language.LLD as LLD
import qualified "numerals" Text.Numeral.Language.MLG as MLG
import qualified "numerals" Text.Numeral.Language.NEN as NEN
import qualified "numerals" Text.Numeral.Language.NLD as NLD
import qualified "numerals" Text.Numeral.Language.NOB as NOB
import qualified "numerals" Text.Numeral.Language.NQM as NQM
import qualified "numerals" Text.Numeral.Language.OJI as OJI
import qualified "numerals" Text.Numeral.Language.PDC as PDC
import qualified "numerals" Text.Numeral.Language.POL as POL
import qualified "numerals" Text.Numeral.Language.POR as POR
import qualified "numerals" Text.Numeral.Language.RUS as RUS
import qualified "numerals" Text.Numeral.Language.SCO as SCO
import qualified "numerals" Text.Numeral.Language.SPA as SPA
import qualified "numerals" Text.Numeral.Language.SWE as SWE
import qualified "numerals" Text.Numeral.Language.TUR as TUR
import qualified "numerals" Text.Numeral.Language.WOL as WOL
import qualified "numerals" Text.Numeral.Language.YOR as YOR
import qualified "numerals" Text.Numeral.Language.ZHO as ZHO

import qualified "this" Text.Numeral.Language.ACH.TestData as ACH
import qualified "this" Text.Numeral.Language.ADY.TestData as ADY
import qualified "this" Text.Numeral.Language.AFR.TestData as AFR
import qualified "this" Text.Numeral.Language.AMP.TestData as AMP
import qualified "this" Text.Numeral.Language.ARI.TestData as ARI
import qualified "this" Text.Numeral.Language.ARN.TestData as ARN
import qualified "this" Text.Numeral.Language.AST.TestData as AST
import qualified "this" Text.Numeral.Language.AZE.TestData as AZE
import qualified "this" Text.Numeral.Language.BAK.TestData as BAK
import qualified "this" Text.Numeral.Language.BAM.TestData as BAM
import qualified "this" Text.Numeral.Language.BUL.TestData as BUL
import qualified "this" Text.Numeral.Language.CAF.TestData as CAF
import qualified "this" Text.Numeral.Language.CAR.TestData as CAR
import qualified "this" Text.Numeral.Language.CAT.TestData as CAT
import qualified "this" Text.Numeral.Language.CBK.TestData as CBK
import qualified "this" Text.Numeral.Language.CES.TestData as CES
import qualified "this" Text.Numeral.Language.CHN.TestData as CHN
import qualified "this" Text.Numeral.Language.CHR.TestData as CHR
import qualified "this" Text.Numeral.Language.CKU.TestData as CKU
import qualified "this" Text.Numeral.Language.CLM.TestData as CLM
import qualified "this" Text.Numeral.Language.CRO.TestData as CRO
import qualified "this" Text.Numeral.Language.COD.TestData as COD
import qualified "this" Text.Numeral.Language.COO.TestData as COO
import qualified "this" Text.Numeral.Language.COS.TestData as COS
import qualified "this" Text.Numeral.Language.CRG.TestData as CRG
import qualified "this" Text.Numeral.Language.CYM.TestData as CYM
import qualified "this" Text.Numeral.Language.DEU.TestData as DEU
import qualified "this" Text.Numeral.Language.DJK.TestData as DJK
import qualified "this" Text.Numeral.Language.EKK.TestData as EKK
import qualified "this" Text.Numeral.Language.EMI.TestData as EMI
import qualified "this" Text.Numeral.Language.ENG.TestData as ENG
import qualified "this" Text.Numeral.Language.EPO.TestData as EPO
import qualified "this" Text.Numeral.Language.FAO.TestData as FAO
import qualified "this" Text.Numeral.Language.FIN.TestData as FIN
import qualified "this" Text.Numeral.Language.FRA.TestData as FRA
import qualified "this" Text.Numeral.Language.FRA_JER.TestData as FRA_JER
import qualified "this" Text.Numeral.Language.FRR.TestData as FRR
import qualified "this" Text.Numeral.Language.FUR.TestData as FUR
import qualified "this" Text.Numeral.Language.GCF_MTQ.TestData as GCF_MTQ
import qualified "this" Text.Numeral.Language.GIL.TestData as GIL
import qualified "this" Text.Numeral.Language.GLG.TestData as GLG
import qualified "this" Text.Numeral.Language.GLV.TestData as GLV
import qualified "this" Text.Numeral.Language.GSW.TestData as GSW
import qualified "this" Text.Numeral.Language.HAI.TestData as HAI
import qualified "this" Text.Numeral.Language.HAT.TestData as HAT
import qualified "this" Text.Numeral.Language.HEB.TestData as HEB
import qualified "this" Text.Numeral.Language.HOP.TestData as HOP
import qualified "this" Text.Numeral.Language.HRV.TestData as HRV
import qualified "this" Text.Numeral.Language.HUN.TestData as HUN
import qualified "this" Text.Numeral.Language.HUP.TestData as HUP
import qualified "this" Text.Numeral.Language.HUR.TestData as HUR
import qualified "this" Text.Numeral.Language.HYE.TestData as HYE
import qualified "this" Text.Numeral.Language.IBO.TestData as IBO
import qualified "this" Text.Numeral.Language.IND.TestData as IND
import qualified "this" Text.Numeral.Language.INH.TestData as INH
import qualified "this" Text.Numeral.Language.ITA.TestData as ITA
import qualified "this" Text.Numeral.Language.IZH.TestData as IZH
import qualified "this" Text.Numeral.Language.JPN.TestData as JPN
import qualified "this" Text.Numeral.Language.KAP.TestData as KAP
import qualified "this" Text.Numeral.Language.KEA.TestData as KEA
import qualified "this" Text.Numeral.Language.KLB.TestData as KLB
import qualified "this" Text.Numeral.Language.KMR.TestData as KMR
import qualified "this" Text.Numeral.Language.KRL.TestData as KRL
import qualified "this" Text.Numeral.Language.LAT.TestData as LAT
import qualified "this" Text.Numeral.Language.LAV.TestData as LAV
import qualified "this" Text.Numeral.Language.LIN.TestData as LIN
import qualified "this" Text.Numeral.Language.LIT.TestData as LIT
import qualified "this" Text.Numeral.Language.LIV.TestData as LIV
import qualified "this" Text.Numeral.Language.LLD.TestData as LLD
import qualified "this" Text.Numeral.Language.LMO.TestData as LMO
import qualified "this" Text.Numeral.Language.LTZ.TestData as LTZ
import qualified "this" Text.Numeral.Language.MIC.TestData as MIC
import qualified "this" Text.Numeral.Language.MIN.TestData as MIN
import qualified "this" Text.Numeral.Language.MLG.TestData as MLG
import qualified "this" Text.Numeral.Language.MNK.TestData as MNK
import qualified "this" Text.Numeral.Language.MOH.TestData as MOH
import qualified "this" Text.Numeral.Language.NAV.TestData as NAV
import qualified "this" Text.Numeral.Language.NEE.TestData as NEE
import qualified "this" Text.Numeral.Language.NEN.TestData as NEN
import qualified "this" Text.Numeral.Language.NLD.TestData as NLD
import qualified "this" Text.Numeral.Language.NOB.TestData as NOB
import qualified "this" Text.Numeral.Language.NQM.TestData as NQM
import qualified "this" Text.Numeral.Language.OCI.TestData as OCI
import qualified "this" Text.Numeral.Language.OJI.TestData as OJI
import qualified "this" Text.Numeral.Language.ONE.TestData as ONE
import qualified "this" Text.Numeral.Language.ORM.TestData as ORM
import qualified "this" Text.Numeral.Language.PAA.TestData as PAA
import qualified "this" Text.Numeral.Language.PDC.TestData as PDC
import qualified "this" Text.Numeral.Language.POL.TestData as POL
import qualified "this" Text.Numeral.Language.POR.TestData as POR
import qualified "this" Text.Numeral.Language.RMN_DZA.TestData as RMN_DZA
import qualified "this" Text.Numeral.Language.RMY_KAL.TestData as RMY_KAL
import qualified "this" Text.Numeral.Language.RON.TestData as RON
import qualified "this" Text.Numeral.Language.RUS.TestData as RUS
import qualified "this" Text.Numeral.Language.SCO.TestData as SCO
import qualified "this" Text.Numeral.Language.SME.TestData as SME
import qualified "this" Text.Numeral.Language.SMN.TestData as SMN
import qualified "this" Text.Numeral.Language.SPA.TestData as SPA
import qualified "this" Text.Numeral.Language.SQI.TestData as SQI
import qualified "this" Text.Numeral.Language.SWE.TestData as SWE
import qualified "this" Text.Numeral.Language.TAR.TestData as TAR
import qualified "this" Text.Numeral.Language.TGS.TestData as TGS
import qualified "this" Text.Numeral.Language.TUR.TestData as TUR
import qualified "this" Text.Numeral.Language.WMW.TestData as WMW
import qualified "this" Text.Numeral.Language.WOL.TestData as WOL
import qualified "this" Text.Numeral.Language.XPQ.TestData as XPQ
import qualified "this" Text.Numeral.Language.YOR.TestData as YOR
import qualified "this" Text.Numeral.Language.ZAI.TestData as ZAI
import qualified "this" Text.Numeral.Language.ZAQ.TestData as ZAQ
import qualified "this" Text.Numeral.Language.ZHO.TestData as ZHO
import qualified "this" Text.Numeral.Language.ZPC.TestData as ZPC
import qualified "this" Text.Numeral.Language.ZPL.TestData as ZPL


--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main :: IO ()
main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests opts { ropt_hide_successes = Just True }

tests :: [Test]
tests =
  [ testGroup "Grammar"
    [ testGroup "Case"
      [ testCase "noCase"       $ testInf noCase       hasNoCase
      , testCase "nominative"   $ testInf nominative   isNominative
      , testCase "accusative"   $ testInf accusative   isAccusative
      , testCase "dative"       $ testInf dative       isDative
      , testCase "ablative"     $ testInf ablative     isAblative
      , testCase "genitive"     $ testInf genitive     isGenitive
      , testCase "vocative"     $ testInf vocative     isVocative
      , testCase "instrumental" $ testInf instrumental isInstrumental
      ]
    , testGroup "Gender"
      [ testCase "noGender"  $ testInf noGender  hasNoGender
      , testCase "neuter"    $ testInf neuter    isNeuter
      , testCase "masculine" $ testInf masculine isMasculine
      , testCase "feminine"  $ testInf feminine  isFeminine
      , testCase "common"    $ testInf common    isCommon
      ]
    , testGroup "Number"
      [ testCase "noNumber" $ testInf noNumber hasNoNumber
      , testCase "singular" $ testInf singular isSingular
      , testCase "dual"     $ testInf dual     isDual
      , testCase "trial"    $ testInf trial    isTrial
      , testCase "paucal"   $ testInf paucal   isPaucal
      , testCase "plural"   $ testInf plural   isPlural
      ]
    ]
  , testGroup "Misc"
    [ testGroup "intLog"
      [ testProperty "power of 10" intLog_pow10
      , testProperty "power of 10 minus 1" intLog_pow10m1
      , testProperty "multiply" intLog_mul
      ]
    ]
  , testGroup "ACH"
    [ testGroup "unique data"
      [testUnique "cardinal" (ACH.cardinals :: TestData Integer)]
    ]
  , testGroup "ADY"
    [ testGroup "unique data"
      [testUnique "cardinal" (ADY.cardinals :: TestData Integer)]
    ]
  , testGroup "AF"
    [ testGroup "unique data"
      [ testUnique "cardinal" (AFR.cardinals :: TestData Integer)
      , testUnique "ordinal"  (AFR.ordinals  :: TestData Integer)
      ]
    , mkTests "cardinal" AFR.cardinal (AFR.cardinals :: TestData Integer)
    , mkTests "ordinal"  AFR.ordinal  (AFR.ordinals  :: TestData Integer)
    ]
  , testGroup "AMP"
    [ testGroup "unique data"
      [testUnique "cardinal" (AMP.cardinals :: TestData Integer)]
    , mkTests "cardinal" AMP.cardinal (AMP.cardinals :: TestData Integer)
    ]
  , testGroup "ARI"
    [ testGroup "unique data"
      [testUnique "cardinal" (ARI.cardinals :: TestData Integer)]
    ]
  , testGroup "ARN"
    [ testGroup "unique data"
      [testUnique "cardinal" (ARN.cardinals :: TestData Integer)]
    ]
  , testGroup "AST"
    [ testGroup "unique data"
      [testUnique "cardinal" (AST.cardinals :: TestData Integer)]
    ]
  , testGroup "AZE"
    [ testGroup "unique data"
      [testUnique "cardinal" (AZE.cardinals :: TestData Integer)]
    ]
  , testGroup "BAK"
    [ testGroup "unique data"
      [testUnique "cardinal" (BAK.cardinals :: TestData Integer)]
    ]
  , testGroup "BUL"
    [ testGroup "unique data"
      [testUnique "cardinal" (BUL.cardinals :: TestData Integer)]
    , mkTests "cardinal" BUL.cardinal (BUL.cardinals :: TestData Integer)
    ]
  , testGroup "BAM"
    [ testGroup "unique data"
      [testUnique "cardinal" (BAM.cardinals :: TestData Integer)]
    ]
  , testGroup "CAT"
    [ testGroup "unique data"
      [testUnique "cardinal" (CAT.cardinals :: TestData Integer)]
    ]
  , testGroup "CAF"
    [ testGroup "unique data"
      [ testUnique "cardinal" (CAF.cardinals :: TestData Integer)
      , testUnique "cardinal (syllabic)" (CAF.syllabic_cardinals :: TestData Integer)
      ]
    ]
  , testGroup "CAR"
    [ testGroup "unique data"
      [testUnique "cardinal" (CAR.cardinals :: TestData Integer)]
    ]
  , testGroup "CBK"
    [ testGroup "unique data"
      [testUnique "cardinal" (CBK.cardinals :: TestData Integer)]
    ]
  , testGroup "CHN"
    [ testGroup "unique data"
      [testUnique "cardinal" (CHN.cardinals :: TestData Integer)]
    , mkTests "cardinal" CHN.cardinal (CHN.cardinals :: TestData Integer)
    ]
  , testGroup "CHR"
    [ testGroup "unique data"
      [testUnique "cardinal" (CHR.cardinals :: TestData Integer)]
    , mkTests "cardinal" CHR.cardinal (CHR.cardinals :: TestData Integer)
    ]
  , testGroup "CKU"
    [ testGroup "unique data"
      [testUnique "cardinal" (CKU.cardinals :: TestData Integer)]
    ]
  , testGroup "CLM"
    [ testGroup "unique data"
      [testUnique "cardinal" (CLM.cardinals :: TestData Integer)]
    , mkTests "cardinal" CLM.cardinal (CLM.cardinals :: TestData Integer)
    ]
  , testGroup "CRO"
    [ testGroup "unique data"
      [testUnique "cardinal" (CRO.cardinals :: TestData Integer)]
    , mkTests "cardinal" CRO.cardinal (CRO.cardinals :: TestData Integer)
    ]
  , testGroup "COS"
    [ testGroup "unique data"
      [testUnique "cardinal" (COS.cardinals :: TestData Integer)]
    ]
  , testGroup "COD"
    [ testGroup "unique data"
      [testUnique "cardinal" (COD.cardinals :: TestData Integer)]
    ]
  , testGroup "COO"
    [ testGroup "unique data"
      [testUnique "cardinal" (COO.cardinals :: TestData Integer)]
    ]
  , testGroup "CRG"
    [ testGroup "unique data"
      [testUnique "cardinal" (CRG.cardinals :: TestData Integer)]
    ]
  , testGroup "CES"
    [ testGroup "unique data"
      [testUnique "cardinal" (CES.cardinals :: TestData Integer)]
    , mkTests "cardinal" CES.cardinal (CES.cardinals :: TestData Integer)
    ]
  , testGroup "CYM"
    [ testGroup "unique data"
      [testUnique "cardinal" (CYM.cardinals :: TestData Integer)]
    ]
  , testGroup "DEU"
    [ testGroup "unique data"
      [ testUnique "cardinal" (DEU.cardinals :: TestData Integer)
      , testUnique "ordinal"  (DEU.ordinals  :: TestData Integer)
      ]
    , mkTests "cardinal" DEU.cardinal (DEU.cardinals :: TestData Integer)
    , mkTests "ordinal"  DEU.ordinal  (DEU.ordinals  :: TestData Integer)
    ]
  , testGroup "DJK"
    [ testGroup "unique data"
      [testUnique "cardinal" (DJK.cardinals :: TestData Integer)]
    ]
  , testGroup "EMI"
    [ testGroup "unique data"
      [testUnique "cardinal" (EMI.cardinals :: TestData Integer)]
    ]
  , testGroup "ENG"
    [ testGroup "GB"
      [ testGroup "unique data"
        [ testUnique "cardinal" (ENG.gb_cardinals :: TestData Integer)
        , testUnique "ordinal"  (ENG.gb_ordinals  :: TestData Integer)
        ]
      , mkTests "cardinal" ENG.gb_cardinal (ENG.gb_cardinals :: TestData Integer)
      , mkTests "ordinal"  ENG.gb_ordinal  (ENG.gb_ordinals  :: TestData Integer)
      ]
    , testGroup "US"
      [ testGroup "unique data"
        [ testUnique "cardinal" (ENG.us_cardinals :: TestData Integer)
        , testUnique "ordinal"  (ENG.us_ordinals  :: TestData Integer)
        ]
      , mkTests "cardinal" ENG.us_cardinal (ENG.us_cardinals :: TestData Integer)
      , mkTests "ordinal"  ENG.us_ordinal  (ENG.us_ordinals  :: TestData Integer)
      ]
    ]
  , testGroup "EPO"
    [ testGroup "unique data"
      [testUnique "cardinal" (EPO.cardinals :: TestData Integer)]
    , mkTests "cardinal" EPO.cardinal (EPO.cardinals :: TestData Integer)
    ]
  , testGroup "SPA"
    [ testGroup "unique data"
      [testUnique "cardinal" (SPA.cardinals :: TestData Integer)]
    , mkTests "cardinal" SPA.cardinal (SPA.cardinals :: TestData Integer)
    ]
  , testGroup "SQI"
    [ testGroup "unique data"
      [testUnique "cardinal" (SQI.cardinals :: TestData Integer)]
    ]
  , testGroup "EKK"
    [ testGroup "unique data"
      [testUnique "cardinal" (EKK.cardinals :: TestData Integer)]
    ]
  , testGroup "FIN"
    [ testGroup "unique data"
      [ testUnique "cardinal" (FIN.cardinals :: TestData Integer)
      , testUnique "ordinal"  (FIN.ordinals  :: TestData Integer)
      ]
    , mkTests "cardinal" FIN.cardinal (FIN.cardinals :: TestData Integer)
    , mkTests "ordinal"  FIN.ordinal  (FIN.ordinals :: TestData Integer)
    ]
  , testGroup "FAO"
    [ testGroup "unique data"
      [testUnique "cardinal" (FAO.cardinals :: TestData Integer)]
    ]
  , testGroup "FRA"
    [ testGroup "unique data"
      [ testUnique "cardinal" (FRA.cardinals :: TestData Integer)
      , testUnique "ordinal"  (FRA.ordinals  :: TestData Integer)
      ]
    , mkTests "cardinal" FRA.cardinal (FRA.cardinals :: TestData Integer)
    , mkTests "ordinal"  FRA.ordinal  (FRA.ordinals  :: TestData Integer)
    ]
  , testGroup "FRR"
    [ testGroup "unique data"
      [testUnique "cardinal" (FRR.cardinals :: TestData Integer)]
    ]
  , testGroup "FRA_JER"
    [ testGroup "unique data"
      [testUnique "cardinal" (FRA_JER.cardinals :: TestData Integer)]
    ]
  , testGroup "FUR"
    [ testGroup "unique data"
      [testUnique "cardinal" (FUR.cardinals :: TestData Integer)]
    , mkTests "cardinal" FUR.cardinal (FUR.cardinals :: TestData Integer)
   ]
  , testGroup "GCF_MTQ"
    [ testGroup "unique data"
      [testUnique "cardinal" (GCF_MTQ.cardinals :: TestData Integer)]
    ]
  , testGroup "GIL"
    [ testGroup "unique data"
      [testUnique "cardinal" (GIL.cardinals :: TestData Integer)]
    ]
  , testGroup "GLG"
    [ testGroup "unique data"
      [testUnique "cardinal" (GLG.cardinals :: TestData Integer)]
    ]
  , testGroup "GSW"
    [ testGroup "unique data"
      [testUnique "cardinal" (GSW.cardinals :: TestData Integer)]
    , mkTests "cardinal" GSW.cardinal (GSW.cardinals :: TestData Integer)
    ]
  , testGroup "GLV"
    [ testGroup "unique data"
      [testUnique "cardinal" (GLV.cardinals :: TestData Integer)]
    , mkTests "cardinal" GLV.cardinal  (GLV.cardinals  :: TestData Integer)
    ]
  , testGroup "HAI"
    [ testGroup "unique data"
      [testUnique "cardinal" (HAI.cardinals :: TestData Integer)]
    ]
  , testGroup "HEB"
    [ testGroup "unique data"
      [testUnique "cardinal" (HEB.cardinals :: TestData Integer)]
    , mkTests "cardinal" HEB.cardinal  (HEB.cardinals  :: TestData Integer)
    ]
  , testGroup "HOP"
    [ testGroup "unique data"
      [testUnique "cardinal" (HOP.cardinals :: TestData Integer)]
    , mkTests "cardinal" HOP.cardinal (HOP.cardinals :: TestData Integer)
    ]
  , testGroup "HRV"
    [ testGroup "unique data"
      [testUnique "cardinal" (HRV.cardinals :: TestData Integer)]
    ]
  , testGroup "HAT"
    [ testGroup "unique data"
      [testUnique "cardinal" (HAT.cardinals :: TestData Integer)]
    ]
  , testGroup "HUN"
    [ testGroup "unique data"
      [testUnique "cardinal" (HUN.cardinals :: TestData Integer)]
    ]
  , testGroup "HUP"
    [ testGroup "unique data"
      [testUnique "cardinal" (HUP.cardinals :: TestData Integer)]
    ]
  , testGroup "HUR"
    [ testGroup "unique data"
      [testUnique "cardinal" (HUR.cardinals :: TestData Integer)]
    ]
  , testGroup "HYE"
    [ testGroup "unique data"
      [testUnique "cardinal" (HYE.cardinals :: TestData Integer)]
    ]
  , testGroup "IND"
    [ testGroup "unique data"
      [testUnique "cardinal" (IND.cardinals :: TestData Integer)]
    ]
  , testGroup "IBO"
    [ testGroup "unique data"
      [testUnique "cardinal" (IBO.cardinals :: TestData Integer)]
    ]
  , testGroup "INH"
    [ testGroup "unique data"
      [testUnique "cardinal" (INH.cardinals :: TestData Integer)]
    ]
  , testGroup "ITA"
    [ testGroup "unique data"
      [testUnique "cardinal" (ITA.cardinals :: TestData Integer)]
    , mkTests "cardinal" ITA.cardinal (ITA.cardinals :: TestData Integer)
    , mkTests "ordinal"  ITA.ordinal  (ITA.ordinals  :: TestData Integer)
    ]
  , testGroup "IZH"
    [ testGroup "unique data"
      [testUnique "cardinal" (IZH.cardinals :: TestData Integer)]
    ]
  , testGroup "JPN"
    [ testGroup "preferred"
      [ testGroup "unique data"
        [testUnique "cardinal" (JPN.preferred_cardinals :: TestData Integer)]
      , mkTests "cardinal" JPN.preferred_cardinal
                          (JPN.preferred_cardinals :: TestData Integer)
      ]
    , testGroup "kanji"
      [ testGroup "unique data"
        [testUnique "cardinal" (JPN.kanji_cardinals :: TestData Integer)]
      , mkTests "cardinal" JPN.kanji_cardinal (JPN.kanji_cardinals :: TestData Integer)
      ]
    , testGroup "daiji"
      [ testGroup "unique data"
        [testUnique "cardinal" (JPN.daiji_cardinals :: TestData Integer)]
      , mkTests "cardinal" JPN.daiji_cardinal (JPN.daiji_cardinals :: TestData Integer)
      ]
    ]
  , testGroup "KAP"
    [ testGroup "unique data"
      [testUnique "cardinal" (KAP.cardinals :: TestData Integer)]
    ]
  , testGroup "KEA"
    [ testGroup "unique data"
      [testUnique "cardinal" (KEA.cardinals :: TestData Integer)]
    ]
  , testGroup "KLB"
    [ testGroup "unique data"
      [testUnique "cardinal" (KLB.cardinals :: TestData Integer)]
    ]
  , testGroup "KMR"
    [ testGroup "unique data"
      [testUnique "cardinal" (KMR.cardinals :: TestData Integer)]
    ]
  , testGroup "KRL"
    [ testGroup "unique data"
      [testUnique "cardinal" (KRL.cardinals :: TestData Integer)]
    ]
  , testGroup "LAT"
    [ testGroup "unique data"
      [testUnique "cardinal" (LAT.cardinals :: TestData Integer)]
    , mkTests "cardinal" LAT.cardinal (LAT.cardinals :: TestData Integer)
    ]
  , testGroup "LTZ"
    [ testGroup "unique data"
      [testUnique "cardinal" (LTZ.cardinals :: TestData Integer)]
    ]
  , testGroup "LIV"
    [ testGroup "unique data"
      [testUnique "cardinal" (LIV.cardinals :: TestData Integer)]
    ]
  , testGroup "LLD"
    [ testGroup "unique data"
      [testUnique "cardinal" (LLD.cardinals :: TestData Integer)]
    , mkTests "cardinal" LLD.cardinal (LLD.cardinals :: TestData Integer)
    ]
  , testGroup "LMO"
    [ testGroup "unique data"
      [testUnique "cardinal" (LMO.cardinals :: TestData Integer)]
    ]
  , testGroup "LIN"
    [ testGroup "unique data"
      [testUnique "cardinal" (LIN.cardinals :: TestData Integer)]
    ]
  , testGroup "LIT"
    [ testGroup "unique data"
      [testUnique "cardinal" (LIT.cardinals :: TestData Integer)]
    ]
  , testGroup "LAV"
    [ testGroup "unique data"
      [testUnique "cardinal" (LAV.cardinals :: TestData Integer)]
    ]
  , testGroup "MLG"
    [ testGroup "unique data"
      [testUnique "cardinal" (MLG.cardinals :: TestData Integer)]
    , mkTests "cardinal" MLG.cardinal (MLG.cardinals  :: TestData Integer)
    ]
  , testGroup "MIC"
    [ testGroup "unique data"
      [testUnique "cardinal" (MIC.cardinals :: TestData Integer)]
    ]
  , testGroup "MIN"
    [ testGroup "unique data"
      [testUnique "cardinal" (MIN.cardinals :: TestData Integer)]
    ]
  , testGroup "MNK"
    [ testGroup "unique data"
      [testUnique "cardinal" (MNK.cardinals :: TestData Integer)]
    ]
  , testGroup "MOH"
    [ testGroup "unique data"
      [testUnique "cardinal" (MOH.cardinals :: TestData Integer)]
    ]
  , testGroup "NEE"
    [ testGroup "unique data"
      [testUnique "cardinal" (NEE.cardinals :: TestData Integer)]
    ]
  , testGroup "NEN"
    [ testGroup "unique data"
      [testUnique "cardinal" (NEN.cardinals :: TestData Integer)]
    , mkTests "cardinal" NEN.cardinal (NEN.cardinals :: TestData Integer)
    ]
  , testGroup "NLD"
    [ testGroup "unique data"
      [ testUnique "cardinal"       (NLD.cardinals       :: TestData Integer)
      , testUnique "ordinal"        (NLD.ordinals        :: TestData Integer)
      , testUnique "partitive"      (NLD.partitives      :: TestData (Integer, Integer))
      , testUnique "multiplicative" (NLD.multiplicatives :: TestData Integer)
      ]
    , mkTests "cardinal"       NLD.cardinal  (NLD.cardinals  :: TestData Integer)
    , mkTests "ordinal"        NLD.ordinal   (NLD.ordinals   :: TestData Integer)
    , mkTests "partitive"      NLD.partitive (NLD.partitives :: TestData (Integer, Integer))
    , mkTests "multiplicative" NLD.multiplicative (NLD.multiplicatives :: TestData Integer)
    ]
  , testGroup "NOB"
    [ testGroup "unique data"
      [testUnique "cardinal" (NOB.cardinals :: TestData Integer)]
    , mkTests "cardinal" NOB.cardinal (NOB.cardinals  :: TestData Integer)
    ]
  , testGroup "NQM"
    [ testGroup "unique data"
      [testUnique "cardinal" (NQM.cardinals :: TestData Integer)]
    , mkTests "cardinal" NQM.cardinal (NQM.cardinals :: TestData Integer)
    ]
  , testGroup "NAV"
    [ testGroup "unique data"
      [testUnique "cardinal" (NAV.cardinals :: TestData Integer)]
    ]
  , testGroup "OCI"
    [ testGroup "unique data"
      [testUnique "cardinal" (OCI.cardinals :: TestData Integer)]
    ]
  , testGroup "OJI"
    [ testGroup "unique data"
      [testUnique "cardinal" (OJI.cardinals :: TestData Integer)]
    , mkTests "cardinal" OJI.cardinal (OJI.cardinals  :: TestData Integer)
    ]
  , testGroup "ONE"
    [ testGroup "unique data"
      [testUnique "cardinal" (ONE.cardinals :: TestData Integer)]
    ]
  , testGroup "ORM"
    [ testGroup "unique data"
      [testUnique "cardinal" (ORM.cardinals :: TestData Integer)]
    ]
  , testGroup "PAA"
    [ testGroup "unique data"
      [testUnique "cardinal" (PAA.cardinals :: TestData Integer)]
    ]
  , testGroup "PDC"
    [ testGroup "unique data"
      [testUnique "cardinal" (PDC.cardinals :: TestData Integer)]
    , mkTests "cardinal" PDC.cardinal (PDC.cardinals :: TestData Integer)
    ]
  , testGroup "POL"
    [ testGroup "unique data"
      [testUnique "cardinal" (POL.cardinals :: TestData Integer)]
    , mkTests "cardinal" POL.cardinal (POL.cardinals  :: TestData Integer)
    ]
  , testGroup "POR"
    [ testGroup "unique data"
      [testUnique "cardinal" (POR.cardinals :: TestData Integer)]
    , mkTests "cardinal" POR.cardinal (POR.cardinals  :: TestData Integer)
    , mkTests "ordinal"  POR.ordinal  (POR.ordinals   :: TestData Integer)
    ]
  , testGroup "RMN_DZA"
    [ testGroup "unique data"
      [testUnique "cardinal" (RMN_DZA.cardinals :: TestData Integer)]
    ]
  , testGroup "RMY_KAL"
    [ testGroup "unique data"
      [testUnique "cardinal" (RMY_KAL.cardinals :: TestData Integer)]
    ]
  , testGroup "RON"
    [ testGroup "unique data"
      [testUnique "cardinal" (RON.cardinals :: TestData Integer)]
    ]
  , testGroup "RUS"
    [ testGroup "unique data"
      [testUnique "cardinal" (RUS.cardinals :: TestData Integer)]
    , mkTests "cardinal" RUS.cardinal (RUS.cardinals  :: TestData Integer)
    ]
  , testGroup "SCO"
    [ testGroup "unique data"
      [testUnique "cardinal" (SCO.cardinals :: TestData Integer)]
    , mkTests "cardinal" SCO.cardinal (SCO.cardinals :: TestData Integer)
    ]
  , testGroup "SME"
    [ testGroup "unique data"
      [testUnique "cardinal" (SME.cardinals :: TestData Integer)]
    ]
  , testGroup "SMN"
    [ testGroup "unique data"
      [testUnique "cardinal" (SMN.cardinals :: TestData Integer)]
    ]
  , testGroup "SWE"
    [ testGroup "unique data"
      [ testUnique "cardinal" (SWE.cardinals :: TestData Integer)
      , testUnique "ordinal"  (SWE.ordinals  :: TestData Integer)
      ]
    , mkTests "cardinal" SWE.cardinal (SWE.cardinals :: TestData Integer)
    , mkTests "ordinal"  SWE.ordinal  (SWE.ordinals  :: TestData Integer)
    ]
  , testGroup "TAR"
    [ testGroup "unique data"
      [testUnique "cardinal" (TAR.cardinals :: TestData Integer)]
    ]
  , testGroup "TGS"
    [ testGroup "unique data"
      [testUnique "cardinal" (TGS.cardinals :: TestData Integer)]
    ]
  , testGroup "TUR"
    [ testGroup "unique data"
      [testUnique "cardinal" (TUR.cardinals :: TestData Integer)]
    , mkTests "cardinal" TUR.cardinal (TUR.cardinals :: TestData Integer)
    ]
  , testGroup "WMW"
    [ testGroup "unique data"
      [testUnique "cardinal" (WMW.cardinals :: TestData Integer)]
    ]
  , testGroup "WOL"
    [ testGroup "unique data"
      [testUnique "cardinal" (WOL.cardinals :: TestData Integer)]
    , mkTests "cardinal" WOL.cardinal (WOL.cardinals :: TestData Integer)
    ]
  , testGroup "XPQ"
    [ testGroup "unique data"
      [testUnique "cardinal" (XPQ.cardinals :: TestData Integer)]
    ]
  , testGroup "YOR"
    [ testGroup "unique data"
      [testUnique "cardinal" (YOR.cardinals :: TestData Integer)]
    , mkTests "cardinal" YOR.cardinal (YOR.cardinals :: TestData Integer)
    ]
  , testGroup "ZAI"
    [ testGroup "unique data"
      [testUnique "cardinal" (ZAI.cardinals :: TestData Integer)]
    ]
  , testGroup "ZAQ"
    [ testGroup "unique data"
      [testUnique "cardinal" (ZAQ.cardinals :: TestData Integer)]
    ]
  , testGroup "ZHO"
    [ testGroup "characters (traditional)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.trad_cardinals :: TestData Integer)]
      , mkTests "cardinal" ZHO.trad_cardinal (ZHO.trad_cardinals :: TestData Integer)
      ]
    , testGroup "characters (simplified)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.simpl_cardinals :: TestData Integer)]
      , mkTests "cardinal" ZHO.simpl_cardinal (ZHO.simpl_cardinals :: TestData Integer)
      ]
    , testGroup "financial characters (traditional)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.finance_trad_cardinals :: TestData Integer)]
      , mkTests "cardinal" ZHO.finance_trad_cardinal (ZHO.finance_trad_cardinals :: TestData Integer)
      ]
    , testGroup "financial characters (simplified)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.finance_simpl_cardinals :: TestData Integer)]
      , mkTests "cardinal" ZHO.finance_simpl_cardinal (ZHO.finance_simpl_cardinals :: TestData Integer)
      ]
    , testGroup "pinyin"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.pinyin_cardinals :: TestData Integer)]
      , mkTests "cardinal" ZHO.pinyin_cardinal (ZHO.pinyin_cardinals :: TestData Integer)
      ]
    ]
  , testGroup "ZPC"
    [ testGroup "unique data"
      [testUnique "cardinal" (ZPC.cardinals :: TestData Integer)]
    ]
  , testGroup "ZPL"
    [ testGroup "unique data"
      [testUnique "cardinal" (ZPL.cardinals :: TestData Integer)]
    ]
  ]


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

{-
-- TODO: this takes way to long for large values (10 ^ 60000 - 1)
testBounds :: Text -> (Integer -> Maybe Text) -> (Integer, Integer) -> Test
testBounds name conversion bounds@(lo, hi) =
      testGroup name
                [ testCase "lower bound" $ assertBool "conversion error" $ checkConv lo
                , testCase "upper bound" $ assertBool "conversion error" $ checkConv hi
                -- , testProperty "in between" $ forAll (choose bounds) checkConv
                ]
    where
      checkConv :: Integer -> Bool
      checkConv = isJust . conversion
-}

testInf :: (Inflection -> Inflection) -> (Inflection -> Bool) -> Assertion
testInf set test = assertBool "False" $ test $ set defaultInflection

intLog_pow10 :: Positive Integer -> Bool
intLog_pow10 (Positive x) = x == intLog (dec x)

intLog_pow10m1 :: NonNegative Integer -> Bool
intLog_pow10m1 (NonNegative x) = x - 1 == intLog (dec x) - 1

intLog_mul :: Positive Integer -> Positive Integer -> Bool
intLog_mul (Positive x) (Positive y) = intLog (dec x * dec y) == intLog (dec x) + intLog (dec y)

mkTests :: forall a. (Show a)
        => String
        -> (Inflection -> a -> Maybe Text)
        -> TestData a
        -> Test
mkTests name f = testGroup name . map perInflection
  where
    perInflection :: (String, Inflection, [(a, Text)]) -> Test
    perInflection (infName, inf, xs) = testGroup infName $ map (perValue inf) xs

    perValue :: Inflection -> (a, Text) -> Test
    perValue inf (n, s) = testCase (show n) $ testConversion f inf n s

-- | Tests whether the test data contains duplicates (different
-- numbers with the same representation).
testUnique :: forall a. (Show a) => String -> TestData a -> Test
testUnique name = testGroup name . map perInflection
  where
    perInflection :: (String, inf, [(a, Text)]) -> Test
    perInflection (infName, _, xs) = testCase infName $ unique xs

    unique :: [(a, Text)] -> Assertion
    unique xs = when (length ys > 0)
                  $ assertFailure
                  $ intercalate "\n" $ map msg ys
      where
        ys :: [[(a, Text)]]
        ys = filter ((1 <) . length)
             $ groupBy ((==) `on` snd)
             $ sortBy (compare `on` snd) xs

    msg :: [(a, Text)] -> String
    msg xs = let (_, s) = head xs
             in printf "The string \"%s\" is associated with multiple values: %s"
                       (T.unpack s)
                       (intercalate ", " $ map (show . fst) xs)

testConversion :: (Show a)
               => (Inflection -> a -> Maybe Text)
               -> Inflection
               -> a
               -> Text
               -> Assertion
testConversion f inf n s =
  let r = f inf n
  in when (r /= Just s)
        $ assertFailure
        $ printf "Expected %s = \"%s\" but got \"%s\""
                 (show n)
                 (T.unpack s)
                 (T.unpack $ fromMaybe "no result" r)
