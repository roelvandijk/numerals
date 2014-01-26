{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}


module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Monad      ( (=<<), when )
import "base" Data.Bool          ( Bool(True) )
import "base" Data.Function      ( ($), on )
import "base" Data.List          ( filter, groupBy, head, intercalate, length, map, sortBy )
import "base" Data.Maybe         ( Maybe(Just), fromMaybe )
import "base" Data.Ord           ( (<), (>), compare )
import "base" Data.Tuple         ( fst, snd )
import "base" Prelude            ( (+), (-), String )
import "base" System.Environment ( getArgs )
import "base" System.IO          ( IO )
import "base" Text.Printf        ( printf )
import "base" Text.Show          ( Show, show )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡), (≢) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Prelude.Unicode       ( ℤ, (⋅) )
import "HUnit" Test.HUnit ( Assertion, assertFailure, assertBool )
import           "numerals" Text.Numeral.Grammar
import qualified "numerals" Text.Numeral.Grammar.Reified as GR
import           "numerals" Text.Numeral.Misc ( dec, intLog )
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

main ∷ IO ()
main = do opts ← interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests opts { ropt_hide_successes = Just True }

tests ∷ [Test]
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
      [testUnique "cardinal" (ACH.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "ADY"
    [ testGroup "unique data"
      [testUnique "cardinal" (ADY.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "AF"
    [ testGroup "unique data"
      [ testUnique "cardinal" (AFR.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (AFR.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" AFR.cardinal (AFR.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  AFR.ordinal  (AFR.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "AMP"
    [ testGroup "unique data"
      [testUnique "cardinal" (AMP.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" AMP.cardinal (AMP.cardinals ∷ TestData ℤ)
    ]
  , testGroup "ARI"
    [ testGroup "unique data"
      [testUnique "cardinal" (ARI.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "ARN"
    [ testGroup "unique data"
      [testUnique "cardinal" (ARN.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "AST"
    [ testGroup "unique data"
      [testUnique "cardinal" (AST.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "AZE"
    [ testGroup "unique data"
      [testUnique "cardinal" (AZE.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "BAK"
    [ testGroup "unique data"
      [testUnique "cardinal" (BAK.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "BUL"
    [ testGroup "unique data"
      [testUnique "cardinal" (BUL.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" BUL.cardinal (BUL.cardinals ∷ TestData ℤ)
    ]
  , testGroup "BAM"
    [ testGroup "unique data"
      [testUnique "cardinal" (BAM.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "CAT"
    [ testGroup "unique data"
      [testUnique "cardinal" (CAT.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "CAF"
    [ testGroup "unique data"
      [ testUnique "cardinal" (CAF.cardinals ∷ TestData ℤ)
      , testUnique "cardinal (syllabic)" (CAF.syllabic_cardinals ∷ TestData ℤ)
      ]
    ]
  , testGroup "CAR"
    [ testGroup "unique data"
      [testUnique "cardinal" (CAR.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "CBK"
    [ testGroup "unique data"
      [testUnique "cardinal" (CBK.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "CHN"
    [ testGroup "unique data"
      [testUnique "cardinal" (CHN.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" CHN.cardinal (CHN.cardinals ∷ TestData ℤ)
    ]
  , testGroup "CHR"
    [ testGroup "unique data"
      [testUnique "cardinal" (CHR.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" CHR.cardinal (CHR.cardinals ∷ TestData ℤ)
    ]
  , testGroup "CKU"
    [ testGroup "unique data"
      [testUnique "cardinal" (CKU.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "CLM"
    [ testGroup "unique data"
      [testUnique "cardinal" (CLM.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" CLM.cardinal (CLM.cardinals ∷ TestData ℤ)
    ]
  , testGroup "COS"
    [ testGroup "unique data"
      [testUnique "cardinal" (COS.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "COD"
    [ testGroup "unique data"
      [testUnique "cardinal" (COD.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "COO"
    [ testGroup "unique data"
      [testUnique "cardinal" (COO.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "CRG"
    [ testGroup "unique data"
      [testUnique "cardinal" (CRG.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "CES"
    [ testGroup "unique data"
      [testUnique "cardinal" (CES.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" CES.cardinal (CES.cardinals ∷ TestData ℤ)
    ]
  , testGroup "CYM"
    [ testGroup "unique data"
      [testUnique "cardinal" (CYM.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "DEU"
    [ testGroup "unique data"
      [ testUnique "cardinal" (DEU.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (DEU.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" DEU.cardinal (DEU.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  DEU.ordinal  (DEU.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "DJK"
    [ testGroup "unique data"
      [testUnique "cardinal" (DJK.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "EMI"
    [ testGroup "unique data"
      [testUnique "cardinal" (EMI.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "ENG"
    [ testGroup "GB"
      [ testGroup "unique data"
        [ testUnique "cardinal" (ENG.gb_cardinals ∷ TestData ℤ)
        , testUnique "ordinal"  (ENG.gb_ordinals  ∷ TestData ℤ)
        ]
      , mkTests "cardinal" ENG.gb_cardinal (ENG.gb_cardinals ∷ TestData ℤ)
      , mkTests "ordinal"  ENG.gb_ordinal  (ENG.gb_ordinals  ∷ TestData ℤ)
      ]
    , testGroup "US"
      [ testGroup "unique data"
        [ testUnique "cardinal" (ENG.us_cardinals ∷ TestData ℤ)
        , testUnique "ordinal"  (ENG.us_ordinals  ∷ TestData ℤ)
        ]
      , mkTests "cardinal" ENG.us_cardinal (ENG.us_cardinals ∷ TestData ℤ)
      , mkTests "ordinal"  ENG.us_ordinal  (ENG.us_ordinals  ∷ TestData ℤ)
      ]
    ]
  , testGroup "EPO"
    [ testGroup "unique data"
      [testUnique "cardinal" (EPO.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" EPO.cardinal (EPO.cardinals ∷ TestData ℤ)
    ]
  , testGroup "SPA"
    [ testGroup "unique data"
      [testUnique "cardinal" (SPA.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" SPA.cardinal (SPA.cardinals ∷ TestData ℤ)
    ]
  , testGroup "SQI"
    [ testGroup "unique data"
      [testUnique "cardinal" (SQI.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "EKK"
    [ testGroup "unique data"
      [testUnique "cardinal" (EKK.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "FIN"
    [ testGroup "unique data"
      [ testUnique "cardinal" (FIN.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (FIN.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" FIN.cardinal (FIN.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  FIN.ordinal  (FIN.ordinals ∷ TestData ℤ)
    ]
  , testGroup "FAO"
    [ testGroup "unique data"
      [testUnique "cardinal" (FAO.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "FRA"
    [ testGroup "unique data"
      [ testUnique "cardinal" (FRA.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (FRA.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" FRA.cardinal (FRA.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  FRA.ordinal  (FRA.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "FRR"
    [ testGroup "unique data"
      [testUnique "cardinal" (FRR.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "FRA_JER"
    [ testGroup "unique data"
      [testUnique "cardinal" (FRA_JER.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "FUR"
    [ testGroup "unique data"
      [testUnique "cardinal" (FUR.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" FUR.cardinal (FUR.cardinals ∷ TestData ℤ)
   ]
  , testGroup "GCF_MTQ"
    [ testGroup "unique data"
      [testUnique "cardinal" (GCF_MTQ.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "GIL"
    [ testGroup "unique data"
      [testUnique "cardinal" (GIL.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "GLG"
    [ testGroup "unique data"
      [testUnique "cardinal" (GLG.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "GSW"
    [ testGroup "unique data"
      [testUnique "cardinal" (GSW.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" GSW.cardinal (GSW.cardinals ∷ TestData ℤ)
    ]
  , testGroup "GLV"
    [ testGroup "unique data"
      [testUnique "cardinal" (GLV.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" GLV.cardinal  (GLV.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "HAI"
    [ testGroup "unique data"
      [testUnique "cardinal" (HAI.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HEB"
    [ testGroup "unique data"
      [testUnique "cardinal" (HEB.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" HEB.cardinal  (HEB.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "HOP"
    [ testGroup "unique data"
      [testUnique "cardinal" (HOP.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" HOP.cardinal (HOP.cardinals ∷ TestData ℤ)
    ]
  , testGroup "HRV"
    [ testGroup "unique data"
      [testUnique "cardinal" (HRV.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HAT"
    [ testGroup "unique data"
      [testUnique "cardinal" (HAT.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HUN"
    [ testGroup "unique data"
      [testUnique "cardinal" (HUN.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HUP"
    [ testGroup "unique data"
      [testUnique "cardinal" (HUP.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HUR"
    [ testGroup "unique data"
      [testUnique "cardinal" (HUR.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HYE"
    [ testGroup "unique data"
      [testUnique "cardinal" (HYE.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "IND"
    [ testGroup "unique data"
      [testUnique "cardinal" (IND.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "IBO"
    [ testGroup "unique data"
      [testUnique "cardinal" (IBO.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "INH"
    [ testGroup "unique data"
      [testUnique "cardinal" (INH.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "ITA"
    [ testGroup "unique data"
      [testUnique "cardinal" (ITA.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" ITA.cardinal (ITA.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  ITA.ordinal  (ITA.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "IZH"
    [ testGroup "unique data"
      [testUnique "cardinal" (IZH.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "JPN"
    [ testGroup "preferred"
      [ testGroup "unique data"
        [testUnique "cardinal" (JPN.preferred_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" JPN.preferred_cardinal
                          (JPN.preferred_cardinals ∷ TestData ℤ)
      ]
    , testGroup "kanji"
      [ testGroup "unique data"
        [testUnique "cardinal" (JPN.kanji_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" JPN.kanji_cardinal (JPN.kanji_cardinals ∷ TestData ℤ)
      ]
    , testGroup "daiji"
      [ testGroup "unique data"
        [testUnique "cardinal" (JPN.daiji_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" JPN.daiji_cardinal (JPN.daiji_cardinals ∷ TestData ℤ)
      ]
    ]
  , testGroup "KAP"
    [ testGroup "unique data"
      [testUnique "cardinal" (KAP.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "KEA"
    [ testGroup "unique data"
      [testUnique "cardinal" (KEA.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "KLB"
    [ testGroup "unique data"
      [testUnique "cardinal" (KLB.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "KMR"
    [ testGroup "unique data"
      [testUnique "cardinal" (KMR.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "KRL"
    [ testGroup "unique data"
      [testUnique "cardinal" (KRL.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LAT"
    [ testGroup "unique data"
      [testUnique "cardinal" (LAT.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" LAT.cardinal (LAT.cardinals ∷ TestData ℤ)
    ]
  , testGroup "LTZ"
    [ testGroup "unique data"
      [testUnique "cardinal" (LTZ.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LIV"
    [ testGroup "unique data"
      [testUnique "cardinal" (LIV.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LLD"
    [ testGroup "unique data"
      [testUnique "cardinal" (LLD.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" LLD.cardinal (LLD.cardinals ∷ TestData ℤ)
    ]
  , testGroup "LMO"
    [ testGroup "unique data"
      [testUnique "cardinal" (LMO.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LIN"
    [ testGroup "unique data"
      [testUnique "cardinal" (LIN.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LIT"
    [ testGroup "unique data"
      [testUnique "cardinal" (LIT.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LAV"
    [ testGroup "unique data"
      [testUnique "cardinal" (LAV.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "MLG"
    [ testGroup "unique data"
      [testUnique "cardinal" (MLG.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" MLG.cardinal (MLG.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "MIC"
    [ testGroup "unique data"
      [testUnique "cardinal" (MIC.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "MIN"
    [ testGroup "unique data"
      [testUnique "cardinal" (MIN.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "MNK"
    [ testGroup "unique data"
      [testUnique "cardinal" (MNK.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "MOH"
    [ testGroup "unique data"
      [testUnique "cardinal" (MOH.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "NEE"
    [ testGroup "unique data"
      [testUnique "cardinal" (NEE.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "NEN"
    [ testGroup "unique data"
      [testUnique "cardinal" (NEN.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" NEN.cardinal (NEN.cardinals ∷ TestData ℤ)
    ]
  , testGroup "NLD"
    [ testGroup "unique data"
      [ testUnique "cardinal"       (NLD.cardinals       ∷ TestData ℤ)
      , testUnique "ordinal"        (NLD.ordinals        ∷ TestData ℤ)
      , testUnique "partitive"      (NLD.partitives      ∷ TestData (ℤ, ℤ))
      , testUnique "multiplicative" (NLD.multiplicatives ∷ TestData ℤ)
      ]
    , mkTests "cardinal"       NLD.cardinal  (NLD.cardinals  ∷ TestData ℤ)
    , mkTests "ordinal"        NLD.ordinal   (NLD.ordinals   ∷ TestData ℤ)
    , mkTests "partitive"      NLD.partitive (NLD.partitives ∷ TestData (ℤ, ℤ))
    , mkTests "multiplicative" NLD.multiplicative (NLD.multiplicatives ∷ TestData ℤ)
    ]
  , testGroup "NOB"
    [ testGroup "unique data"
      [testUnique "cardinal" (NOB.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" NOB.cardinal (NOB.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "NQM"
    [ testGroup "unique data"
      [testUnique "cardinal" (NQM.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" NQM.cardinal (NQM.cardinals ∷ TestData ℤ)
    ]
  , testGroup "NAV"
    [ testGroup "unique data"
      [testUnique "cardinal" (NAV.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "OCI"
    [ testGroup "unique data"
      [testUnique "cardinal" (OCI.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "OJI"
    [ testGroup "unique data"
      [testUnique "cardinal" (OJI.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" OJI.cardinal (OJI.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "ONE"
    [ testGroup "unique data"
      [testUnique "cardinal" (ONE.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "ORM"
    [ testGroup "unique data"
      [testUnique "cardinal" (ORM.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "PAA"
    [ testGroup "unique data"
      [testUnique "cardinal" (PAA.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "PDC"
    [ testGroup "unique data"
      [testUnique "cardinal" (PDC.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" PDC.cardinal (PDC.cardinals ∷ TestData ℤ)
    ]
  , testGroup "POL"
    [ testGroup "unique data"
      [testUnique "cardinal" (POL.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" POL.cardinal (POL.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "POR"
    [ testGroup "unique data"
      [testUnique "cardinal" (POR.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" POR.cardinal (POR.cardinals  ∷ TestData ℤ)
    , mkTests "ordinal"  POR.ordinal  (POR.ordinals   ∷ TestData ℤ)
    ]
  , testGroup "RMN_DZA"
    [ testGroup "unique data"
      [testUnique "cardinal" (RMN_DZA.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "RMY_KAL"
    [ testGroup "unique data"
      [testUnique "cardinal" (RMY_KAL.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "RON"
    [ testGroup "unique data"
      [testUnique "cardinal" (RON.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "RUS"
    [ testGroup "unique data"
      [testUnique "cardinal" (RUS.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" RUS.cardinal (RUS.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "SCO"
    [ testGroup "unique data"
      [testUnique "cardinal" (SCO.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" SCO.cardinal (SCO.cardinals ∷ TestData ℤ)
    ]
  , testGroup "SME"
    [ testGroup "unique data"
      [testUnique "cardinal" (SME.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "SMN"
    [ testGroup "unique data"
      [testUnique "cardinal" (SMN.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "SWE"
    [ testGroup "unique data"
      [ testUnique "cardinal" (SWE.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (SWE.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" SWE.cardinal (SWE.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  SWE.ordinal  (SWE.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "TAR"
    [ testGroup "unique data"
      [testUnique "cardinal" (TAR.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "TGS"
    [ testGroup "unique data"
      [testUnique "cardinal" (TGS.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "TUR"
    [ testGroup "unique data"
      [testUnique "cardinal" (TUR.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" TUR.cardinal (TUR.cardinals ∷ TestData ℤ)
    ]
  , testGroup "WMW"
    [ testGroup "unique data"
      [testUnique "cardinal" (WMW.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "WOL"
    [ testGroup "unique data"
      [testUnique "cardinal" (WOL.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" WOL.cardinal (WOL.cardinals ∷ TestData ℤ)
    ]
  , testGroup "XPQ"
    [ testGroup "unique data"
      [testUnique "cardinal" (XPQ.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "YOR"
    [ testGroup "unique data"
      [testUnique "cardinal" (YOR.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" YOR.cardinal (YOR.cardinals ∷ TestData ℤ)
    ]
  , testGroup "ZAI"
    [ testGroup "unique data"
      [testUnique "cardinal" (ZAI.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "ZAQ"
    [ testGroup "unique data"
      [testUnique "cardinal" (ZAQ.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "ZHO"
    [ testGroup "characters (traditional)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.trad_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZHO.trad_cardinal (ZHO.trad_cardinals ∷ TestData ℤ)
      ]
    , testGroup "characters (simplified)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.simpl_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZHO.simpl_cardinal (ZHO.simpl_cardinals ∷ TestData ℤ)
      ]
    , testGroup "financial characters (traditional)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.finance_trad_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZHO.finance_trad_cardinal (ZHO.finance_trad_cardinals ∷ TestData ℤ)
      ]
    , testGroup "financial characters (simplified)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.finance_simpl_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZHO.finance_simpl_cardinal (ZHO.finance_simpl_cardinals ∷ TestData ℤ)
      ]
    , testGroup "pinyin"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZHO.pinyin_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZHO.pinyin_cardinal (ZHO.pinyin_cardinals ∷ TestData ℤ)
      ]
    ]
  , testGroup "ZPC"
    [ testGroup "unique data"
      [testUnique "cardinal" (ZPC.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "ZPL"
    [ testGroup "unique data"
      [testUnique "cardinal" (ZPL.cardinals ∷ TestData ℤ)]
    ]
  ]


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

{-
-- TODO: this takes way to long for large values (10 ^ 60000 - 1)
testBounds ∷ Text → (ℤ → Maybe Text) → (ℤ, ℤ) → Test
testBounds name conversion bounds@(lo, hi) =
      testGroup name
                [ testCase "lower bound" $ assertBool "conversion error" $ checkConv lo
                , testCase "upper bound" $ assertBool "conversion error" $ checkConv hi
                -- , testProperty "in between" $ forAll (choose bounds) checkConv
                ]
    where
      checkConv ∷ ℤ → Bool
      checkConv = isJust ∘ conversion
-}

testInf ∷ (GR.Inflection → GR.Inflection) → (GR.Inflection → Bool) → Assertion
testInf set test = assertBool "False" $ test $ set GR.defaultInflection

intLog_pow10 ∷ Positive ℤ → Bool
intLog_pow10 (Positive x) = x ≡ intLog (dec x)

intLog_pow10m1 ∷ NonNegative ℤ → Bool
intLog_pow10m1 (NonNegative x) = x - 1 ≡ intLog (dec x) - 1

intLog_mul ∷ Positive ℤ → Positive ℤ → Bool
intLog_mul (Positive x) (Positive y) = intLog (dec x ⋅ dec y) ≡ intLog (dec x) + intLog (dec y)

mkTests ∷ ∀ α. (Show α)
        ⇒ String
        → (GR.Inflection → α → Maybe Text)
        → TestData α
        → Test
mkTests name f = testGroup name ∘ map perInflection
  where
    perInflection ∷ (String, GR.Inflection, [(α, Text)]) → Test
    perInflection (infName, inf, xs) = testGroup infName $ map (perValue inf) xs

    perValue ∷ GR.Inflection → (α, Text) → Test
    perValue inf (n, s) = testCase (show n) $ testConversion f inf n s

-- | Tests whether the test data contains duplicates (different
-- numbers with the same representation).
testUnique ∷ ∀ α. (Show α) ⇒ String → TestData α → Test
testUnique name = testGroup name ∘ map perInflection
  where
    perInflection ∷ (String, inf, [(α, Text)]) → Test
    perInflection (infName, _, xs) = testCase infName $ unique xs

    unique ∷ [(α, Text)] → Assertion
    unique xs = when (length ys > 0)
                  $ assertFailure
                  $ intercalate "\n" $ map msg ys
      where
        ys ∷ [[(α, Text)]]
        ys = filter ((1 <) ∘ length)
             $ groupBy ((≡) `on` snd)
             $ sortBy (compare `on` snd) xs

    msg ∷ [(α, Text)] → String
    msg xs = let (_, s) = head xs
             in printf "The string \"%s\" is associated with multiple values: %s"
                       (T.unpack s)
                       (intercalate ", " $ map (show ∘ fst) xs)

testConversion ∷ (Show α)
               ⇒ (GR.Inflection → α → Maybe Text)
               → GR.Inflection
               → α
               → Text
               → Assertion
testConversion f inf n s =
  let r = f inf n
  in when (r ≢ Just s)
        $ assertFailure
        $ printf "Expected %s = \"%s\" but got \"%s\""
                 (show n)
                 (T.unpack s)
                 (T.unpack $ fromMaybe "no result" r)
