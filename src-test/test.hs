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

import qualified "numerals" Text.Numeral.Language.AF  as AF
import qualified "numerals" Text.Numeral.Language.AMP as AMP
import qualified "numerals" Text.Numeral.Language.BG  as BG
import qualified "numerals" Text.Numeral.Language.CHN as CHN
import qualified "numerals" Text.Numeral.Language.CHR as CHR
import qualified "numerals" Text.Numeral.Language.CLM as CLM
import qualified "numerals" Text.Numeral.Language.CS  as CS
import qualified "numerals" Text.Numeral.Language.DE  as DE
import qualified "numerals" Text.Numeral.Language.EN  as EN
import qualified "numerals" Text.Numeral.Language.EO  as EO
import qualified "numerals" Text.Numeral.Language.ES  as ES
import qualified "numerals" Text.Numeral.Language.FI  as FI
import qualified "numerals" Text.Numeral.Language.FR  as FR
import qualified "numerals" Text.Numeral.Language.FUR as FUR
import qualified "numerals" Text.Numeral.Language.GSW as GSW
import qualified "numerals" Text.Numeral.Language.GV  as GV
import qualified "numerals" Text.Numeral.Language.HE  as HE
import qualified "numerals" Text.Numeral.Language.HOP as HOP
import qualified "numerals" Text.Numeral.Language.IT  as IT
import qualified "numerals" Text.Numeral.Language.JA  as JA
import qualified "numerals" Text.Numeral.Language.LA  as LA
import qualified "numerals" Text.Numeral.Language.LLD as LLD
import qualified "numerals" Text.Numeral.Language.MG  as MG
import qualified "numerals" Text.Numeral.Language.NEN as NEN
import qualified "numerals" Text.Numeral.Language.NL  as NL
import qualified "numerals" Text.Numeral.Language.NO  as NO
import qualified "numerals" Text.Numeral.Language.NQM as NQM
import qualified "numerals" Text.Numeral.Language.OJ  as OJ
import qualified "numerals" Text.Numeral.Language.PDC as PDC
import qualified "numerals" Text.Numeral.Language.PL  as PL
import qualified "numerals" Text.Numeral.Language.PT  as PT
import qualified "numerals" Text.Numeral.Language.RU  as RU
import qualified "numerals" Text.Numeral.Language.SCO as SCO
import qualified "numerals" Text.Numeral.Language.SV  as SV
import qualified "numerals" Text.Numeral.Language.TR  as TR
import qualified "numerals" Text.Numeral.Language.WO  as WO
import qualified "numerals" Text.Numeral.Language.YOR as YOR
import qualified "numerals" Text.Numeral.Language.ZH  as ZH

import qualified "this" Text.Numeral.Language.ACH.TestData as ACH
import qualified "this" Text.Numeral.Language.ADY.TestData as ADY
import qualified "this" Text.Numeral.Language.AF.TestData  as AF
import qualified "this" Text.Numeral.Language.AMP.TestData as AMP
import qualified "this" Text.Numeral.Language.ARI.TestData as ARI
import qualified "this" Text.Numeral.Language.ARN.TestData as ARN
import qualified "this" Text.Numeral.Language.AST.TestData as AST
import qualified "this" Text.Numeral.Language.AZ.TestData  as AZ
import qualified "this" Text.Numeral.Language.BA.TestData  as BA
import qualified "this" Text.Numeral.Language.BG.TestData  as BG
import qualified "this" Text.Numeral.Language.BM.TestData  as BM
import qualified "this" Text.Numeral.Language.CA.TestData  as CA
import qualified "this" Text.Numeral.Language.CAF.TestData as CAF
import qualified "this" Text.Numeral.Language.CAR.TestData as CAR
import qualified "this" Text.Numeral.Language.CBK.TestData as CBK
import qualified "this" Text.Numeral.Language.CHN.TestData as CHN
import qualified "this" Text.Numeral.Language.CHR.TestData as CHR
import qualified "this" Text.Numeral.Language.CKU.TestData as CKU
import qualified "this" Text.Numeral.Language.CLM.TestData as CLM
import qualified "this" Text.Numeral.Language.CO.TestData  as CO
import qualified "this" Text.Numeral.Language.COD.TestData as COD
import qualified "this" Text.Numeral.Language.COO.TestData as COO
import qualified "this" Text.Numeral.Language.CRG.TestData as CRG
import qualified "this" Text.Numeral.Language.CS.TestData  as CS
import qualified "this" Text.Numeral.Language.CY.TestData  as CY
import qualified "this" Text.Numeral.Language.DE.TestData  as DE
import qualified "this" Text.Numeral.Language.DJK.TestData as DJK
import qualified "this" Text.Numeral.Language.EMI.TestData as EMI
import qualified "this" Text.Numeral.Language.EN.TestData  as EN
import qualified "this" Text.Numeral.Language.EO.TestData  as EO
import qualified "this" Text.Numeral.Language.ES.TestData  as ES
import qualified "this" Text.Numeral.Language.ET.TestData  as ET
import qualified "this" Text.Numeral.Language.FI.TestData  as FI
import qualified "this" Text.Numeral.Language.FO.TestData  as FO
import qualified "this" Text.Numeral.Language.FR.TestData  as FR
import qualified "this" Text.Numeral.Language.FRA_JER.TestData as FRA_JER
import qualified "this" Text.Numeral.Language.FUR.TestData as FUR
import qualified "this" Text.Numeral.Language.GCF_MTQ.TestData as GCF_MTQ
import qualified "this" Text.Numeral.Language.GIL.TestData as GIL
import qualified "this" Text.Numeral.Language.GL.TestData  as GL
import qualified "this" Text.Numeral.Language.GSW.TestData as GSW
import qualified "this" Text.Numeral.Language.GV.TestData  as GV
import qualified "this" Text.Numeral.Language.HAI.TestData as HAI
import qualified "this" Text.Numeral.Language.HE.TestData  as HE
import qualified "this" Text.Numeral.Language.HOP.TestData as HOP
import qualified "this" Text.Numeral.Language.HR.TestData  as HR
import qualified "this" Text.Numeral.Language.HT.TestData  as HT
import qualified "this" Text.Numeral.Language.HU.TestData  as HU
import qualified "this" Text.Numeral.Language.HUP.TestData as HUP
import qualified "this" Text.Numeral.Language.HUR.TestData as HUR
import qualified "this" Text.Numeral.Language.HY.TestData  as HY
import qualified "this" Text.Numeral.Language.ID.TestData  as ID
import qualified "this" Text.Numeral.Language.IG.TestData  as IG
import qualified "this" Text.Numeral.Language.INH.TestData as INH
import qualified "this" Text.Numeral.Language.IT.TestData  as IT
import qualified "this" Text.Numeral.Language.IZH.TestData as IZH
import qualified "this" Text.Numeral.Language.JA.TestData  as JA
import qualified "this" Text.Numeral.Language.KAP.TestData as KAP
import qualified "this" Text.Numeral.Language.KEA.TestData as KEA
import qualified "this" Text.Numeral.Language.KLB.TestData as KLB
import qualified "this" Text.Numeral.Language.KRL.TestData as KRL
import qualified "this" Text.Numeral.Language.LA.TestData  as LA
import qualified "this" Text.Numeral.Language.LB.TestData  as LB
import qualified "this" Text.Numeral.Language.LIV.TestData as LIV
import qualified "this" Text.Numeral.Language.LLD.TestData as LLD
import qualified "this" Text.Numeral.Language.LMO.TestData as LMO
import qualified "this" Text.Numeral.Language.LN.TestData  as LN
import qualified "this" Text.Numeral.Language.LT.TestData  as LT
import qualified "this" Text.Numeral.Language.LV.TestData  as LV
import qualified "this" Text.Numeral.Language.MG.TestData  as MG
import qualified "this" Text.Numeral.Language.MIC.TestData as MIC
import qualified "this" Text.Numeral.Language.MIN.TestData as MIN
import qualified "this" Text.Numeral.Language.MNK.TestData as MNK
import qualified "this" Text.Numeral.Language.MOH.TestData as MOH
import qualified "this" Text.Numeral.Language.NEE.TestData as NEE
import qualified "this" Text.Numeral.Language.NEN.TestData as NEN
import qualified "this" Text.Numeral.Language.NL.TestData  as NL
import qualified "this" Text.Numeral.Language.NO.TestData  as NO
import qualified "this" Text.Numeral.Language.NQM.TestData as NQM
import qualified "this" Text.Numeral.Language.NV.TestData  as NV
import qualified "this" Text.Numeral.Language.OJ.TestData  as OJ
import qualified "this" Text.Numeral.Language.PAA.TestData as PAA
import qualified "this" Text.Numeral.Language.PDC.TestData as PDC
import qualified "this" Text.Numeral.Language.PL.TestData  as PL
import qualified "this" Text.Numeral.Language.PT.TestData  as PT
import qualified "this" Text.Numeral.Language.RMN_DZA.TestData as RMN_DZA
import qualified "this" Text.Numeral.Language.RMY_KAL.TestData as RMY_KAL
import qualified "this" Text.Numeral.Language.RO.TestData  as RO
import qualified "this" Text.Numeral.Language.RU.TestData  as RU
import qualified "this" Text.Numeral.Language.SCO.TestData as SCO
import qualified "this" Text.Numeral.Language.SMN.TestData as SMN
import qualified "this" Text.Numeral.Language.SV.TestData  as SV
import qualified "this" Text.Numeral.Language.TAR.TestData as TAR
import qualified "this" Text.Numeral.Language.TR.TestData  as TR
import qualified "this" Text.Numeral.Language.WMW.TestData as WMW
import qualified "this" Text.Numeral.Language.WO.TestData  as WO
import qualified "this" Text.Numeral.Language.XPQ.TestData as XPQ
import qualified "this" Text.Numeral.Language.YOR.TestData as YOR
import qualified "this" Text.Numeral.Language.ZAI.TestData as ZAI
import qualified "this" Text.Numeral.Language.ZAQ.TestData as ZAQ
import qualified "this" Text.Numeral.Language.ZH.TestData  as ZH
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
      [ testUnique "cardinal" (AF.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (AF.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" AF.cardinal (AF.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  AF.ordinal  (AF.ordinals  ∷ TestData ℤ)
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
  , testGroup "AZ"
    [ testGroup "unique data"
      [testUnique "cardinal" (AZ.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "BA"
    [ testGroup "unique data"
      [testUnique "cardinal" (BA.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "BG"
    [ testGroup "unique data"
      [testUnique "cardinal" (BG.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" BG.cardinal (BG.cardinals ∷ TestData ℤ)
    ]
  , testGroup "BM"
    [ testGroup "unique data"
      [testUnique "cardinal" (BM.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "CA"
    [ testGroup "unique data"
      [testUnique "cardinal" (CA.cardinals ∷ TestData ℤ)]
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
  , testGroup "CO"
    [ testGroup "unique data"
      [testUnique "cardinal" (CO.cardinals ∷ TestData ℤ)]
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
  , testGroup "CS"
    [ testGroup "unique data"
      [testUnique "cardinal" (CS.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" CS.cardinal (CS.cardinals ∷ TestData ℤ)
    ]
  , testGroup "CY"
    [ testGroup "unique data"
      [testUnique "cardinal" (CY.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "DE"
    [ testGroup "unique data"
      [ testUnique "cardinal" (DE.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (DE.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" DE.cardinal (DE.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  DE.ordinal  (DE.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "DJK"
    [ testGroup "unique data"
      [testUnique "cardinal" (DJK.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "EMI"
    [ testGroup "unique data"
      [testUnique "cardinal" (EMI.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "EN"
    [ testGroup "GB"
      [ testGroup "unique data"
        [ testUnique "cardinal" (EN.gb_cardinals ∷ TestData ℤ)
        , testUnique "ordinal"  (EN.gb_ordinals  ∷ TestData ℤ)
        ]
      , mkTests "cardinal" EN.gb_cardinal (EN.gb_cardinals ∷ TestData ℤ)
      , mkTests "ordinal"  EN.gb_ordinal  (EN.gb_ordinals  ∷ TestData ℤ)
      ]
    , testGroup "US"
      [ testGroup "unique data"
        [ testUnique "cardinal" (EN.us_cardinals ∷ TestData ℤ)
        , testUnique "ordinal"  (EN.us_ordinals  ∷ TestData ℤ)
        ]
      , mkTests "cardinal" EN.us_cardinal (EN.us_cardinals ∷ TestData ℤ)
      , mkTests "ordinal"  EN.us_ordinal  (EN.us_ordinals  ∷ TestData ℤ)
      ]
    ]
  , testGroup "EO"
    [ testGroup "unique data"
      [testUnique "cardinal" (EO.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" EO.cardinal (EO.cardinals ∷ TestData ℤ)
    ]
  , testGroup "ES"
    [ testGroup "unique data"
      [testUnique "cardinal" (ES.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" ES.cardinal (ES.cardinals ∷ TestData ℤ)
    ]
  , testGroup "ET"
    [ testGroup "unique data"
      [testUnique "cardinal" (ET.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "FI"
    [ testGroup "unique data"
      [ testUnique "cardinal" (FI.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (FI.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" FI.cardinal (FI.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  FI.ordinal  (FI.ordinals ∷ TestData ℤ)
    ]
  , testGroup "FO"
    [ testGroup "unique data"
      [testUnique "cardinal" (FO.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "FR"
    [ testGroup "unique data"
      [ testUnique "cardinal" (FR.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (FR.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" FR.cardinal (FR.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  FR.ordinal  (FR.ordinals  ∷ TestData ℤ)
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
  , testGroup "GL"
    [ testGroup "unique data"
      [testUnique "cardinal" (GL.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "GSW"
    [ testGroup "unique data"
      [testUnique "cardinal" (GSW.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" GSW.cardinal (GSW.cardinals ∷ TestData ℤ)
    ]
  , testGroup "GV"
    [ testGroup "unique data"
      [testUnique "cardinal" (GV.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" GV.cardinal  (GV.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "HAI"
    [ testGroup "unique data"
      [testUnique "cardinal" (HAI.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HE"
    [ testGroup "unique data"
      [testUnique "cardinal" (HE.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" HE.cardinal  (HE.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "HOP"
    [ testGroup "unique data"
      [testUnique "cardinal" (HOP.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" HOP.cardinal (HOP.cardinals ∷ TestData ℤ)
    ]
  , testGroup "HR"
    [ testGroup "unique data"
      [testUnique "cardinal" (HR.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HT"
    [ testGroup "unique data"
      [testUnique "cardinal" (HT.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HU"
    [ testGroup "unique data"
      [testUnique "cardinal" (HU.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HUP"
    [ testGroup "unique data"
      [testUnique "cardinal" (HUP.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HUR"
    [ testGroup "unique data"
      [testUnique "cardinal" (HUR.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "HY"
    [ testGroup "unique data"
      [testUnique "cardinal" (HY.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "ID"
    [ testGroup "unique data"
      [testUnique "cardinal" (ID.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "IG"
    [ testGroup "unique data"
      [testUnique "cardinal" (IG.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "INH"
    [ testGroup "unique data"
      [testUnique "cardinal" (INH.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "IT"
    [ testGroup "unique data"
      [testUnique "cardinal" (IT.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" IT.cardinal (IT.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  IT.ordinal  (IT.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "IZH"
    [ testGroup "unique data"
      [testUnique "cardinal" (IZH.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "JA"
    [ testGroup "preferred"
      [ testGroup "unique data"
        [testUnique "cardinal" (JA.preferred_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" JA.preferred_cardinal
                          (JA.preferred_cardinals ∷ TestData ℤ)
      ]
    , testGroup "kanji"
      [ testGroup "unique data"
        [testUnique "cardinal" (JA.kanji_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" JA.kanji_cardinal (JA.kanji_cardinals ∷ TestData ℤ)
      ]
    , testGroup "daiji"
      [ testGroup "unique data"
        [testUnique "cardinal" (JA.daiji_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" JA.daiji_cardinal (JA.daiji_cardinals ∷ TestData ℤ)
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
  , testGroup "KRL"
    [ testGroup "unique data"
      [testUnique "cardinal" (KRL.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LA"
    [ testGroup "unique data"
      [testUnique "cardinal" (LA.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" LA.cardinal (LA.cardinals ∷ TestData ℤ)
    ]
  , testGroup "LB"
    [ testGroup "unique data"
      [testUnique "cardinal" (LB.cardinals ∷ TestData ℤ)]
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
  , testGroup "LN"
    [ testGroup "unique data"
      [testUnique "cardinal" (LN.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LT"
    [ testGroup "unique data"
      [testUnique "cardinal" (LT.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LV"
    [ testGroup "unique data"
      [testUnique "cardinal" (LV.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "MG"
    [ testGroup "unique data"
      [testUnique "cardinal" (MG.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" MG.cardinal (MG.cardinals  ∷ TestData ℤ)
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
  , testGroup "NL"
    [ testGroup "unique data"
      [ testUnique "cardinal"       (NL.cardinals       ∷ TestData ℤ)
      , testUnique "ordinal"        (NL.ordinals        ∷ TestData ℤ)
      , testUnique "partitive"      (NL.partitives      ∷ TestData (ℤ, ℤ))
      , testUnique "multiplicative" (NL.multiplicatives ∷ TestData ℤ)
      ]
    , mkTests "cardinal"       NL.cardinal  (NL.cardinals  ∷ TestData ℤ)
    , mkTests "ordinal"        NL.ordinal   (NL.ordinals   ∷ TestData ℤ)
    , mkTests "partitive"      NL.partitive (NL.partitives ∷ TestData (ℤ, ℤ))
    , mkTests "multiplicative" NL.multiplicative (NL.multiplicatives ∷ TestData ℤ)
    ]
  , testGroup "NO"
    [ testGroup "unique data"
      [testUnique "cardinal" (NO.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" NO.cardinal  (NO.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "NQM"
    [ testGroup "unique data"
      [testUnique "cardinal" (NQM.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" NQM.cardinal (NQM.cardinals ∷ TestData ℤ)
    ]
  , testGroup "NV"
    [ testGroup "unique data"
      [testUnique "cardinal" (NV.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "OJ"
    [ testGroup "unique data"
      [testUnique "cardinal" (OJ.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" OJ.cardinal  (OJ.cardinals  ∷ TestData ℤ)
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
  , testGroup "PL"
    [ testGroup "unique data"
      [testUnique "cardinal" (PL.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" PL.cardinal  (PL.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "PT"
    [ testGroup "unique data"
      [testUnique "cardinal" (PT.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" PT.cardinal (PT.cardinals  ∷ TestData ℤ)
    , mkTests "ordinal"  PT.ordinal  (PT.ordinals   ∷ TestData ℤ)
    ]
  , testGroup "RMN_DZA"
    [ testGroup "unique data"
      [testUnique "cardinal" (RMN_DZA.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "RMY_KAL"
    [ testGroup "unique data"
      [testUnique "cardinal" (RMY_KAL.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "RO"
    [ testGroup "unique data"
      [testUnique "cardinal" (RO.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "RU"
    [ testGroup "unique data"
      [testUnique "cardinal" (RU.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" RU.cardinal  (RU.cardinals  ∷ TestData ℤ)
    ]
  , testGroup "SCO"
    [ testGroup "unique data"
      [testUnique "cardinal" (SCO.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" SCO.cardinal (SCO.cardinals ∷ TestData ℤ)
    ]
  , testGroup "SMN"
    [ testGroup "unique data"
      [testUnique "cardinal" (SMN.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "SV"
    [ testGroup "unique data"
      [ testUnique "cardinal" (SV.cardinals ∷ TestData ℤ)
      , testUnique "ordinal"  (SV.ordinals  ∷ TestData ℤ)
      ]
    , mkTests "cardinal" SV.cardinal (SV.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  SV.ordinal  (SV.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "TAR"
    [ testGroup "unique data"
      [testUnique "cardinal" (TAR.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "TR"
    [ testGroup "unique data"
      [testUnique "cardinal" (TR.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" TR.cardinal (TR.cardinals ∷ TestData ℤ)
    ]
  , testGroup "WMW"
    [ testGroup "unique data"
      [testUnique "cardinal" (WMW.cardinals ∷ TestData ℤ)]
    ]
  , testGroup "WO"
    [ testGroup "unique data"
      [testUnique "cardinal" (WO.cardinals ∷ TestData ℤ)]
    , mkTests "cardinal" WO.cardinal (WO.cardinals ∷ TestData ℤ)
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
  , testGroup "ZH"
    [ testGroup "characters (traditional)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZH.trad_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZH.trad_cardinal (ZH.trad_cardinals ∷ TestData ℤ)
      ]
    , testGroup "characters (simplified)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZH.simpl_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZH.simpl_cardinal (ZH.simpl_cardinals ∷ TestData ℤ)
      ]
    , testGroup "financial characters (traditional)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZH.finance_trad_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZH.finance_trad_cardinal (ZH.finance_trad_cardinals ∷ TestData ℤ)
      ]
    , testGroup "financial characters (simplified)"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZH.finance_simpl_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZH.finance_simpl_cardinal (ZH.finance_simpl_cardinals ∷ TestData ℤ)
      ]
    , testGroup "pinyin"
      [ testGroup "unique data"
        [testUnique "cardinal" (ZH.pinyin_cardinals ∷ TestData ℤ)]
      , mkTests "cardinal" ZH.pinyin_cardinal (ZH.pinyin_cardinals ∷ TestData ℤ)
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
