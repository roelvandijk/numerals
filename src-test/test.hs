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
import "base" Data.Function      ( ($) )
import "base" Data.List          ( map )
import "base" Data.Maybe         ( Maybe(Just), fromMaybe )
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
import qualified "numerals" Text.Numeral.Language.NL  as NL
import qualified "numerals" Text.Numeral.Language.NO  as NO
import qualified "numerals" Text.Numeral.Language.NQM as NQM
import qualified "numerals" Text.Numeral.Language.OJ  as OJ
-- import qualified "numerals" Text.Numeral.Language.PAA as PAA
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

import qualified "this" Text.Numeral.Language.AMP.TestData as AMP
import qualified "this" Text.Numeral.Language.AF.TestData  as AF
import qualified "this" Text.Numeral.Language.BG.TestData  as BG
import qualified "this" Text.Numeral.Language.CHN.TestData as CHN
import qualified "this" Text.Numeral.Language.CHR.TestData as CHR
import qualified "this" Text.Numeral.Language.CLM.TestData as CLM
import qualified "this" Text.Numeral.Language.CS.TestData  as CS
-- import qualified "this" Text.Numeral.Language.CY.TestData  as CY
-- import qualified "this" Text.Numeral.Language.DA.TestData  as DA
import qualified "this" Text.Numeral.Language.DE.TestData  as DE
import qualified "this" Text.Numeral.Language.EN.TestData  as EN
import qualified "this" Text.Numeral.Language.EO.TestData  as EO
import qualified "this" Text.Numeral.Language.ES.TestData  as ES
-- import qualified "this" Text.Numeral.Language.ET.TestData  as ET
import qualified "this" Text.Numeral.Language.FI.TestData  as FI
import qualified "this" Text.Numeral.Language.FR.TestData  as FR
import qualified "this" Text.Numeral.Language.FUR.TestData as FUR
import qualified "this" Text.Numeral.Language.GSW.TestData as GSW
import qualified "this" Text.Numeral.Language.GV.TestData  as GV
import qualified "this" Text.Numeral.Language.HE.TestData  as HE
import qualified "this" Text.Numeral.Language.HOP.TestData as HOP
-- import qualified "this" Text.Numeral.Language.HR.TestData  as HR
-- import qualified "this" Text.Numeral.Language.HU.TestData  as HU
import qualified "this" Text.Numeral.Language.IT.TestData  as IT
import qualified "this" Text.Numeral.Language.JA.TestData  as JA
import qualified "this" Text.Numeral.Language.LA.TestData  as LA
import qualified "this" Text.Numeral.Language.LLD.TestData as LLD
import qualified "this" Text.Numeral.Language.MG.TestData  as MG
import qualified "this" Text.Numeral.Language.NL.TestData  as NL
import qualified "this" Text.Numeral.Language.NO.TestData  as NO
import qualified "this" Text.Numeral.Language.NQM.TestData as NQM
import qualified "this" Text.Numeral.Language.OJ.TestData  as OJ
-- import qualified "this" Text.Numeral.Language.PAA.TestData as PAA
import qualified "this" Text.Numeral.Language.PDC.TestData as PDC
import qualified "this" Text.Numeral.Language.PL.TestData  as PL
import qualified "this" Text.Numeral.Language.PT.TestData  as PT
-- import qualified "this" Text.Numeral.Language.RO.TestData  as RO
import qualified "this" Text.Numeral.Language.RU.TestData  as RU
import qualified "this" Text.Numeral.Language.SCO.TestData as SCO
import qualified "this" Text.Numeral.Language.SV.TestData  as SV
import qualified "this" Text.Numeral.Language.TR.TestData  as TR
import qualified "this" Text.Numeral.Language.WO.TestData  as WO
import qualified "this" Text.Numeral.Language.YOR.TestData as YOR
import qualified "this" Text.Numeral.Language.ZH.TestData  as ZH


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
  , testGroup "AF"
    [ mkTests "cardinal" AF.cardinal (AF.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  AF.ordinal  (AF.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "AMP" [mkTests "cardinal" AMP.cardinal (AMP.cardinals ∷ TestData ℤ)]
  , testGroup "BG"  [mkTests "cardinal" BG.cardinal  (BG.cardinals  ∷ TestData ℤ)]
  , testGroup "CHN" [mkTests "cardinal" CHN.cardinal (CHN.cardinals ∷ TestData ℤ)]
  , testGroup "CHR" [mkTests "cardinal" CHR.cardinal (CHR.cardinals ∷ TestData ℤ)]
  , testGroup "CLM" [mkTests "cardinal" CLM.cardinal (CLM.cardinals ∷ TestData ℤ)]
  , testGroup "CS"  [mkTests "cardinal" CS.cardinal  (CS.cardinals  ∷ TestData ℤ)]
  , testGroup "DE"
    [ mkTests "cardinal" DE.cardinal (DE.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  DE.ordinal  (DE.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "EN"
    [ testGroup "GB"
      [ mkTests "cardinal" EN.gb_cardinal (EN.gb_cardinals ∷ TestData ℤ)
      , mkTests "ordinal"  EN.gb_ordinal  (EN.gb_ordinals  ∷ TestData ℤ)
      ]
    , testGroup "US"
      [ mkTests "cardinal" EN.us_cardinal (EN.us_cardinals ∷ TestData ℤ)
      , mkTests "ordinal"  EN.us_ordinal  (EN.us_ordinals  ∷ TestData ℤ)
      ]
    ]
  , testGroup "EO"  [mkTests "cardinal" EO.cardinal (EO.cardinals ∷ TestData ℤ)]
  , testGroup "ES"  [mkTests "cardinal" ES.cardinal (ES.cardinals ∷ TestData ℤ)]
  , testGroup "FI"
    [ mkTests "cardinal" FI.cardinal (FI.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  FI.ordinal  (FI.ordinals ∷ TestData ℤ)
    ]
  , testGroup "FR"
    [ mkTests "cardinal" FR.cardinal (FR.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  FR.ordinal  (FR.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "FUR" [mkTests "cardinal" FUR.cardinal (FUR.cardinals ∷ TestData ℤ)]
  , testGroup "GSW" [mkTests "cardinal" GSW.cardinal (GSW.cardinals ∷ TestData ℤ)]
  , testGroup "GV"  [mkTests "cardinal" GV.cardinal  (GV.cardinals  ∷ TestData ℤ)]
  , testGroup "HE"  [mkTests "cardinal" HE.cardinal  (HE.cardinals  ∷ TestData ℤ)]
  , testGroup "HOP" [mkTests "cardinal" HOP.cardinal (HOP.cardinals ∷ TestData ℤ)]
  , testGroup "IT"
    [ mkTests "cardinal" IT.cardinal (IT.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  IT.ordinal  (IT.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "JA"
    [ testGroup "preferred"
      [mkTests "cardinal" JA.preferred_cardinal
                          (JA.preferred_cardinals ∷ TestData ℤ)
      ]
    , testGroup "kanji"
      [mkTests "cardinal" JA.kanji_cardinal (JA.kanji_cardinals ∷ TestData ℤ)]
    , testGroup "daiji"
      [mkTests "cardinal" JA.daiji_cardinal (JA.daiji_cardinals ∷ TestData ℤ)]
    ]
  , testGroup "LA"  [mkTests "cardinal" LA.cardinal  (LA.cardinals  ∷ TestData ℤ)]
  , testGroup "LLD" [mkTests "cardinal" LLD.cardinal (LLD.cardinals ∷ TestData ℤ)]
  , testGroup "MG"  [mkTests "cardinal" MG.cardinal  (MG.cardinals  ∷ TestData ℤ)]
  , testGroup "NL"
    [ mkTests "cardinal"       NL.cardinal  (NL.cardinals  ∷ TestData ℤ)
    , mkTests "ordinal"        NL.ordinal   (NL.ordinals   ∷ TestData ℤ)
    , mkTests "partitive"      NL.partitive (NL.partitives ∷ TestData (ℤ, ℤ))
    , mkTests "multiplicative" NL.multiplicative (NL.multiplicatives ∷ TestData ℤ)
    ]
  , testGroup "NO"  [mkTests "cardinal" NO.cardinal  (NO.cardinals  ∷ TestData ℤ)]
  , testGroup "NQM" [mkTests "cardinal" NQM.cardinal (NQM.cardinals ∷ TestData ℤ)]
  , testGroup "OJ"  [mkTests "cardinal" OJ.cardinal  (OJ.cardinals  ∷ TestData ℤ)]
  -- , testGroup "PAA" [mkTests "cardinal" PAA.cardinal (PAA.cardinals ∷ TestData ℤ)]
  , testGroup "PDC" [mkTests "cardinal" PDC.cardinal (PDC.cardinals ∷ TestData ℤ)]
  , testGroup "PL"  [mkTests "cardinal" PL.cardinal  (PL.cardinals  ∷ TestData ℤ)]
  , testGroup "PT"  [ mkTests "cardinal" PT.cardinal (PT.cardinals  ∷ TestData ℤ)
                    , mkTests "ordinal"  PT.ordinal  (PT.ordinals   ∷ TestData ℤ)
                    ]
  , testGroup "RU"  [mkTests "cardinal" RU.cardinal  (RU.cardinals  ∷ TestData ℤ)]
  , testGroup "SCO" [mkTests "cardinal" SCO.cardinal (SCO.cardinals ∷ TestData ℤ)]
  , testGroup "SV"
    [ mkTests "cardinal" SV.cardinal  (SV.cardinals ∷ TestData ℤ)
    , mkTests "ordinal"  SV.ordinal   (SV.ordinals  ∷ TestData ℤ)
    ]
  , testGroup "TR"  [mkTests "cardinal" TR.cardinal  (TR.cardinals  ∷ TestData ℤ)]
  , testGroup "WO"  [mkTests "cardinal" WO.cardinal  (WO.cardinals  ∷ TestData ℤ)]
  , testGroup "YOR" [mkTests "cardinal" YOR.cardinal (YOR.cardinals ∷ TestData ℤ)]
  , testGroup "ZH"
    [ testGroup "characters (traditional)"
      [mkTests "cardinal" ZH.trad_cardinal (ZH.trad_cardinals ∷ TestData ℤ)]
    , testGroup "characters (simplified)"
      [mkTests "cardinal" ZH.simpl_cardinal (ZH.simpl_cardinals ∷ TestData ℤ)]
    , testGroup "financial characters (traditional)"
      [mkTests "cardinal" ZH.finance_trad_cardinal (ZH.finance_trad_cardinals ∷ TestData ℤ)]
    , testGroup "financial characters (simplified)"
      [mkTests "cardinal" ZH.finance_simpl_cardinal (ZH.finance_simpl_cardinals ∷ TestData ℤ)]
    , testGroup "pinyin"
      [mkTests "cardinal" ZH.pinyin_cardinal (ZH.pinyin_cardinals ∷ TestData ℤ)]
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

mkTests ∷ ∀ α
        . (Show α)
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
