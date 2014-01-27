{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE PackageImports    #-}

module Text.Numeral.Grammar.Reified
    ( -- * Inflection
      Inflection(..)
    , defaultInflection
      -- * Grammatical categories
    , Case(..)
    , Gender(..)
    , Number(..)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool     ( Bool(False, True) )
import "base" Data.Eq       ( Eq )
import "base" Data.Function ( ($) )
import "base" Data.Maybe    ( Maybe(Nothing, Just) )
import "base" Text.Show     ( Show )
import "base-unicode-symbols" Data.Eq.Unicode ( (≡) )
import qualified "this" Text.Numeral.Grammar as G


-------------------------------------------------------------------------------
-- Inflection
-------------------------------------------------------------------------------

data Inflection = Inflection { iCase   ∷ Maybe Case
                             , iGender ∷ Maybe Gender
                             , iNumber ∷ Maybe Number
                             } deriving (Show, Eq)

defaultInflection ∷ Inflection
defaultInflection = Inflection Nothing Nothing Nothing


-------------------------------------------------------------------------------
-- Grammatical categories
-------------------------------------------------------------------------------

data Case = Abessive
          | Ablative
          | Accusative
          | Comitative
          | Dative
          | Delative
          | Distributive
          | DistributiveTemporal
          | Essive
          | Genitive
          | Instrumental
          | Instructive
          | Lative
          | Locative (Maybe Locative)
          | Multiplicative
          | Nominative
          | Partitive
          | Sublative
          | SuperEssive
          | Translative
          | Vocative
            deriving (Eq, Show)

data Locative = LocativeIllative
              | LocativeInessive
              | LocativeElative
              | LocativeAllative
              | LocativeAdessive
              | LocativeAblative
                deriving (Eq, Show)

data Gender = Neuter
            | Masculine
            | Feminine
            | Common
              deriving (Eq, Show)

data Number = Singular
            | Dual
            | Trial
            | Paucal
            | Plural
              deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Case instances
-------------------------------------------------------------------------------

instance G.NoCase Inflection where
    noCase inf = inf { iCase = Nothing }
    hasNoCase (Inflection {iCase}) = iCase ≡ Nothing

instance G.Ablative Inflection where
    ablative inf = inf { iCase = Just Ablative }
    isAblative (Inflection {iCase}) = iCase ≡ Just Ablative

instance G.Abessive Inflection where
    abessive inf = inf { iCase = Just Abessive }
    isAbessive (Inflection {iCase}) = iCase ≡ Just Abessive

instance G.Accusative Inflection where
    accusative inf = inf { iCase = Just Accusative }
    isAccusative (Inflection {iCase}) = iCase ≡ Just Accusative

instance G.Comitative Inflection where
    comitative inf = inf { iCase = Just Comitative }
    isComitative (Inflection {iCase}) = iCase ≡ Just Comitative

instance G.Dative Inflection where
    dative inf = inf { iCase = Just Dative }
    isDative (Inflection {iCase}) = iCase ≡ Just Dative

instance G.Delative Inflection where
    delative inf = inf { iCase = Just Delative }
    isDelative (Inflection {iCase}) = iCase ≡ Just Delative

instance G.Distributive Inflection where
    distributive inf = inf { iCase = Just Distributive }
    isDistributive (Inflection {iCase}) = iCase ≡ Just Distributive

instance G.DistributiveTemporal Inflection where
    distributiveTemporal inf = inf { iCase = Just DistributiveTemporal }
    isDistributiveTemporal (Inflection {iCase}) = iCase ≡ Just DistributiveTemporal

instance G.Essive Inflection where
    essive inf = inf { iCase = Just Essive }
    isEssive (Inflection {iCase}) = iCase ≡ Just Essive

instance G.Genitive Inflection where
    genitive inf = inf { iCase = Just Genitive }
    isGenitive (Inflection {iCase}) = iCase ≡ Just Genitive

instance G.Instrumental Inflection where
    instrumental inf = inf { iCase = Just Instrumental }
    isInstrumental (Inflection {iCase}) = iCase ≡ Just Instrumental

instance G.Instructive Inflection where
    instructive inf = inf { iCase = Just Instructive }
    isInstructive (Inflection {iCase}) = iCase ≡ Just Instructive

instance G.Lative Inflection where
    lative inf = inf { iCase = Just Lative }
    isLative (Inflection {iCase}) = iCase ≡ Just Lative

instance G.Locative Inflection where
    locative inf = inf { iCase = Just (Locative Nothing) }
    isLocative (Inflection {iCase}) =
        case iCase of
          Just (Locative _) → True
          _                 → False

instance G.Multiplicative Inflection where
    multiplicative inf = inf { iCase = Just Multiplicative }
    isMultiplicative (Inflection {iCase}) = iCase ≡ Just Multiplicative

instance G.Nominative Inflection where
    nominative inf = inf { iCase = Just Nominative }
    isNominative (Inflection {iCase}) = iCase ≡ Just Nominative

instance G.Partitive Inflection where
    partitive inf = inf { iCase = Just Partitive }
    isPartitive (Inflection {iCase}) = iCase ≡ Just Partitive

instance G.SuperEssive Inflection where
    superEssive inf = inf { iCase = Just SuperEssive }
    isSuperEssive (Inflection {iCase}) = iCase ≡ Just SuperEssive

instance G.Sublative Inflection where
    sublative inf = inf { iCase = Just Sublative }
    isSublative (Inflection {iCase}) = iCase ≡ Just Sublative

instance G.Translative Inflection where
    translative inf = inf { iCase = Just Translative }
    isTranslative (Inflection {iCase}) = iCase ≡ Just Translative

instance G.Vocative Inflection where
    vocative inf = inf { iCase = Just Vocative }
    isVocative (Inflection {iCase}) = iCase ≡ Just Vocative


-------------------------------------------------------------------------------
-- Locative case instances
-------------------------------------------------------------------------------

instance G.LocativeInessive Inflection where
    locativeInessive inf = inf {iCase = Just $ Locative $ Just LocativeInessive}
    isLocativeInessive (Inflection {iCase}) =
        iCase ≡ (Just $ Locative $ Just LocativeInessive)

instance G.LocativeElative Inflection where
    locativeElative inf = inf {iCase = Just $ Locative $ Just LocativeElative}
    isLocativeElative (Inflection {iCase}) =
        iCase ≡ (Just $ Locative $ Just LocativeElative)

instance G.LocativeIllative Inflection where
    locativeIllative inf = inf {iCase = Just $ Locative $ Just LocativeIllative}
    isLocativeIllative (Inflection {iCase}) =
        iCase ≡ (Just $ Locative $ Just LocativeIllative)

instance G.LocativeAdessive Inflection where
    locativeAdessive inf = inf {iCase = Just $ Locative $ Just LocativeAdessive}
    isLocativeAdessive (Inflection {iCase}) =
        iCase ≡ (Just $ Locative $ Just LocativeAdessive)

instance G.LocativeAblative Inflection where
    locativeAblative inf = inf {iCase = Just $ Locative $ Just LocativeAblative}
    isLocativeAblative (Inflection {iCase}) =
        iCase ≡ (Just $ Locative $ Just LocativeAblative)

instance G.LocativeAllative Inflection where
    locativeAllative inf = inf {iCase = Just $ Locative $ Just LocativeAllative}
    isLocativeAllative (Inflection {iCase}) =
        iCase ≡ (Just $ Locative $ Just LocativeAllative)


-------------------------------------------------------------------------------
-- Gender instances
-------------------------------------------------------------------------------

instance G.NoGender Inflection where
    noGender inf = inf { iGender = Nothing }
    hasNoGender (Inflection {iGender}) = iGender ≡ Nothing

instance G.Neuter Inflection where
    neuter inf = inf { iGender = Just Neuter }
    isNeuter (Inflection {iGender}) = iGender ≡ Just Neuter

instance G.Masculine Inflection where
    masculine inf = inf { iGender = Just Masculine }
    isMasculine (Inflection {iGender}) = iGender ≡ Just Masculine

instance G.Feminine Inflection where
    feminine inf = inf { iGender = Just Feminine }
    isFeminine (Inflection {iGender}) = iGender ≡ Just Feminine

instance G.Common Inflection where
    common inf = inf { iGender = Just Common }
    isCommon (Inflection {iGender}) = iGender ≡ Just Common


-------------------------------------------------------------------------------
-- Number instances
-------------------------------------------------------------------------------

instance G.NoNumber Inflection where
    noNumber inf = inf { iNumber = Nothing }
    hasNoNumber (Inflection {iNumber}) = iNumber ≡ Nothing

instance G.Singular Inflection where
    singular inf = inf { iNumber = Just Singular }
    isSingular (Inflection {iNumber}) = iNumber ≡ Just Singular

instance G.Dual Inflection where
    dual inf = inf { iNumber = Just Dual }
    isDual (Inflection {iNumber}) = iNumber ≡ Just Dual

instance G.Trial Inflection where
    trial inf = inf { iNumber = Just Trial }
    isTrial (Inflection {iNumber}) = iNumber ≡ Just Trial

instance G.Paucal Inflection where
    paucal inf = inf { iNumber = Just Paucal }
    isPaucal (Inflection {iNumber}) = iNumber ≡ Just Paucal

instance G.Plural Inflection where
    plural inf = inf { iNumber = Just Plural }
    isPlural (Inflection {iNumber}) = iNumber ≡ Just Plural
