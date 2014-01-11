{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , PackageImports
  #-}

module Text.Numeral.Grammar
    ( -- * Inflection
      Inflection
      -- * Grammatical categories
      -- ** Case
    , NoCase               (noCase,               hasNoCase)
    , Ablative             (ablative,             isAblative)
    , Abessive             (abessive,             isAbessive)
    , Accusative           (accusative,           isAccusative)
    , Comitative           (comitative,           isComitative)
    , Dative               (dative,               isDative)
    , Delative             (delative,             isDelative)
    , Distributive         (distributive,         isDistributive)
    , DistributiveTemporal (distributiveTemporal, isDistributiveTemporal)
    , Essive               (essive,               isEssive)
    , Genitive             (genitive,             isGenitive)
    , Instrumental         (instrumental,         isInstrumental)
    , Instructive          (instructive,          isInstructive)
    , Lative               (lative,               isLative)
    , Locative             (locative,             isLocative)
    , Multiplicative       (multiplicative,       isMultiplicative)
    , Nominative           (nominative,           isNominative)
    , Partitive            (partitive,            isPartitive)
    , SuperEssive          (superEssive,          isSuperEssive)
    , Sublative            (sublative,            isSublative)
    , Translative          (translative,          isTranslative)
    , Vocative             (vocative,             isVocative)
      -- *** Locative cases
    , LocativeInessive (locativeInessive, isLocativeInessive)
    , LocativeElative  (locativeElative,  isLocativeElative)
    , LocativeIllative (locativeIllative, isLocativeIllative)
    , LocativeAdessive (locativeAdessive, isLocativeAdessive)
    , LocativeAblative (locativeAblative, isLocativeAblative)
    , LocativeAllative (locativeAllative, isLocativeAllative)
      -- ** Gender
    , NoGender  (noGender,  hasNoGender)
    , Neuter    (neuter,    isNeuter)
    , Masculine (masculine, isMasculine)
    , Feminine  (feminine,  isFeminine)
    , Common    (common,    isCommon)
      -- ** Number
    , NoNumber (noNumber, hasNoNumber)
    , Singular (singular, isSingular)
    , Dual     (dual,     isDual)
    , Trial    (trial,    isTrial)
    , Paucal   (paucal,   isPaucal)
    , Plural   (plural,   isPlural)
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Bool ( Bool )


--------------------------------------------------------------------------------
-- Inflection
--------------------------------------------------------------------------------

-- | Base class for various kinds of inflections.
class Inflection α


--------------------------------------------------------------------------------
-- Case
--------------------------------------------------------------------------------

class Inflection α ⇒ NoCase α where
    noCase ∷ α → α
    hasNoCase ∷ α → Bool

-- | The ablative case (abbreviated abl) indicates movement from
-- something, or cause.
class Inflection α ⇒ Ablative α where
    ablative ∷ α → α
    isAblative ∷ α → Bool

-- | In linguistics, abessive (abbreviated abe or abess), caritive and privative
-- (abbreviated priv) are names for a grammatical case expressing the lack or
-- absence of the marked noun.
class Inflection α ⇒ Abessive α where
    abessive ∷ α → α
    isAbessive ∷ α → Bool

-- | The accusative case (abbreviated acc) indicates the direct object
-- of a verb.
class Inflection α ⇒ Accusative α where
    accusative ∷ α → α
    isAccusative ∷ α → Bool

-- | The comitative case (abbreviated com), also known as the associative case
-- (abbreviated ass), is a grammatical case that denotes companionship, and is
-- used where English would typically use preposition "with" in the sense of "in
-- company with" or "together with" (other uses of "with," e.g. with the meaning
-- of "using," "by means of" (I cut bread with a knife) would correspond to the
-- instrumental case or related cases).
class Inflection α ⇒ Comitative α where
    comitative ∷ α → α
    isComitative ∷ α → Bool

-- | The dative case (abbreviated dat, or sometimes d when it is a
-- core argument) indicates the indirect object of a verb.
class Inflection α ⇒ Dative α where
    dative ∷ α → α
    isDative ∷ α → Bool

-- | The delative case (abbreviated del; from Latin deferre "to bear or bring
-- away or down") in the Hungarian language can originally express the movement
-- from the surface of something (e.g. "off the table"), but it is used in
-- several other meanings (e.g. "about people"), some of them related to the
-- original (e.g. "from the post office").
class Inflection α ⇒ Delative α where
    delative ∷ α → α
    isDelative ∷ α → Bool

-- | The distributive case (abbreviated distr) is used on nouns for the meanings
-- of per or each.
class Inflection α ⇒ Distributive α where
    distributive ∷ α → α
    isDistributive ∷ α → Bool

-- | The distributive-temporal case specifies when something is done.
class Inflection α ⇒ DistributiveTemporal α where
    distributiveTemporal ∷ α → α
    isDistributiveTemporal ∷ α → Bool

-- | The essive or similaris case (abbreviated ess) carries the meaning of a
-- temporary location or state of being, often equivalent to the English "as a
-- (child)".
class Inflection α ⇒ Essive α where
    essive ∷ α → α
    isEssive ∷ α → Bool

-- | The genitive case (abbreviated gen; also called the possessive
-- case or second case), which roughly corresponds to English's
-- possessive case and preposition of, indicates the possessor of
-- another noun.
class Inflection α ⇒ Genitive α where
    genitive ∷ α → α
    isGenitive ∷ α → Bool

-- | The instrumental case (abbreviated ins or instr; also called the
-- eighth case) indicates an object used in performing an action.
class Inflection α ⇒ Instrumental α where
    instrumental ∷ α → α
    isInstrumental ∷ α → Bool

-- | In the Finnish language and Estonian language, the instructive case has the
-- basic meaning of "by means of". It is a comparatively rarely used case,
-- though it is found in some commonly used expressions, such as omin silmin →
-- "with one's own eyes".
class Inflection α ⇒ Instructive α where
    instructive ∷ α → α
    isInstructive ∷ α → Bool

-- | Lative (abbreviated lat) is a case which indicates motion to a location. It
-- corresponds to the English prepositions "to" and "into". The lative case
-- belongs to the group of the general local cases together with the locative
-- and separative case. The term derives from the Latin lat-, the participle
-- stem of ferre, "to bring".
class Inflection α ⇒ Lative α where
    lative ∷ α → α
    isLative ∷ α → Bool

-- | The locative case (abbreviated loc) indicates a location.
class Inflection α ⇒ Locative α where
    locative ∷ α → α
    isLocative ∷ α → Bool

-- | The multiplicative case is a grammatical case used for marking a number of
-- something ("three times").
class Inflection α ⇒ Multiplicative α where
    multiplicative ∷ α → α
    isMultiplicative ∷ α → Bool

-- | The nominative case (abbreviated nom) indicates the subject of a
-- finite verb.
class Inflection α ⇒ Nominative α where
    nominative ∷ α → α
    isNominative ∷ α → Bool

-- | The partitive case (abbreviated ptv or more ambiguously part)
-- denotes "partialness", "without result", or "without specific
-- identity". It is also used in contexts where a subgroup is selected
-- from a larger group, or with numbers.
class Inflection α ⇒ Partitive α where
    partitive ∷ α → α
    isPartitive ∷ α → Bool

-- | The Superessive case (abbreviated supe) is a grammatical declension
-- indicating location on top of, or on the surface of something. Its name comes
-- from Latin supersum, superesse: to be over and above.
class Inflection α ⇒ SuperEssive α where
    superEssive ∷ α → α
    isSuperEssive ∷ α → Bool

-- | The term sublative case (abbreviated subl) is used to refer to grammatical
-- cases expressing different situations: In Hungarian, it expresses the
-- destination of the movement, originally to the surface of something (e.g. sit
-- down on the ground, climb the tree), but in other figurative meanings as well
-- (e.g. to university, for two nights), while in Tsez and other Northeast
-- Caucasian languages it denotes a movement towards the bottomsides or the area
-- under an object. The sublative case is used in the Finnish, Tsez and
-- Hungarian languages.
class Inflection α ⇒ Sublative α where
    sublative ∷ α → α
    isSublative ∷ α → Bool

-- | The translative case (abbreviated transl) is a grammatical case that
-- indicates a change in state of a noun, with the general sense of "becoming X"
-- or "change to X".
class Inflection α ⇒ Translative α where
    translative ∷ α → α
    isTranslative ∷ α → Bool

-- | The vocative case indicates an addressee.
class Inflection α ⇒ Vocative α where
    vocative ∷ α → α
    isVocative ∷ α → Bool


--------------------------------------------------------------------------------
-- Locative cases
--------------------------------------------------------------------------------

-- | Inessive case (abbreviated ine; from Latin inesse "to be in or at") is a
-- locative grammatical case. This case carries the basic meaning of "in".
class Locative α ⇒ LocativeInessive α where
    locativeInessive ∷ α → α
    isLocativeInessive ∷ α → Bool

-- | Elative (abbreviated ela; from Latin efferre "to bring or carry out") is a
-- locative case with the basic meaning "out of".
class Locative α ⇒ LocativeElative α where
    locativeElative ∷ α → α
    isLocativeElative ∷ α → Bool

-- | Illative (abbreviated ill; from Latin illatus "brought in") is, in the
-- Finnish language, Estonian language and the Hungarian language, the third of
-- the locative cases with the basic meaning of "into (the inside of)".
class Locative α ⇒ LocativeIllative α where
    locativeIllative ∷ α → α
    isLocativeIllative ∷ α → Bool

-- | In Uralic languages, such as Finnish, Estonian and Hungarian, the adessive
-- case (abbreviated ade; from Latin adesse "to be present") is the fourth of
-- the locative cases with the basic meaning of "on".
class Locative α ⇒ LocativeAdessive α where
    locativeAdessive ∷ α → α
    isLocativeAdessive ∷ α → Bool

-- | In linguistics, ablative case (abbreviated abl) is a name given to cases in
-- various languages whose common characteristic is that they mark motion away
-- from something, though the details in each language may differ. The name
-- "ablative" derives from the Latin ablatus, the (irregular) perfect passive
-- participle of auferre "to carry away".
class Locative α ⇒ LocativeAblative α where
    locativeAblative ∷ α → α
    isLocativeAblative ∷ α → Bool

-- | Allative case (abbreviated all; from Latin allāt-, afferre "to bring to")
-- is a type of the locative cases used in several languages. The term allative
-- is generally used for the lative case in the majority of languages which do
-- not make finer distinctions.
class Locative α ⇒ LocativeAllative α where
    locativeAllative ∷ α → α
    isLocativeAllative ∷ α → Bool


--------------------------------------------------------------------------------
-- Gender
--------------------------------------------------------------------------------

class Inflection α ⇒ NoGender α where
    noGender ∷ α → α
    hasNoGender ∷ α → Bool

class Inflection α ⇒ Neuter α where
    neuter ∷ α → α
    isNeuter ∷ α → Bool

class Inflection α ⇒ Masculine α where
    masculine ∷ α → α
    isMasculine ∷ α → Bool

class Inflection α ⇒ Feminine α where
    feminine ∷ α → α
    isFeminine ∷ α → Bool

class Inflection α ⇒ Common α where
    common ∷ α → α
    isCommon ∷ α → Bool


--------------------------------------------------------------------------------
-- Number
--------------------------------------------------------------------------------

class Inflection α ⇒ NoNumber α where
    noNumber ∷ α → α
    hasNoNumber ∷ α → Bool

class Inflection α ⇒ Singular α where
    singular ∷ α → α
    isSingular ∷ α → Bool

class Inflection α ⇒ Dual α where
    dual ∷ α → α
    isDual ∷ α → Bool

class Inflection α ⇒ Trial α where
    trial ∷ α → α
    isTrial ∷ α → Bool

class Inflection α ⇒ Paucal α where
    paucal ∷ α → α
    isPaucal ∷ α → Bool

class Inflection α ⇒ Plural α where
    plural ∷ α → α
    isPlural ∷ α → Bool

