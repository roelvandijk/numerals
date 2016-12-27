{-# LANGUAGE NamedFieldPuns #-}

module Text.Numeral.Grammar
    ( -- * Inflection
      Inflection(..)
    , defaultInflection
      -- * Grammatical categories
    , Case(..)
    , Gender(..)
    , Number(..)

      -- * Grammatical categories
      -- ** Case
    , noCase,               hasNoCase
    , ablative,             isAblative
    , abessive,             isAbessive
    , accusative,           isAccusative
    , comitative,           isComitative
    , dative,               isDative
    , delative,             isDelative
    , distributive,         isDistributive
    , distributiveTemporal, isDistributiveTemporal
    , essive,               isEssive
    , genitive,             isGenitive
    , instrumental,         isInstrumental
    , instructive,          isInstructive
    , lative,               isLative
    , locative,             isLocative
    , multiplicative,       isMultiplicative
    , nominative,           isNominative
    , partitive,            isPartitive
    , superEssive,          isSuperEssive
    , sublative,            isSublative
    , translative,          isTranslative
    , vocative,             isVocative
      -- *** Locative cases
    , locativeInessive, isLocativeInessive
    , locativeElative,  isLocativeElative
    , locativeIllative, isLocativeIllative
    , locativeAdessive, isLocativeAdessive
    , locativeAblative, isLocativeAblative
    , locativeAllative, isLocativeAllative
      -- ** Gender
    , noGender,  hasNoGender
    , neuter,    isNeuter
    , masculine, isMasculine
    , feminine,  isFeminine
    , common,    isCommon
      -- ** Number
    , noNumber, hasNoNumber
    , singular, isSingular
    , dual,     isDual
    , trial,    isTrial
    , paucal,   isPaucal
    , plural,   isPlural
    ) where

-------------------------------------------------------------------------------
-- Inflection
-------------------------------------------------------------------------------

data Inflection
   = Inflection
     { iCase   :: Maybe Case
     , iGender :: Maybe Gender
     , iNumber :: Maybe Number
     } deriving (Show, Eq)

defaultInflection :: Inflection
defaultInflection = Inflection Nothing Nothing Nothing


-------------------------------------------------------------------------------
-- Grammatical categories
-------------------------------------------------------------------------------

data Case
   = Abessive
     -- ^ In linguistics, abessive (abbreviated abe or abess), caritive and
     -- privative (abbreviated priv) are names for a grammatical case expressing
     -- the lack or absence of the marked noun.
   | Ablative
     -- ^ The ablative case (abbreviated abl) indicates movement from something,
     -- or cause.
   | Accusative
     -- ^ The accusative case (abbreviated acc) indicates the direct object of a
     -- verb.
   | Comitative
     -- ^ The comitative case (abbreviated com), also known as the associative
     -- case (abbreviated ass), is a grammatical case that denotes
     -- companionship, and is used where English would typically use preposition
     -- "with" in the sense of "in company with" or "together with" (other uses
     -- of "with," e.g. with the meaning of "using," "by means of" (I cut bread
     -- with a knife) would correspond to the instrumental case or related
     -- cases).
   | Dative
     -- ^ The dative case (abbreviated dat, or sometimes d when it is a core
     -- argument) indicates the indirect object of a verb.
   | Delative
     -- ^ The delative case (abbreviated del; from Latin deferre "to bear or
     -- bring away or down") in the Hungarian language can originally express
     -- the movement from the surface of something (e.g. "off the table"), but
     -- it is used in several other meanings (e.g. "about people"), some of them
     -- related to the original (e.g. "from the post office").
   | Distributive
     -- ^ The distributive case (abbreviated distr) is used on nouns for the
     -- meanings of per or each.
   | DistributiveTemporal
     -- ^ The distributive-temporal case specifies when something is done.
   | Essive
     -- ^ The essive or similaris case (abbreviated ess) carries the meaning of
     -- a temporary location or state of being, often equivalent to the English
     -- "as a (child)".
   | Genitive
     -- ^ The genitive case (abbreviated gen; also called the possessive case or
     -- second case), which roughly corresponds to English's possessive case and
     -- preposition of, indicates the possessor of another noun.
   | Instrumental
     -- ^ The instrumental case (abbreviated ins or instr; also called the
     -- eighth case) indicates an object used in performing an action.
   | Instructive
     -- ^ In the Finnish language and Estonian language, the instructive case
     -- has the basic meaning of "by means of". It is a comparatively rarely
     -- used case, though it is found in some commonly used expressions, such as
     -- omin silmin -> "with one's own eyes".
   | Lative
     -- ^ Lative (abbreviated lat) is a case which indicates motion to a
     -- location. It corresponds to the English prepositions "to" and
     -- "into". The lative case belongs to the group of the general local cases
     -- together with the locative and separative case. The term derives from
     -- the Latin lat-, the participle stem of ferre, "to bring".
   | Locative (Maybe Locative)
     -- ^ The locative case (abbreviated loc) indicates a location.
   | Multiplicative
     -- ^ The multiplicative case is a grammatical case used for marking a
     -- number of something ("three times").
   | Nominative
     -- ^ The nominative case (abbreviated nom) indicates the subject of a
     -- finite verb.
   | Partitive
     -- ^ The partitive case (abbreviated ptv or more ambiguously part) denotes
     -- "partialness", "without result", or "without specific identity". It is
     -- also used in contexts where a subgroup is selected from a larger group,
     -- or with numbers.
   | Sublative
     -- ^ The term sublative case (abbreviated subl) is used to refer to
     -- grammatical cases expressing different situations: In Hungarian, it
     -- expresses the destination of the movement, originally to the surface of
     -- something (e.g. sit down on the ground, climb the tree), but in other
     -- figurative meanings as well (e.g. to university, for two nights), while
     -- in Tsez and other Northeast Caucasian languages it denotes a movement
     -- towards the bottomsides or the area under an object. The sublative case
     -- is used in the Finnish, Tsez and Hungarian languages.
   | SuperEssive
     -- ^ The Superessive case (abbreviated supe) is a grammatical declension
     -- indicating location on top of, or on the surface of something. Its name
     -- comes from Latin supersum, superesse: to be over and above.
   | Translative
     -- ^ The translative case (abbreviated transl) is a grammatical case that
     -- indicates a change in state of a noun, with the general sense of
     -- "becoming X" or "change to X".
   | Vocative
     -- ^ The vocative case indicates an addressee.
     deriving (Eq, Show)

data Locative
   = LocativeIllative
     -- ^ Illative (abbreviated ill; from Latin illatus "brought in") is, in the
     -- Finnish language, Estonian language and the Hungarian language, the
     -- third of the locative cases with the basic meaning of "into (the inside
     -- of)".
   | LocativeInessive
     -- ^ Inessive case (abbreviated ine; from Latin inesse "to be in or at") is
     -- a locative grammatical case. This case carries the basic meaning of
     -- "in".
   | LocativeElative
     -- ^ Elative (abbreviated ela; from Latin efferre "to bring or carry out")
     -- is a locative case with the basic meaning "out of".
   | LocativeAllative
     -- ^ Allative case (abbreviated all; from Latin allāt-, afferre "to bring
     -- to") is a type of the locative cases used in several languages. The term
     -- allative is generally used for the lative case in the majority of
     -- languages which do not make finer distinctions.
   | LocativeAdessive
     -- ^ In Uralic languages, such as Finnish, Estonian and Hungarian, the
     -- adessive case (abbreviated ade; from Latin adesse "to be present") is
     -- the fourth of the locative cases with the basic meaning of "on".
   | LocativeAblative
     -- ^ In linguistics, ablative case (abbreviated abl) is a name given to
     -- cases in various languages whose common characteristic is that they mark
     -- motion away from something, though the details in each language may
     -- differ. The name "ablative" derives from the Latin ablatus, the
     -- (irregular) perfect passive participle of auferre "to carry away".
   deriving (Eq, Show)

data Gender
   = Neuter
   | Masculine
   | Feminine
   | Common
     deriving (Eq, Show)

data Number
   = Singular
   | Dual
   | Trial
   | Paucal
   | Plural
     deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Smart case constructors
-------------------------------------------------------------------------------

noCase :: Inflection -> Inflection
noCase inf = inf { iCase = Nothing }

hasNoCase :: Inflection -> Bool
hasNoCase (Inflection {iCase}) = iCase == Nothing

ablative :: Inflection -> Inflection
ablative inf = inf { iCase = Just Ablative }

isAblative :: Inflection -> Bool
isAblative (Inflection {iCase}) = iCase == Just Ablative

abessive :: Inflection -> Inflection
abessive inf = inf { iCase = Just Abessive }

isAbessive :: Inflection -> Bool
isAbessive (Inflection {iCase}) = iCase == Just Abessive

accusative :: Inflection -> Inflection
accusative inf = inf { iCase = Just Accusative }

isAccusative :: Inflection -> Bool
isAccusative (Inflection {iCase}) = iCase == Just Accusative

comitative :: Inflection -> Inflection
comitative inf = inf { iCase = Just Comitative }

isComitative :: Inflection -> Bool
isComitative (Inflection {iCase}) = iCase == Just Comitative

dative :: Inflection -> Inflection
dative inf = inf { iCase = Just Dative }

isDative :: Inflection -> Bool
isDative (Inflection {iCase}) = iCase == Just Dative

delative :: Inflection -> Inflection
delative inf = inf { iCase = Just Delative }

isDelative :: Inflection -> Bool
isDelative (Inflection {iCase}) = iCase == Just Delative

distributive :: Inflection -> Inflection
distributive inf = inf { iCase = Just Distributive }

isDistributive :: Inflection -> Bool
isDistributive (Inflection {iCase}) = iCase == Just Distributive

distributiveTemporal :: Inflection -> Inflection
distributiveTemporal inf = inf { iCase = Just DistributiveTemporal }

isDistributiveTemporal :: Inflection -> Bool
isDistributiveTemporal (Inflection {iCase}) = iCase == Just DistributiveTemporal

essive :: Inflection -> Inflection
essive inf = inf { iCase = Just Essive }

isEssive :: Inflection -> Bool
isEssive (Inflection {iCase}) = iCase == Just Essive

genitive :: Inflection -> Inflection
genitive inf = inf { iCase = Just Genitive }

isGenitive :: Inflection -> Bool
isGenitive (Inflection {iCase}) = iCase == Just Genitive

instrumental :: Inflection -> Inflection
instrumental inf = inf { iCase = Just Instrumental }

isInstrumental :: Inflection -> Bool
isInstrumental (Inflection {iCase}) = iCase == Just Instrumental

instructive :: Inflection -> Inflection
instructive inf = inf { iCase = Just Instructive }

isInstructive :: Inflection -> Bool
isInstructive (Inflection {iCase}) = iCase == Just Instructive

lative :: Inflection -> Inflection
lative inf = inf { iCase = Just Lative }

isLative :: Inflection -> Bool
isLative (Inflection {iCase}) = iCase == Just Lative

locative :: Inflection -> Inflection
locative inf = inf { iCase = Just (Locative Nothing) }

isLocative :: Inflection -> Bool
isLocative (Inflection {iCase}) =
        case iCase of
          Just (Locative _) -> True
          _                 -> False

multiplicative :: Inflection -> Inflection
multiplicative inf = inf { iCase = Just Multiplicative }

isMultiplicative :: Inflection -> Bool
isMultiplicative (Inflection {iCase}) = iCase == Just Multiplicative

nominative :: Inflection -> Inflection
nominative inf = inf { iCase = Just Nominative }

isNominative :: Inflection -> Bool
isNominative (Inflection {iCase}) = iCase == Just Nominative

partitive :: Inflection -> Inflection
partitive inf = inf { iCase = Just Partitive }

isPartitive :: Inflection -> Bool
isPartitive (Inflection {iCase}) = iCase == Just Partitive

superEssive :: Inflection -> Inflection
superEssive inf = inf { iCase = Just SuperEssive }

isSuperEssive :: Inflection -> Bool
isSuperEssive (Inflection {iCase}) = iCase == Just SuperEssive

sublative :: Inflection -> Inflection
sublative inf = inf { iCase = Just Sublative }

isSublative :: Inflection -> Bool
isSublative (Inflection {iCase}) = iCase == Just Sublative

translative :: Inflection -> Inflection
translative inf = inf { iCase = Just Translative }

isTranslative :: Inflection -> Bool
isTranslative (Inflection {iCase}) = iCase == Just Translative

vocative :: Inflection -> Inflection
vocative inf = inf { iCase = Just Vocative }

isVocative :: Inflection -> Bool
isVocative (Inflection {iCase}) = iCase == Just Vocative


-------------------------------------------------------------------------------
-- Smart locative case constructors
-------------------------------------------------------------------------------

locativeInessive :: Inflection -> Inflection
locativeInessive inf = inf {iCase = Just $ Locative $ Just LocativeInessive}

isLocativeInessive :: Inflection -> Bool
isLocativeInessive (Inflection {iCase}) =
        iCase == (Just $ Locative $ Just LocativeInessive)

locativeElative :: Inflection -> Inflection
locativeElative inf = inf {iCase = Just $ Locative $ Just LocativeElative}

isLocativeElative :: Inflection -> Bool
isLocativeElative (Inflection {iCase}) =
        iCase == (Just $ Locative $ Just LocativeElative)

locativeIllative :: Inflection -> Inflection
locativeIllative inf = inf {iCase = Just $ Locative $ Just LocativeIllative}

isLocativeIllative :: Inflection -> Bool
isLocativeIllative (Inflection {iCase}) =
        iCase == (Just $ Locative $ Just LocativeIllative)

locativeAdessive :: Inflection -> Inflection
locativeAdessive inf = inf {iCase = Just $ Locative $ Just LocativeAdessive}

isLocativeAdessive :: Inflection -> Bool
isLocativeAdessive (Inflection {iCase}) =
        iCase == (Just $ Locative $ Just LocativeAdessive)

locativeAblative :: Inflection -> Inflection
locativeAblative inf = inf {iCase = Just $ Locative $ Just LocativeAblative}

isLocativeAblative :: Inflection -> Bool
isLocativeAblative (Inflection {iCase}) =
        iCase == (Just $ Locative $ Just LocativeAblative)

locativeAllative :: Inflection -> Inflection
locativeAllative inf = inf {iCase = Just $ Locative $ Just LocativeAllative}

isLocativeAllative :: Inflection -> Bool
isLocativeAllative (Inflection {iCase}) =
        iCase == (Just $ Locative $ Just LocativeAllative)


-------------------------------------------------------------------------------
-- Smart gender constructors
-------------------------------------------------------------------------------

noGender :: Inflection -> Inflection
noGender inf = inf { iGender = Nothing }

hasNoGender :: Inflection -> Bool
hasNoGender (Inflection {iGender}) = iGender == Nothing

neuter :: Inflection -> Inflection
neuter inf = inf { iGender = Just Neuter }

isNeuter :: Inflection -> Bool
isNeuter (Inflection {iGender}) = iGender == Just Neuter

masculine :: Inflection -> Inflection
masculine inf = inf { iGender = Just Masculine }

isMasculine :: Inflection -> Bool
isMasculine (Inflection {iGender}) = iGender == Just Masculine

feminine :: Inflection -> Inflection
feminine inf = inf { iGender = Just Feminine }

isFeminine :: Inflection -> Bool
isFeminine (Inflection {iGender}) = iGender == Just Feminine

common :: Inflection -> Inflection
common inf = inf { iGender = Just Common }

isCommon :: Inflection -> Bool
isCommon (Inflection {iGender}) = iGender == Just Common


-------------------------------------------------------------------------------
-- Smart number constructors
-------------------------------------------------------------------------------

noNumber :: Inflection -> Inflection
noNumber inf = inf { iNumber = Nothing }

hasNoNumber :: Inflection -> Bool
hasNoNumber (Inflection {iNumber}) = iNumber == Nothing

singular :: Inflection -> Inflection
singular inf = inf { iNumber = Just Singular }

isSingular :: Inflection -> Bool
isSingular (Inflection {iNumber}) = iNumber == Just Singular

dual :: Inflection -> Inflection
dual inf = inf { iNumber = Just Dual }

isDual :: Inflection -> Bool
isDual (Inflection {iNumber}) = iNumber == Just Dual

trial :: Inflection -> Inflection
trial inf = inf { iNumber = Just Trial }

isTrial :: Inflection -> Bool
isTrial (Inflection {iNumber}) = iNumber == Just Trial

paucal :: Inflection -> Inflection
paucal inf = inf { iNumber = Just Paucal }

isPaucal :: Inflection -> Bool
isPaucal (Inflection {iNumber}) = iNumber == Just Paucal

plural :: Inflection -> Inflection
plural inf = inf { iNumber = Just Plural }

isPlural :: Inflection -> Bool
isPlural (Inflection {iNumber}) = iNumber == Just Plural
