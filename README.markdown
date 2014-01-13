Numerals
========

Convert numbers to numerals (number words) in a number of
languages. Each language has its own module. The module name is based
on one of the ISO 639 Alpha codes. Each module contains one or more
functions to convert numerical values to numerals. Several types of
numerals are supported. But not every type is supported by every
language. Some because they do not occur in that language. Others
because they are not yet defined in this package.

- Cardinal numerals

  Describe quantity - _one_, _two_, _three_, etc.

- Ordinal numerals

  Describe position in a sequential order - _first_, _second_,
  _third_, etc.

- Partitive numerals

  Describe division into fractions - _two thirds_, _three quarters_.

- Multiplicative numerals

  Describe repetition, how many time - _once_, _twice_, _thrice_.

### Inflection

In some languages number words are modified based on a number of
grammatical categories such as _gender_ or _number_. For instance, in
Spanish, the numeral for the quantity '1' can be one of _uno_, _un_ or
_una_ depending on whether it is of the neuter, masculine or feminine
gender. In order to support this process every conversion function
takes an inflection parameter which defines the grammatical state.

Inflections are not concrete types, but polymorphic parameters
constrained by type classes. Use the reified inflection type provided
by the `numerals-base` package to get a concrete value:

    >>> import Text.Numeral.Grammar.Reified ( defaultInflection )

### Numeral structure

The `struct` functions convert numbers to a polymorphic representation
of their grammatical structure. They are found in every language
module.

### Examples

The use of this package is best understood with some examples. First
some English number names, both British and US variants:

    >>> import qualified Text.Numeral.Language.EN as EN
    >>> EN.uk_cardinal defaultInflection 123 :: Maybe Text
    Just "one hundred and twenty-three"
    >>> EN.us_cardinal defaultInflection (10^50 + 42) :: Maybe Text
    Just "one hundred quindecillion forty-two"

French, which contains some traces of a base 20 system:

    >>> import qualified Text.Numeral.Language.FR as FR
    >>> FR.cardinal defaultInflection (-99) :: Maybe Text
    Just "moins quatre-vingt-dix-neuf"

Conversions can fail. Alamblak, a language spoken by a few people in
Papua New Guinea, has no representation for negative numbers:

    >>> import qualified Text.Numeral.Language.AMP as AMP
    >>> AMP.cardinal defaultInflection (-3) :: Maybe Text
    Nothing

Some languages have multiple scripts and methods for writing number
names. Take Chinese for example, which can be written using Han
characters or transcribed to the Latin script using Pinyin.

Traditional Chinese characters:

    >>> import qualified Text.Numeral.Language.ZH as ZH
    >>> ZH.trad_cardinal defaultInflection 123456 :: Maybe Text
    Just "十二萬三千四百五十六"

Simplified characters for use in financial contexts:

    >>> ZH.finance_simpl_cardinal defaultInflection 123456 :: Maybe Text
    Just "拾贰万参仟肆伯伍拾陆"

Transcribed using Pinyin:

    >>> ZH.pinyin_cardinal defaultInflection 123456 :: Maybe Text
    Just "shíèrwàn sānqiān sìbǎi wǔshí liù"

In Spanish the word for the quantity '1' differs based on its
gender. We convey the gender via the inflection parameter:

    >>> import Text.Numeral.Grammar ( masculine, feminine, neuter )
    >>> import Text.Numeral.Grammar.Reified ( defaultInflection )
    >>> import qualified Text.Numeral.Language.ES as ES
    >>> ES.cardinal (masculine defaultInflection) 1 :: Maybe Text
    Just "un"
    >>> ES.cardinal (feminine defaultInflection) 1 :: Maybe Text
    Just "una"
    >>> ES.cardinal (neuter defaultInflection) 1 :: Maybe Text
    Just "uno"

Using the `struct` functions you can see the grammatical structure of
number names. Because the results of these functions are polymorphic
you need to specify a specific type.

    >>> import qualified Text.Numeral.Language.NL as NL
    >>> NL.struct 123 :: Integer
    123
    >>> import Text.Numeral.Exp.Reified ( Exp, showExp )
    >>> showExp (NL.struct 123 :: Exp i)
    Add (Lit 100) (Add (Lit 3) (Mul (Lit 2) (Lit 10)))

Compare with:

    >>> NL.cardinal defaultInflection 123 :: Maybe Text
    Just "honderddrieëntwintig"

100 (honderd) + (3 (drie) + (ën) 2 (twin) * 10 (tig))
