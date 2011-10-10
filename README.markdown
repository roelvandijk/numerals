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

### Numeral structure

The `struct` functions convert numbers to a polymorphic representation
of their grammatical structure. They are found in every language
module.

### Implementation

All language modules are implemented using the
[numerals-base](https://github.com/roelvandijk/numerals-base) package.

### Examples

The use of this package is best understood with some examples. Because
the results of conversion are polymorphic we need to choose a specific
type. For these examples we'll use simple strings. But any type that
has instances for `Monoid` and `IsString` will work. First some
English number names, both British and US variants:

    >>> import qualified Text.Numeral.Language.EN as EN
    >>> EN.uk_cardinal 123 :: Maybe String
    Just "one hundred and twenty-three"
    >>> EN.us_cardinal (10^50 + 42) :: Maybe String
    Just "one hundred quindecillion forty-two"

French, which contains some traces of a base 20 system:

    >>> import qualified Text.Numeral.Language.FR as FR
    >>> FR.cardinal (-99) :: Maybe String
    Just "moins quatre-vingt-dix-neuf"

Conversions can fail. Alamblak, a language spoken by a few people in
Papua New Guinea, has no representation for negative numbers:

    >>> import qualified Text.Numeral.Language.AMP as AMP
    >>> AMP.cardinal (-3) :: Maybe String
    Nothing

Some languages have multiple scripts and methods for writing number
names. Take Chinese for example, which can be written using Han
characters or transcribed to the Latin script using Pinyin.

Traditional Chinese characters:

    >>> import qualified Text.Numeral.Language.ZH as ZH
    >>> ZH.trad_cardinal 123456 :: Maybe String
    Just "十二萬三千四百五十六"

Simplified characters for use in financial contexts:

    >>> ZH.finance_simpl_cardinal 123456 :: Maybe String
    Just "拾贰万参仟肆伯伍拾陆"

Transcribed using Pinyin:

    >>> ZH.pinyin_cardinal 123456 :: Maybe String
    Just "shíèrwàn sānqiān sìbǎi wǔshí liù"

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

    >>> NL.cardinal 123 :: Maybe String
    Just "honderddrieëntwintig"

100 (honderd) + (3 (drie) + (ën) 2 (twin) * 10 (tig))
