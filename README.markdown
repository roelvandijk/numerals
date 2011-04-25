Numerals
========

Convert numbers to number words in a number of languages. Each
language has its own module. The module name is based on one of the
ISO 639 Alpha codes. Each module contains one or more `cardinal`
functions and a `struct` function. The `cardinal` functions directly
convert cardinal numbers to a string-like representation of their
spoken form. The `struct` functions convert numbers to a polymorphic
representation of their grammatical structure. All language modules
are implemented using the @numerals-base@ package.

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
    >>> NL.struct 123 :: Maybe Integer
    Just 123
    >>> import Text.Numeral
    >>> NL.struct 123 :: Maybe Exp
    Just (Add (Lit 100) (Add (Lit 3) (Mul (Lit 2) (Lit 10))))

Compare with:

    >>> NL.cardinal 123 :: Maybe String
    Just "honderddrieëntwintig"

100 (honderd) + (3 (drie) + (ën) 2 (twin) * 10 (tig))
