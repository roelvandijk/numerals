{-# LANGUAGE PackageImports #-}

module Text.Numeral
    ( -- * Overview
      -- $overview

      -- ** Expression language
      -- $dsl

      -- ** Rules
      -- $rules

      -- ** Rendering
      -- $render

      -- ** Examples
      -- $examples

      module Text.Numeral.Exp
    , module Text.Numeral.Render
    , module Text.Numeral.Rules
    , module Text.Numeral.Grammar
    )
    where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "this" Text.Numeral.Exp
import "this" Text.Numeral.Render
import "this" Text.Numeral.Rules
import "this" Text.Numeral.Grammar

-------------------------------------------------------------------------------
-- Documentation
-------------------------------------------------------------------------------

{- $overview
The general idea behind this package is to take a number, convert that number to
an abstract representation of its spoken form and finally render that
representation to a 'Text' value.
-}

{- $dsl
Numerals are represented by a small expression language defined in the
"Text.Numeral.Exp" module.
-}

{- $rules
Conversion from numbers to numerals is the responsibility of rules. The 'Rule'
type itself and a number of useful rules are defined in the "Text.Numeral.Rules"
module.
-}

{- $render
Finally, the "Text.Numeral.Render" module is responsible for converting the
numeral expression language to a 'Text' value. This happens via the 'render'
function. Render is parametrised with a 'Repr' value and with an
'Inflection'. The 'Repr' contains all the knowledge on how to convert the
abstract expression to a concrete 'Text' value. The 'Inflection' is used for
languages where number words change based on a number of grammatical categories
such as case, gender or number.
-}

{- $examples
The use of this package is best understood with some examples. First some
English number names, both British and US variants:

>>> import qualified Text.Numeral.Language.ENG as ENG
>>> ENG.gb_cardinal defaultInflection 123 :: Maybe Text
Just "one hundred and twenty-three"
>>> ENG.us_cardinal defaultInflection (10^50 + 42) :: Maybe Text
Just "one hundred quindecillion forty-two"

French, which contains some traces of a base 20 system:

>>> import qualified Text.Numeral.Language.FRA as FRA
>>> FRA.cardinal defaultInflection (-99) :: Maybe Text
Just "moins quatre-vingt-dix-neuf"

Conversions can fail. Alamblak, a language spoken by a few people in Papua New
Guinea, has no representation for negative numbers:

>>> import qualified Text.Numeral.Language.AMP as AMP
>>> AMP.cardinal defaultInflection (-3) :: Maybe Text
Nothing

Some languages have multiple scripts and methods for writing number names. Take
Chinese for example, which can be written using Han characters or transcribed to
the Latin script using Pinyin.

Traditional Chinese characters:

>>> import qualified Text.Numeral.Language.ZHO as ZHO
>>> ZHO.trad_cardinal defaultInflection 123456 :: Maybe Text
Just "十二萬三千四百五十六"

Simplified characters for use in financial contexts:

>>> ZHO.finance_simpl_cardinal defaultInflection 123456 :: Maybe Text
Just "拾贰万参仟肆伯伍拾陆"

Transcribed using Pinyin:

>>> ZHO.pinyin_cardinal defaultInflection 123456 :: Maybe Text
Just "shíèrwàn sānqiān sìbǎi wǔshí liù"

Using the 'struct' functions you can see the grammatical structure of number
names. Because the results of these functions are polymorphic you need to
specify a specific type.

>>> import qualified Text.Numeral.Language.NLD as NLD
>>> import Text.Numeral
>>> NLD.struct 123 :: Exp
Add (Lit 100) (Add (Lit 3) (Mul (Lit 2) (Lit 10)))

Compare with:

>>> NLD.cardinal defaultInflection 123 :: Maybe Text
Just "honderddrieëntwintig"

100 (honderd) + (3 (drie) + (ën) 2 (twin) * 10 (tig))
-}
