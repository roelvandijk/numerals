{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2@]        chr

[@ISO639-3@]        chr

[@Native name@]     ᏣᎳᎩ ᎦᏬᏂᎯᏍᏗ (Tsalagi Gawonihisdi)

[@English name@]    Cherokee
-}

module Text.Numeral.Language.CHR
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
      -- * Transliteration
    , transliterate
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Char     ( Char )
import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Prelude       ( Integral, (-), String )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.Exp as E
import           "this" Text.Numeral.Grammar ( Inflection )
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )
import qualified "text" Data.Text as T ( concatMap, singleton )


--------------------------------------------------------------------------------
-- CHR
--------------------------------------------------------------------------------

entry ∷ Entry
entry = emptyEntry
    { entIso639_2    = ["chr"]
    , entIso639_3    = Just "chr"
    , entNativeNames = let n = "ᏣᎳᎩ ᎦᏬᏂᎯᏍᏗ" in [n, transliterate n]
    , entEnglishName = Just "Cherokee"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal ∷ (Inflection i, Integral α) ⇒ i → α → Maybe Text
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β) ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (   0, lit                )
                [ (  11, add   10        L  )
                , (  20, step  10   10   R L)
                , ( 100, step1 100  10   R L)
                , (1000, step1 1000 1000 R L)
                ]
                  (dec 6 - 1)

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (0, dec 6 - 1)

cardinalRepr ∷ i → Exp i → Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               }
    where
      (Mul _ _ ⊞ _) _ = " "
      (Lit 100 ⊞ _) _ = " "
      (_       ⊞ _) _ = ""

      (Lit _ ⊡ Lit 100) CtxEmpty = ""
      (Lit _ ⊡ Lit 100) _ = " "
      (_     ⊡ Lit 1000) _ = " "
      (_     ⊡ _       ) _ = ""

      syms =
          M.fromList
          [ (0, const "Ꮭ ᎪᏍᏗ")
          , (1, \c → case c of
                       CtxAdd L (Lit 10) _ → "Ꮜ"
                       CtxAdd R _        _ → "ᏌᏬ"
                       _                   → "ᏐᏬ"
            )
          , (2, const "ᏔᎵ")
          , (3, \c → case c of
                       CtxAdd L (Lit 10)  _ → "ᏦᎦ"
                       CtxMul _ (Lit 100) CtxEmpty → "Ꮶ"
                       CtxMul _ (Lit 100) _ → "ᏦᎢ"
                       _                    → "ᏦᎢ"
            )
          , (4, \c → case c of
                       CtxAdd L (Lit 10) _ → "ᏂᎦ"
                       _                   → "ᏅᎩ"
            )
          , (5, \c → case c of
                       CtxAdd L (Lit 10) _ → "ᎯᏍᎦ"
                       _                   → "ᎯᏍᎩ"
            )
          , (6, \c → case c of
                       CtxAdd L (Lit 10) _ → "ᏓᎳ"
                       _                   → "ᏑᏓᎵ"
            )
          , (7, \c → case c of
                       CtxAdd L (Lit 10)  _ → "ᎦᎵᏆ"
                       CtxMul _ (Lit 100) CtxEmpty → "ᎦᎵᏆ"
                       CtxMul _ (Lit 100) _ → "ᎦᎵᏉᎩ"
                       CtxMul _ _         _ → "ᎦᎵᏆ"
                       _                    → "ᎦᎵᏉᎩ"
            )
          , (8, \c → case c of
                       CtxAdd L (Lit 10)  _ → "ᏁᎳ"
                       CtxMul _ (Lit 100) CtxEmpty → "ᏧᏁᎵ"
                       _                    → "ᏧᏁᎳ"
            )
          , (9, \c → case c of
                       CtxMul _ (Lit 100) _ → "ᏐᏁᎵ"
                       _                    → "ᏐᏁᎳ"
            )
          , (10, \c → case c of
                        CtxAdd R (Lit _) _           → "Ꮪ"
                        CtxMul R (Lit _) (CtxAdd {}) → "ᏍᎪ"
                        _                            → "ᏍᎪᎯ"
            )
          , (100, const "ᏍᎪᎯᏥᏆ")
          , (1000, const "ᎢᏯᎦᏴᎵ")
          ]

-- | Transliterates a string written in the Cherokee syllabary to the
-- latin alphabet.
transliterate ∷ Text → Text
transliterate xs = T.concatMap f xs
    where
      f ∷ Char → Text
      f 'Ꭰ' = "a"
      f 'Ꭱ' = "e"
      f 'Ꭲ' = "i"
      f 'Ꭳ' = "o"
      f 'Ꭴ' = "u"
      f 'Ꭵ' = "v"
      f 'Ꭶ' = "ga"
      f 'Ꭷ' = "ka"
      f 'Ꭸ' = "ge"
      f 'Ꭹ' = "gi"
      f 'Ꭺ' = "go"
      f 'Ꭻ' = "gu"
      f 'Ꭼ' = "gv"
      f 'Ꭽ' = "ha"
      f 'Ꭾ' = "he"
      f 'Ꭿ' = "hi"
      f 'Ꮀ' = "ho"
      f 'Ꮁ' = "hu"
      f 'Ꮂ' = "hv"
      f 'Ꮃ' = "la"
      f 'Ꮄ' = "le"
      f 'Ꮅ' = "li"
      f 'Ꮆ' = "lo"
      f 'Ꮇ' = "lu"
      f 'Ꮈ' = "lv"
      f 'Ꮉ' = "ma"
      f 'Ꮊ' = "me"
      f 'Ꮋ' = "mi"
      f 'Ꮌ' = "mo"
      f 'Ꮍ' = "mu"
      f 'Ꮎ' = "na"
      f 'Ꮏ' = "hna"
      f 'Ꮐ' = "nah"
      f 'Ꮑ' = "ne"
      f 'Ꮒ' = "ni"
      f 'Ꮓ' = "no"
      f 'Ꮔ' = "nu"
      f 'Ꮕ' = "nv"
      f 'Ꮖ' = "qua"
      f 'Ꮗ' = "que"
      f 'Ꮘ' = "qui"
      f 'Ꮙ' = "quo"
      f 'Ꮚ' = "quu"
      f 'Ꮛ' = "quv"
      f 'Ꮝ' = "s"
      f 'Ꮜ' = "sa"
      f 'Ꮞ' = "se"
      f 'Ꮟ' = "si"
      f 'Ꮠ' = "so"
      f 'Ꮡ' = "su"
      f 'Ꮢ' = "sv"
      f 'Ꮣ' = "da"
      f 'Ꮤ' = "ta"
      f 'Ꮥ' = "de"
      f 'Ꮦ' = "te"
      f 'Ꮧ' = "di"
      f 'Ꮨ' = "ti"
      f 'Ꮩ' = "do"
      f 'Ꮪ' = "du"
      f 'Ꮫ' = "dv"
      f 'Ꮬ' = "dla"
      f 'Ꮭ' = "tla"
      f 'Ꮮ' = "tle"
      f 'Ꮯ' = "tli"
      f 'Ꮰ' = "tlo"
      f 'Ꮱ' = "tlu"
      f 'Ꮲ' = "tlv"
      f 'Ꮳ' = "tsa"
      f 'Ꮴ' = "tse"
      f 'Ꮵ' = "tsi"
      f 'Ꮶ' = "tso"
      f 'Ꮷ' = "tsu"
      f 'Ꮸ' = "tsv"
      f 'Ꮹ' = "wa"
      f 'Ꮺ' = "we"
      f 'Ꮻ' = "wi"
      f 'Ꮼ' = "wo"
      f 'Ꮽ' = "wu"
      f 'Ꮾ' = "wv"
      f 'Ꮿ' = "ya"
      f 'Ᏸ' = "ye"
      f 'Ᏹ' = "yi"
      f 'Ᏺ' = "yo"
      f 'Ᏻ' = "yu"
      f 'Ᏼ' = "yv"
      f c   = T.singleton c

