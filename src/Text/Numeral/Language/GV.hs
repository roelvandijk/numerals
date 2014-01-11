{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

{-|
[@ISO639-1@]        gv

[@ISO639-2@]        glv

[@ISO639-3@]        glv

[@Native name@]     Gaelg

[@English name@]    Manx
-}

module Text.Numeral.Language.GV
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( ($), const, fix )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Data.String   ( IsString )
import "base" Prelude       ( Integral )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.Exp as E
import           "this" Text.Numeral.Grammar ( Inflection )
import "this" Text.Numeral.Entry


-------------------------------------------------------------------------------
-- GV
-------------------------------------------------------------------------------

entry ∷ (Monoid s, IsString s) ⇒ Entry s
entry = emptyEntry
    { entIso639_1    = Just "gv"
    , entIso639_2    = ["glv"]
    , entIso639_3    = Just "glv"
    , entNativeNames = ["Gaelg"]
    , entEnglishName = Just "Manx"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β) ⇒ α → β
struct = checkPos
       $ fix
       $ findRule (   1, lit       )
                [ (  11, add 10 L  )
                , (  20, lit       )
                , (  21, add 20 L  )
                , (  40, lit       )
                , (  41, add 40 L  )
                , (  60, mul 20 R L)
                , ( 100, step 100 10 R L)
                , (1000, lit)
                ]
                   1000

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (1, 1000)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprAdd   = Just (⊞)
               , reprMul   = Just $ \_ _ _ → " "
               }
    where
      ((Lit _ `Mul` Lit 20) ⊞ Lit 10) _ = " as "
      (_                    ⊞ Lit 10) _ = "-"
      (_                    ⊞ _     ) _ = " as "

      syms =
          M.fromList
          [ (1, const "nane")
          , (2, \c → case c of
                       CtxAdd _ (Lit 10) _ → "daa"
                       CtxMul {}           → "daa"
                       _                   → "jees"
            )
          , (3, const "tree")
          , (4, const "kiare")
          , (5, const "queig")
          , (6, const "shey")
          , (7, const "shiaght")
          , (8, const "hoght")
          , (9, const "nuy")
          , (10, \c → case c of
                        CtxAdd _ (Lit 2)              _ → "yeig"
                        CtxAdd _ (Lit _ `Mul` Lit 20) _ → "jeih"
                        CtxAdd R _                    _ → "jeig"
                        _                               → "jeih"
            )
          , (20, const "feed")
          , (40, const "daeed")
          , (100, \c → case c of
                         CtxMul {} → "cheead"
                         _         → "keead"
            )
          , (1000, const "thousane")
          ]
