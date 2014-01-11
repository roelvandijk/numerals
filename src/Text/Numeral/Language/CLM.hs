{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        -

[@ISO639-2B@]       -

[@ISO639-3@]        clm

[@Native name@]     nəxʷsƛ̕ay̕əmúcən

[@English name@]    Klallam
-}

module Text.Numeral.Language.CLM
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- CLM
--------------------------------------------------------------------------------

entry ∷ (Monoid s, IsString s) ⇒ Entry s
entry = emptyEntry
    { entIso639_3    = Just "clm"
    , entNativeNames = ["nəxʷsƛ̕ay̕əmúcən"]
    , entEnglishName = Just "Klallam"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

cardinal ∷ (Inflection i, Integral α, Monoid s, IsString s) ⇒ i → α → Maybe s
cardinal inf = cardinalRepr inf ∘ struct

struct ∷ (Integral α, E.Unknown β, E.Lit β, E.Add β, E.Mul β) ⇒ α → β
struct = fix
       $ findRule (   1, lit            )
                [ (  11, add 10 R       )
                , (  20, lit            )
                , (  21, add 20 R       )
                , (  30, step 10 10 R L )
                , ( 100, lit            )
                , ( 101, step 100 10 R L)
                , (1000, mul 100 R L    )
                ]
                  1000

bounds ∷ (Integral α) ⇒ (α, α)
bounds = (1, 1000)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ i → Exp i → Maybe s
cardinalRepr = render defaultRepr
               { reprValue = \_ n → M.lookup n syms
               , reprAdd   = Just $ \_ _ _ → " ʔiʔ "
               , reprMul   = Just (⊡)
               }
    where
      (_ ⊡ Lit 100) _ = " "
      (_ ⊡ _      ) _ = ""

      syms =
          M.fromList
          [ (1, const "nə́c̕uʔ")
          , (2, const "čə́saʔ")
          , (3, forms "ɬíxʷ"   "ɬxʷ"   )
          , (4, forms "ŋús"    "ŋəs"   )
          , (5, forms "ɬq̕áčš"  "ɬq̕čš"  )
          , (6, forms "t̕x̣ə́ŋ"   "t̕x̣əŋ"  )
          , (7, forms "c̕úʔkʷs" "c̕aʔkʷs")
          , (8, forms "táʔcs"  "taʔcs" )
          , (9, forms "tə́kʷxʷ" "təkʷxʷ")
          , (10, \c → case c of
                        CtxMul R _ _ → "ɬšáʔ"
                        _            → "ʔúpən"
            )
          , (20, const "nəc̕xʷk̕ʷə́s")
          , (100, const "snáč̕əwəč")
          ]

      forms ∷ s
            → s
            → Ctx (Exp i)
            → s
      forms n t ctx = case ctx of
                        CtxMul _ (Lit 10) _ → t
                        _                   → n
