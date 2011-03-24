{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        it

[@ISO639-2B@]       ita

[@ISO639-3@]        ita

[@Native name@]     Italiano

[@English name@]    Italian
-}

module Text.Numeral.Language.IT
    ( cardinal
    , struct
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, (-), Integer )

-- from base-unicode-symbols:
import Data.List.Unicode   ( (∈) )
import Data.Ord.Unicode    ( (≥) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc ( dec )
import qualified Text.Numeral.BigNum      as BN
import qualified Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- IT
--------------------------------------------------------------------------------

-- Sources:
--   http://www.sf.airnet.ne.jp/~ts/language/number/italian.html
--   http://www.orbilat.com/Languages/Italian/Grammar/Italian-Numerals.html
--   http://italian.about.com/library/weekly/aa042600a.htm
--   http://www.suite101.com/content/how-to-count-in-italian-a146487

cardinal ∷ (Monoid s, IsString s, Integral α, C.Scale α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ ( Integral α, C.Scale α
         , C.Lit β, C.Neg β, C.Add β, C.Mul β, C.Scale β
         )
       ⇒ α → Maybe β
struct = pos $ fix $ rule `combine` pelletierScale R L BN.rule
    where
      rule = findRule (   0, lit         )
                    [ (  11, add   10 L  )
                    , (  17, add   10 R  )
                    , (  20, lit         )
                    , (  21, add   20 R  )
                    , (  30, mul   10 R L)
                    , ( 100, step  100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                      (dec 6 - 1)

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n syms
               , reprScale = pelletierRepr
               , reprAdd   = (⊞)
               , reprMul   = (⊡)
               , reprNeg   = \_ → Just "meno "
               }
    where
      Lit 10                ⊞ Lit 7 = Just "as"
      Lit 10                ⊞ Lit 9 = Just "an"
      (_ `Mul` Scale _ _ _) ⊞ _     = Just " "
      _                     ⊞ _     = Just ""


      Lit n ⊡ Lit 10 | n ≥ 4 = Just "an"
      _     ⊡ Scale _ _ _    = Just " "
      _     ⊡ _              = Just ""

      syms =
          M.fromList
          [ (0, const "zero")
          , (1, \c → case c of
                       CtxAdd _ (Lit 10) _ → "un"
                       _                   → "uno"
            )
          , (2, \c → case c of
                       CtxAdd _ (Lit 10) _ → "do"
                       _                   → "due"
            )
          , (3, \c → case c of
                       CtxAdd R _        _ → "tré"
                       CtxMul _ (Lit 10) _ → "tren"
                       _                   → "tre"
            )
          , (4, \c → case c of
                       CtxAdd _ (Lit 10) _ → "quattor"
                       CtxMul _ (Lit 10) _ → "quar"
                       _                   → "quattro"
            )
          , (5, \c → case c of
                       CtxAdd _ (Lit 10) _ → "quin"
                       CtxMul _ (Lit 10) _ → "cinqu"
                       _                   → "cinque"
            )
          , (6, \c → case c of
                       CtxAdd _ (Lit 10) _ → "se"
                       CtxMul _ (Lit 10) _ → "sess"
                       _                   → "sei"
            )
          , (7, \c → case c of
                       CtxMul _ (Lit 10) _ → "sett"
                       _                   → "sette"
            )
          , (8, \c → case c of
                       CtxMul _ (Lit 10) _ → "ott"
                       _                   → "otto"
            )
          , (9, \c → case c of
                       CtxMul _ (Lit 10) _ → "nov"
                       _                   → "nove"
            )
          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ → "dici"
                        -- Last vowel removed because of a
                        -- phonetic rule:
                        CtxMul _ (Lit _) (CtxAdd _ (Lit n) _)
                            | n ∈ [1,8]    → "t"
                        CtxMul R (Lit _) _ → "ta"
                        _                  → "dieci"
            )
          , (20, \c → case c of
                        CtxAdd _ (Lit n) _
                            | n ∈ [1,8]   → "vent"
                        _                 → "venti"
            )
          , ( 100
            , let f c = case c of
                          CtxAdd _ (Lit 8)                      _  → "cent"
                          CtxAdd _ (Lit 8 `Mul` Lit 10)         _  → "cent"
                          CtxAdd _ (Lit 8 `Mul` Lit 10 `Add` _) _  → "cent"
                          CtxMul _ (Lit _) c2 → f c2
                          _ → "cento"
              in f
            )
          , (1000, \c → case c of
                          CtxMul {} → "mila"
                          _         → "mille"
            )
          ]

pelletierRepr ∷ (IsString s, Monoid s)
              ⇒ Integer → Integer → Exp → Ctx Exp → Maybe s
pelletierRepr = BN.pelletierRepr
                  "ilione"  "ilioni"
                  "iliardo" "iliardi"
                  [ (6, BN.forms "sest" "sex"    "ses"    "sexa"   "ses")
                  , (7, BN.forms "sett" "septen" "septem" "septua" "septin")
                  , (8, BN.forms "ott"  "otto"   "otto"   "otto"   "ottin")
                  ]
