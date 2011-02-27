{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        la

[@ISO639-2B@]       lat

[@ISO639-3@]        lat

[@Native name@]     Latine

[@English name@]    Latin

[@French name@]     Latin

[@Spanish name@]    Latin

[@Chinese name@]    拉丁语

[@Russian name@]    латинский

[@German name@]     Latein

[@Language family@] Indo-European,
                    Italic,
                    Latino-Faliscan,
                    Latin

[@Scope@]           Individual language

[@Type@]            Ancient
-}

module Text.Numeral.Language.LA
    ( cardinal
    , rule
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( otherwise )
import Data.Function ( ($), const )
import Data.List     ( concat )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, Num, (+) )

-- from base-unicode-symbols:
import Data.List.Unicode     ( (∈) )
import Data.Ord.Unicode      ( (≤) )
import Data.Monoid.Unicode   ( (⊕) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral


--------------------------------------------------------------------------------
-- LA
--------------------------------------------------------------------------------

{-
Sources:
  http://www.informalmusic.com/latinsoc/latnum.html
  http://www.sf.airnet.ne.jp/~ts/language/number/latin.html
-}

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = mkCardinal rule cardinalRepr

rule ∷ (Integral α, Num β, Subtract β) ⇒ Rule α β
rule = findRule rules

rules ∷ (Integral α, Num β, Subtract β) ⇒ Rules α β
rules = [ (( 0, 10), atom)
        , ((11, 17), add 10 L)
        , ((18, 19), sub 20)
        ]
      ⊕ concat [ [ ((n,  n+7), mul 10 R L)
                 , ((n+8,n+9), sub $ n+10)
                 ]
               | n ← [20,30..90]
               ]
      ⊕ [ (( 100,  100), atom)
        , (( 101,  199), add 100 L)
        , (( 200,  999), mul 100 R L)
        , ((1000, 1000), atom)
        ]

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
cardinalRepr =
    defaultRepr
    { reprValue = \n → M.lookup n symMap
    , reprAdd   = (⊞)
    , reprMul   = (⊡)
    , reprSub   = \_ _ → Just "dē"
    }
    where
      (_ :*: C _) ⊞ _ = Just " "
      _           ⊞ _ = Just ""

      _ ⊡ (C n) | n ≤ 100 = Just ""
      _ ⊡ _               = Just " "

      symMap = M.fromList
               [ (0, const "nihil")
               , (1, \c → case c of
                            AddL (C 10)  _ → "ūn"
                            SubL {}        → "ūn"
                            _              → "ūnus"
                 )
               , (2, \c → case c of
                            MulL (C 10)  _ → "vī"
                            MulL (C 100) _ → "du"
                            _              → "duo"
                 )
               , (3, \c → case c of
                            AddL (C 10)  _ → "trē"
                            MulL (C 10)  _ → "trī"
                            MulL (C 100) _ → "tre"
                            _              → "trēs"
                 )
               , (4, \c → case c of
                            MulL (C 10)  _ → "quadrā"
                            MulL (C 100) _ → "quadrin"
                            _              → "quattuor"
                 )
               , (5, \c → case c of
                            AddL (C 10)  _ → "quīn"
                            MulL (C 10)  _ → "quīnquā"
                            MulL (C 100) _ → "quīn"
                            _              → "quīnque"
                 )
               , (6, \c → case c of
                            AddL (C 10)  _ → "sē"
                            MulL (C 10)  _ → "sexā"
                            MulL (C 100) _ → "ses"
                            _              → "sex"
                 )
               , (7, \c → case c of
                            AddL (C 10)  _ → "septen"
                            MulL (C 10)  _ → "septuā"
                            MulL (C 100) _ → "septin"
                            _              → "septem"
                 )
               , (8, \c → case c of
                            MulL (C 100) _ → "octin"
                            _              → "octō"
                 )
               , (9, \c → case c of
                            MulL (C 10)  _ → "nōnā"
                            MulL (C 100) _ → "nōn"
                            _              → "novem"
                 )
               , (10, \c → case c of
                             AddR {}       → "decim"
                             MulR (C 2)  _ → "gintī"
                             MulR {}       → "gintā"
                             _             → "decem"
                 )
               , (100, \c → case c of
                              MulR (C n) _ | n ∈ [2,3,6] → "centī"
                                           | otherwise   → "gentī"
                              _                          → "centum"
                 )
               , (1000, \c → case c of
                               MulR {} → "milia"
                               _       → "mīlle"
                 )
               ]
