{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.LA
    ( cardinal
    , findRule
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>>=) )
import Data.Bool     ( otherwise )
import Data.Function ( const )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral )

-- from base-unicode-symbols:
import Data.List.Unicode     ( (∈) )
import Data.Ord.Unicode      ( (≤) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Rules ( Side(L, R), atom, add, mul, sub )


--------------------------------------------------------------------------------
-- LA
--------------------------------------------------------------------------------

{-
Sources:
  http://www.informalmusic.com/latinsoc/latnum.html
  http://www.sf.airnet.ne.jp/~ts/language/number/latin.html
-}

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal n = deconstruct findRule n >>= textify cardinalRepr

findRule ∷ (Integral α, Subtract β) ⇒ FindRule α β
findRule = mkFindRule rules []

-- TODO: doen

rules ∷ (Integral α, Subtract β) ⇒ Rules α β
rules = [ ((   0,   10), atom)
        , ((  11,   17), add 10 L)
        , ((  18,   19), sub 20)
        , ((  20,   27), mul 10 R L)
        , ((  28,   29), sub 30)
        , ((  30,   37), mul 10 R L)
        , ((  38,   39), sub 40)
        , ((  40,   47), mul 10 R L)
        , ((  48,   49), sub 50)
        , ((  50,   57), mul 10 R L)
        , ((  58,   59), sub 60)
        , ((  60,   67), mul 10 R L)
        , ((  68,   69), sub 70)
        , ((  70,   77), mul 10 R L)
        , ((  78,   79), sub 80)
        , ((  80,   87), mul 10 R L)
        , ((  88,   89), sub 90)
        , ((  90,   97), mul 10 R L)
        , ((  98,   99), sub 100)
        , (( 100,  100), atom)
        , (( 101,  199), add 100 L)
        , (( 200,  999), mul 100 R L)
        , ((1000, 1000), atom)
        ]

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd   = (⊞)
         , reprMul   = (⊡)
         , reprSub   = \_ _ → "dē"
         -- TODO: negative numbers probably can't be represented in latin
         , reprNeg   = "- "
         }
    where
      (_ :*: C _) ⊞ _ = " "
      _           ⊞ _ = ""

      _ ⊡ (C n) | n ≤ 100 = ""
      _ ⊡ _               = " "

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
                              MulR (C n) _ | n ∈ [2, 3, 6] → "centī"
                                           | otherwise     → "gentī"
                              _                            → "centum"
                 )
               , (1000, \c → case c of
                               MulR {} → "milia"
                               _       → "mīlle"
                 )
               ]
