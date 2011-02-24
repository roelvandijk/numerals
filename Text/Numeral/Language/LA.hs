{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Language.LA
    ( cardinal
    , rules
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool(False, True), otherwise )
import Data.Function ( const, ($) )
import Data.List     ( map, concatMap )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( (+), Integral )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.List.Unicode     ( (∈) )
import Data.Monoid.Unicode   ( (⊕) )
import Data.Ord.Unicode      ( (≤) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral
import Text.Numeral.Misc      ( dec )
import Text.Numeral.Rules     ( Side(L, R), atom, atom1, add, mul, mul1 )


--------------------------------------------------------------------------------
-- LA
--------------------------------------------------------------------------------

{-
Sources:
  http://www.informalmusic.com/latinsoc/latnum.html
  http://www.sf.airnet.ne.jp/~ts/language/number/latin.html

TODO: need overcounting to represent [18, 19, 28, 29, ..., 98, 99].

18 = 2 from (2 ⋅ 10)
19 = 1 from (2 ⋅ 10)
28 = 2 from (3 ⋅ 10)
29 = 1 from (3 ⋅ 10)
-}

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal n = deconstruct findRule n >>= textify cardinalRepr

findRule ∷ (Integral α, Num β) ⇒ FindRule α β
findRule = mkFindRule rules (scale1 3 R L)

-- TODO: doen

rules ∷ (Integral α, Num β) ⇒ Rules α β
rules = [ ((  0,  10), atom)
        , (( 13,  17), add 10 L)
        , (( 18,  18), atom)
        , (( 19,  19), atom)
        ]

rules ∷ (Integral i) ⇒ Rules i
rules = Rules { rsFindRule = findRule rs
              , rsMulOne   = const False
              }
    where
      rs = map atom [1..9]
         ⊕ [ add 10 10 10 LeftAdd True
           , mul 10 10 10 RightAdd
           ]
         ⊕ concatMap (\n → [atom $ n + 8, atom $ n + 9]) [10,20..90]
         ⊕ [ mul (dec 2) (dec 2) (dec 1) RightAdd
           , mul (dec 3) (dec 3) (dec 3) RightAdd
           , mul (dec 6) (dec 6) (dec 6) RightAdd
           ]

cardinalRepr ∷ (IsString s) ⇒ Repr s
cardinalRepr =
    Repr { reprValue = \n → M.lookup n symMap
         , reprAdd   = (⊞)
         , reprMul   = (⊡)
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
                            LA (C 10)  _ → "ūn"
                            _            → "ūnus"
                 )
               , (2, \c → case c of
                            LM (C 10)  _ → "vī"
                            LM (C 100) _ → "du"
                            _            → "duo"
                 )
               , (3, \c → case c of
                            LA (C 10)  _ → "trē"
                            LM (C 10)  _ → "trī"
                            LM (C 100) _ → "tre"
                            _            → "trēs"
                 )
               , (4, \c → case c of
                            LM (C 10)  _ → "quadrā"
                            LM (C 100) _ → "quadrin"
                            _            → "quattuor"
                 )
               , (5, \c → case c of
                            LA (C 10)  _ → "quīn"
                            LM (C 10)  _ → "quīnquā"
                            LM (C 100) _ → "quīn"
                            _            → "quīnque"
                 )
               , (6, \c → case c of
                            LA (C 10)  _ → "sē"
                            LM (C 10)  _ → "sexā"
                            LM (C 100) _ → "ses"
                            _            → "sex"
                 )
               , (7, \c → case c of
                            LA (C 10)  _ → "septen"
                            LM (C 10)  _ → "septuā"
                            LM (C 100) _ → "septin"
                            _            → "septem"
                 )
               , (8, \c → case c of
                            LM (C 100) _ → "octin"
                            _            → "octō"
                 )
               , (9, \c → case c of
                            LM (C 10)  _ → "nōnā"
                            LM (C 100) _ → "nōn"
                            _            → "novem"
                 )
               , (10, \c → case c of
                             RA {}       → "decim"
                             RM (C 2)  _ → "gintī"
                             RM {}       → "gintā"
                             _           → "decem"
                 )
               , (18, const "duodēvīgintī")
               , (19, const "ūndēvīgintī")
               , (28, const "duodētrīgintā")
               , (29, const "ūndētrīgintā")
               , (38, const "duodēquadrāgintā")
               , (39, const "ūndēquadrāgintā")
               , (48, const "duodēquīnquāgintā")
               , (49, const "ūndēquīnquāgintā")
               , (58, const "duodēsexāgintā")
               , (59, const "ūndēsexāgintā")
               , (68, const "duodēseptuāgintā")
               , (69, const "ūndēseptuāgintā")
               , (78, const "duodēoctōgintā")
               , (79, const "ūndēoctōgintā")
               , (88, const "duodēnōnāgintā")
               , (89, const "ūndēnōnāgintā")
               , (98, const "duodēcentum")
               , (99, const "ūndēcentum")
               , (100, \c → case c of
                              RM (C n) _ | n ∈ [2, 3, 6] → "centī"
                                         | otherwise     → "gentī"
                              _                          → "centum"
                 )
               , (1000, \c → case c of
                               RM {} → "milia"
                               _     → "mīlle"
                 )
               ]
