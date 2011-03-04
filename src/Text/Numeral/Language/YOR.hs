{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

{-|
[@ISO639-1@]        yo

[@ISO639-2@]        yor

[@ISO639-3@]        yor

[@Native name@]     èdè Yorùbá

[@English name@]    Yoruba

[@French name@]     -

[@Spanish name@]    -

[@Chinese name@]    约鲁巴语

[@Russian name@]    -

[@German name@]     -

[@Language family@] Niger-Congo,
                    Atlantic-Congo,
                    Volta-Niger,
                    yeai,
                    Yoruboid,
                    Edekiri,
                    Yorùbá

[@Scope@]           Individual

[@Type@]            Living
-}

module Text.Numeral.Language.YOR
    ( cardinal
    , struct
    , cardinalRepr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( (>=>) )
import Data.Bool     ( otherwise )
import Data.Function ( ($), const, fix )
import Data.Maybe    ( Maybe(Nothing, Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Num, Integral )

-- from base:
import Data.Eq.Unicode     ( (≡) )
import Data.Monoid.Unicode ( (⊕) )

-- from containers:
import qualified Data.Map as M ( fromList, lookup )

-- from numerals:
import Text.Numeral


--------------------------------------------------------------------------------
-- YOR
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = struct >=> cardinalRepr

struct ∷ (Integral α, Num β, Subtract β) ⇒ α → Maybe β
struct = positive (fix rule)

rule ∷ (Integral α, Num β, Subtract β) ⇒ Rule α β
rule = conditional (≡ -5) atom $
       findRule (   1, atom       )
              [ (  11, add 10  L  )
              , (  15, add 20  L  )
              , (  16, sub 20     )
              , (  20, atom       )
              , (  21, add 20  L  )
              , (  25, add 30  L  )
              , (  26, sub 30     )
              , (  30, atom       )
              , (  31, add 30  L  )
              , (  35, sub 40     )
              , (  40, mul 20  L R)
              , (  45, sub 50     )
              , (  50, atom       )
              , (  51, add 50  L  )
              , (  55, sub 60     )
              , (  60, mul 20  L R)
              , (  65, sub 70     )
              , (  70, atom       )
              , (  71, add 70  L  )
              , (  75, sub 80     )
              , (  80, mul 20  L R)
              , (  85, sub 90     )
              , (  90, atom       )
              , (  91, add 90  L  )
              , (  95, sub 100    )
              , ( 100, mul 20  L R)
              , ( 200, mul 100 L R)
              , (1000, atom       )
              ]
                 1000

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Exp → Maybe s
cardinalRepr = textify defaultRepr
               { reprValue = \n → M.lookup n symMap
               , reprAdd   = (⊞)
               , reprMul   = (⊡)
               , reprSub   = \_ _ → Just "dil"
               }
    where
      _   ⊞ C 10           = Just ""
      C n ⊞ _  | n ≡ -5    = Just ""
               | otherwise = Just "lel"
      _   ⊞ _              = Nothing

      (C 20 :*: C 5) ⊡ _ = Just " "
      _              ⊡ _ = Just ""

      symMap = M.fromList
               [ (-5, const "med")
               , ( 1, \c → case c of
                             AddL {} → "mokan"
                             SubL {} → "mokan"
                             _       → "ikan"
                 )
               , ( 2, twentyForm "me" "go" "ji")
               , ( 3, twentyForm "me" "go" "ta")
               , ( 4, twentyForm "me" "go" "rin")
               , ( 5, twentyForm "ma" "go" "run")
               , ( 6, const    $ "me"  ⊕   "fa")
               , ( 7, const    $ "me"  ⊕   "je")
               , ( 8, const    $ "me"  ⊕   "jo")
               , ( 9, const    $ "me"  ⊕   "san")
               , (10, \c → case c of
                             AddR {} → "la"
                             _       → "mewa"
                 )
               , (20, \c → case c of
                             MulL {} → "o"
                             _       → "ogun"
                 )
               , (30, const "ogbon")
               , (50, const "adota")
               , (70, const "adorin")
               , (90, const "adorun")
               , (1000, const "egberun")
               ]

      twentyForm c m s = \ctx → case ctx of
                                  MulR (C 20) _ → m
                                  _             → c
                                ⊕ s
