{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , RecordWildCards
  #-}

module Text.Numeral.Repr
    ( -- * Representation of numerals
      Repr(..), defaultRepr
    , textify
    )
    where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Function ( ($) )
import Data.Functor  ( (<$>) )
import Data.Maybe    ( Maybe(Nothing, Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integer )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )

-- from numerals:
import Text.Numeral.Exp ( Exp(..), Side(L, R), Ctx(..) )


--------------------------------------------------------------------------------
-- Structure of numerals
--------------------------------------------------------------------------------

data Repr s = Repr { reprValue ∷ Integer → Maybe (Ctx Exp → s)
                   , reprScale ∷ Integer → Integer → Exp → Ctx Exp → Maybe s
                   , reprAdd ∷ Maybe (Exp → Exp → s)
                   , reprMul ∷ Maybe (Exp → Exp → s)
                   , reprSub ∷ Maybe (Exp → Exp → s)
                   , reprNeg ∷ Maybe (Exp       → s)
                   , reprAddCombine ∷ Maybe (s → s → s → s)
                   , reprMulCombine ∷ Maybe (s → s → s → s)
                   , reprSubCombine ∷ Maybe (s → s → s → s)
                   , reprNegCombine ∷ Maybe (s → s     → s)
                   }

defaultRepr ∷ (Monoid s) ⇒ Repr s
defaultRepr =
    Repr { reprValue = \_       → Nothing
         , reprScale = \_ _ _ _ → Nothing
         , reprAdd   = Nothing
         , reprMul   = Nothing
         , reprSub   = Nothing
         , reprNeg   = Nothing
         , reprAddCombine = Just $ \a x y → x ⊕ a ⊕ y
         , reprMulCombine = Just $ \m x y → x ⊕ m ⊕ y
         , reprSubCombine = Just $ \s x y → x ⊕ s ⊕ y
         , reprNegCombine = Just $ \n x   → n ⊕ x
         }


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

textify ∷ (Monoid s, IsString s) ⇒ Repr s → Exp → Maybe s
textify (Repr {..}) e = go CtxEmpty e
    where
      go ctx (Lit n) = ($ ctx) <$> reprValue n
      go ctx (Scale b o r) = reprScale b o r ctx
      go ctx (Neg x) = do x' ← go (CtxNeg ctx) x
                          rn ← reprNeg
                          rnc ← reprNegCombine
                          Just $ rnc (rn x) x'
      go ctx (Add x y) = do x' ← go (CtxAdd L y ctx) x
                            y' ← go (CtxAdd R x ctx) y
                            ra ← reprAdd
                            rac ← reprAddCombine
                            Just $ rac (ra x y) x' y'
      go ctx (Mul x y) = do x' ← go (CtxMul L y ctx) x
                            y' ← go (CtxMul R x ctx) y
                            rm ← reprMul
                            rmc ← reprMulCombine
                            Just $ rmc (rm x y) x' y'
      go ctx (Sub x y) = do x' ← go (CtxSub L y ctx) x
                            y' ← go (CtxSub R x ctx) y
                            rs ← reprSub
                            rsc ← reprSubCombine
                            Just $ rsc (rs x y) x' y'
