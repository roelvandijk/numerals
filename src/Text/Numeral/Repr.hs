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
                   , reprAdd ∷ Exp → Exp → Maybe s
                   , reprMul ∷ Exp → Exp → Maybe s
                   , reprSub ∷ Exp → Exp → Maybe s
                   , reprNeg ∷ Exp       → Maybe s
                   , reprAddCombine ∷ s → s → s → Maybe s
                   , reprMulCombine ∷ s → s → s → Maybe s
                   , reprSubCombine ∷ s → s → s → Maybe s
                   , reprNegCombine ∷ s → s     → Maybe s
                   }

defaultRepr ∷ (Monoid s) ⇒ Repr s
defaultRepr =
    Repr { reprValue = \_       → Nothing
         , reprScale = \_ _ _ _ → Nothing
         , reprAdd   = \_ _     → Nothing
         , reprMul   = \_ _     → Nothing
         , reprSub   = \_ _     → Nothing
         , reprNeg   = \_       → Nothing
         , reprAddCombine = \a x y → Just $ x ⊕ a ⊕ y
         , reprMulCombine = \m x y → Just $ x ⊕ m ⊕ y
         , reprSubCombine = \s x y → Just $ x ⊕ s ⊕ y
         , reprNegCombine = \n x   → Just $ n ⊕ x
         }


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

textify ∷ (Monoid s, IsString s) ⇒ Repr s → Exp → Maybe s
textify (Repr {..}) e = go CtxEmpty e
    where
      go ctx (Lit n) = ($ ctx) <$> reprValue n
      go ctx (Scale b o r) = reprScale b o r ctx
      go ctx (Neg x) = do x' ← go (CtxNeg ctx) x
                          n' ← reprNeg x
                          reprNegCombine n' x'
      go ctx (Add x y) = do x' ← go (CtxAdd L y ctx) x
                            y' ← go (CtxAdd R x ctx) y
                            a' ← reprAdd x y
                            reprAddCombine a' x' y'
      go ctx (Mul x y) = do x' ← go (CtxMul L y ctx) x
                            y' ← go (CtxMul R x ctx) y
                            m' ← reprMul x y
                            reprMulCombine m' x' y'
      go ctx (Sub x y) = do x' ← go (CtxSub L y ctx) x
                            y' ← go (CtxSub R x ctx) y
                            s' ← reprSub x y
                            reprSubCombine s' x' y'
