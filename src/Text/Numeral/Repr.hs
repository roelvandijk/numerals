{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , RecordWildCards
  #-}

module Text.Numeral.Repr
    ( -- * Representation of numerals
      SymbolContext(..)
    , Repr(..), defaultRepr
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
import Text.Show     ( Show )

-- from base-unicode-symbols:
import Data.Monoid.Unicode ( (⊕) )

-- from numerals:
import Text.Numeral.Exp ( Exp(..) )


--------------------------------------------------------------------------------
-- Structure of numerals
--------------------------------------------------------------------------------

data SymbolContext = Empty
                   | AddL Exp SymbolContext
                   | AddR Exp SymbolContext
                   | MulL Exp SymbolContext
                   | MulR Exp SymbolContext
                   | SubL Exp SymbolContext
                   | SubR Exp SymbolContext
                     deriving Show

data Repr s = Repr { reprValue ∷ Integer → Maybe (SymbolContext → s)
                   , reprScale ∷ Integer → Integer → Exp → SymbolContext → Maybe s
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
textify (Repr {..}) e = go Empty e
    where
      go ctx (C n) = ($ ctx) <$> reprValue n
      go ctx (Scale b o r) = reprScale b o r ctx
      go ctx (Neg x) = do x' ← go ctx x
                          n' ← reprNeg x
                          reprNegCombine n' x'
      go ctx (x :+: y) = do x' ← go (AddL y ctx) x
                            y' ← go (AddR x ctx) y
                            a' ← reprAdd x y
                            reprAddCombine a' x' y'
      go ctx (x :*: y) = do x' ← go (MulL y ctx) x
                            y' ← go (MulR x ctx) y
                            m' ← reprMul x y
                            reprMulCombine m' x' y'
      go ctx (Sub x y) = do x' ← go (SubL y ctx) x
                            y' ← go (SubR x ctx) y
                            s' ← reprSub x y
                            reprSubCombine s' x' y'
