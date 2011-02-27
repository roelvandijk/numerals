{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , RecordWildCards
  #-}

module Text.Numeral.Repr
    ( -- * Representation of numerals
      SymbolContext(..)
    , Repr(..)
    , textify
    )
    where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad ( return )
import Data.Function ( ($) )
import Data.Functor  ( (<$>) )
import Data.Maybe    ( Maybe )
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
                   , reprAdd   ∷ Exp → Exp → s
                   , reprMul   ∷ Exp → Exp → s
                   , reprSub   ∷ Exp → Exp → s
                   , reprNeg   ∷ s
                   }


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

textify ∷ (Monoid s, IsString s) ⇒ Repr s → Exp → Maybe s
textify (Repr {..}) e = go Empty e
    where
      go ctx (C n) = ($ ctx) <$> reprValue n
      go ctx (Neg x) = (reprNeg ⊕) <$> go ctx x
      go ctx (x :+: y) = do x' ← go (AddL y ctx) x
                            y' ← go (AddR x ctx) y
                            return $ x' ⊕ reprAdd x y ⊕ y'
      go ctx (x :*: y) = do x' ← go (MulL y ctx) x
                            y' ← go (MulR x ctx) y
                            return $ x' ⊕ reprMul x y ⊕ y'
      go ctx (Sub x y) = do x' ← go (SubL y ctx) x
                            y' ← go (SubR x ctx) y
                            return $ x' ⊕ reprSub x y ⊕ y'
