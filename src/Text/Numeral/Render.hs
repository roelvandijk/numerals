{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , PackageImports
           , RecordWildCards
  #-}

module Text.Numeral.Render
    ( -- * Rendering numerals
      render
      -- * Representation of numerals
    , Repr(..)
    , ScaleRepr
    , defaultRepr
      -- * Context of expressions
    , Ctx(..)
    , posIndex
    , isOutside
    )
    where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool     ( Bool(False, True), otherwise )
import "base" Data.Eq       ( Eq )
import "base" Data.Function ( ($) )
import "base" Data.Functor  ( (<$>) )
import "base" Data.Maybe    ( Maybe(Nothing, Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Prelude       ( (+) )
import "base" Text.Show     ( Show )
import "base-unicode-symbols" Data.Eq.Unicode     ( (≡) )
import "base-unicode-symbols" Data.Monoid.Unicode ( (⊕) )
import "base-unicode-symbols" Prelude.Unicode     ( ℤ )
import "this"                 Text.Numeral.Exp.Reified ( Exp(..), Side(L, R) )


-------------------------------------------------------------------------------
-- Rendering numerals
-------------------------------------------------------------------------------

-- | Renders an expression to a string-like value according to a
-- certain representation and inflection.
render ∷ (Monoid s)
       ⇒ Repr i s -- ^ Representation.
       → i        -- ^ Initial inflection.
       → Exp i    -- ^ The expression to render.
       → Maybe s
render (Repr {..}) = go CtxEmpty
    where
      go _   _   Unknown = reprUnknown
      go ctx inf (Lit n) = ($ ctx) <$> reprValue inf n
      go ctx inf (Neg x) = do x' ← go (CtxNeg ctx) inf x
                              rn ← reprNeg
                              rnc ← reprNegCombine
                              Just $ rnc (rn x ctx) x' x
      go ctx inf (Add x y) = do x' ← go (CtxAdd L y ctx) inf x
                                y' ← go (CtxAdd R x ctx) inf y
                                ra ← reprAdd
                                rac ← reprAddCombine
                                Just $ rac (ra x y ctx) x' x y' y
      go ctx inf (Mul x y) = do x' ← go (CtxMul L y ctx) inf x
                                y' ← go (CtxMul R x ctx) inf y
                                rm ← reprMul
                                rmc ← reprMulCombine
                                Just $ rmc (rm x y ctx) x' x y' y
      go ctx inf (Sub x y) = do x' ← go (CtxSub L y ctx) inf x
                                y' ← go (CtxSub R x ctx) inf y
                                rs ← reprSub
                                rsc ← reprSubCombine
                                Just $ rsc (rs x y ctx) x' x y' y
      go ctx inf (Frac x y) = do x' ← go (CtxFrac L y ctx) inf x
                                 y' ← go (CtxFrac R x ctx) inf y
                                 rf ← reprFrac
                                 rfc ← reprFracCombine
                                 Just $ rfc (rf x y ctx) x' x y' y
      go ctx inf (Scale b o r) = reprScale inf b o r ctx
      go ctx inf (Dual   x) = go (CtxDual   ctx) inf x
      go ctx inf (Plural x) = go (CtxPlural ctx) inf x
      go ctx inf (Inflection f x) = go ctx (f inf) x


--------------------------------------------------------------------------------
-- Representation of numerals
--------------------------------------------------------------------------------

-- | A representation for numerals.
--
-- A 'Repr' contains all the information on how to render an
-- 'Exp'ression to a string-like value.
data Repr i s =
    Repr
    { -- | Representation for unknown values.
      reprUnknown ∷ Maybe s
      -- | Renders a literal value. Not necessarily defined for every
      -- value.
    , reprValue ∷ i → ℤ → Maybe (Ctx (Exp i) → s)
      -- | Renders a negation. This concerns the negation itself, not
      -- the thing being negated.
    , reprNeg ∷ Maybe (Exp i → Ctx (Exp i) → s)
      -- | Renders an addition. This concerns the addition itself, not
      -- the things being added. For example: In \"one hundred and
      -- eighty\" this function would be responsible for rendering the
      -- \"and\".
    , reprAdd ∷ Maybe (Exp i → Exp i → Ctx (Exp i) → s)
      -- | Renders a multiplication. This concerns the multiplication
      -- itself, not the things being multiplied.
    , reprMul ∷ Maybe (Exp i → Exp i → Ctx (Exp i) → s)
      -- | Renders a subtraction. This concerns the subtraction
      -- itself, not the things being subtracted.
    , reprSub ∷ Maybe (Exp i → Exp i → Ctx (Exp i) → s)
      -- | Renders a fraction. This concerns the fraction itself, not
      -- the numerator or the denominator.
    , reprFrac ∷ Maybe (Exp i → Exp i → Ctx (Exp i) → s)
      -- | Renders a step in a scale of large values.
    , reprScale ∷ ScaleRepr i s
      -- | Combines a negation and the thing being negated. For
      -- example: this would combine \"minus\" and \"three\" into
      -- \"minus three\".
    , reprNegCombine ∷ Maybe (s → s → Exp i → s)
      -- | Combines an addition and the things being added.
    , reprAddCombine ∷ Maybe (s → s → Exp i → s → Exp i → s)
      -- | Combines a multiplication and the things being multiplied.
    , reprMulCombine ∷ Maybe (s → s → Exp i → s → Exp i → s)
      -- | Combines a subtraction and the things being subtracted.
    , reprSubCombine ∷ Maybe (s → s → Exp i → s → Exp i → s)
      -- | Combines a fraction and the numerator and denominator.
    , reprFracCombine ∷ Maybe (s → s → Exp i → s → Exp i → s)
    }

-- | Function that renders the representation of a step in a scale of
-- large values. The value represented by the step is 10 ^ (rank *
-- base + offset).
type ScaleRepr i s = i
                   → ℤ -- ^ Base.
                   → ℤ -- ^ Offset.
                   → Exp i -- ^ Rank.
                   → Ctx (Exp i) -- ^ Rank context.
                   → Maybe s

-- | The default representation.
--
-- Only the combining functions are defined. The rest are either
-- 'Nothing' or always produce 'Nothing'.
defaultRepr ∷ (Monoid s) ⇒ Repr inf s
defaultRepr =
    Repr { reprUnknown = Nothing
         , reprValue   = \_ _ → Nothing
         , reprNeg     = Nothing
         , reprAdd     = Nothing
         , reprMul     = Nothing
         , reprSub     = Nothing
         , reprFrac    = Nothing
         , reprScale   = \_ _ _ _ _ → Nothing
         , reprNegCombine  = Just $ \n x _     → n ⊕ x
         , reprAddCombine  = Just $ \a x _ y _ → x ⊕ a ⊕ y
         , reprMulCombine  = Just $ \m x _ y _ → x ⊕ m ⊕ y
         , reprSubCombine  = Just $ \s x _ y _ → x ⊕ s ⊕ y
         , reprFracCombine = Just $ \f n _ d _ → n ⊕ f ⊕ d
         }


--------------------------------------------------------------------------------
-- Context of expressions
--------------------------------------------------------------------------------

-- | A context in which an 'Exp'ression appears.
data Ctx α   -- | The empty context. Used for top level expressions.
           = CtxEmpty
             -- | Negation context.
           | CtxNeg (Ctx α)
             -- | Addition context.
           | CtxAdd Side α (Ctx α)
             -- | Multiplication context.
           | CtxMul Side α (Ctx α)
             -- | Subtraction context.
           | CtxSub Side α (Ctx α)
             -- | Fraction context.
           | CtxFrac Side α (Ctx α)
             -- | Scale context.
           | CtxScale (Ctx α)
             -- | Dual context.
           | CtxDual (Ctx α)
             -- | Plural context.
           | CtxPlural (Ctx α)
             deriving (Eq, Show)


posIndex ∷ Ctx α → ℤ
posIndex c = go 0 c
    where
      go ∷ ℤ → Ctx α → ℤ
      go acc CtxEmpty = acc
      go acc (CtxNeg nc) = go acc nc
      go acc (CtxAdd  as _ ac) = go (acc + if as ≡ L then -1 else 1) ac
      go acc (CtxMul  ms _ mc) = go (acc + if ms ≡ L then -1 else 1) mc
      go acc (CtxSub  ss _ sc) = go (acc + if ss ≡ L then -1 else 1) sc
      go acc (CtxFrac fs _ fc) = go (acc + if fs ≡ L then -1 else 1) fc
      go acc (CtxScale  sc) = go acc sc
      go acc (CtxDual   dc) = go acc dc
      go acc (CtxPlural pc) = go acc pc

-- | Checks whether a context is completely on the outside of an
-- expression, either left or right.
--
-- Given the following expression:
--
-- @
-- 'Add' ('Lit' 1000) ('Add' ('Mul' ('Lit' 2) ('Lit' 100)) ('Add' ('Lit' 4) ('Mul' ('Lit' 3) ('Lit' 10))))
-- @
--
-- On the left we have @'Lit' 1000@ and on the right @'Lit' 10@.
isOutside ∷ Side → Ctx α → Bool
isOutside s c = go c
    where
      go ∷ Ctx α → Bool
      go CtxEmpty = True
      go (CtxNeg nc) = go nc
      go (CtxAdd  as _ ac) | as ≡ s    = go ac
                           | otherwise = False
      go (CtxMul  ms _ mc) | ms ≡ s    = go mc
                           | otherwise = False
      go (CtxSub  ss _ sc) | ss ≡ s    = go sc
                           | otherwise = False
      go (CtxFrac fs _ fc) | fs ≡ s    = go fc
                           | otherwise = False
      go (CtxScale  sc) = go sc
      go (CtxDual   dc) = go dc
      go (CtxPlural pc) = go pc
