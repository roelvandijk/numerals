{-# language CPP #-}
{-# language RecordWildCards #-}

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
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

#if !(MIN_VERSION_base(4,8,0))
import "base" Control.Applicative ( (<$>) )
#endif
import "base" Data.Monoid ( (<>) )
import "text" Data.Text ( Text )
import "this" Text.Numeral.Exp ( Exp(..), Side(L, R) )
import "this" Text.Numeral.Grammar ( Inflection(..) )

-------------------------------------------------------------------------------
-- Rendering numerals
-------------------------------------------------------------------------------

-- | Renders an expression to a 'Text' value according to a certain
-- representation and inflection.
render :: Repr -- ^ Representation.
       -> Inflection -- ^ Initial inflection.
       -> Exp -- ^ The expression to render.
       -> Maybe Text
render (Repr {..}) = go CtxEmpty
    where
      go _   _   Unknown = reprUnknown
      go ctx inf (Lit n) = ($ ctx) <$> reprValue inf n
      go ctx inf (Neg x) = do x' <- go (CtxNeg ctx) inf x
                              rn <- reprNeg
                              rnc <- reprNegCombine
                              Just $ rnc (rn x ctx) x' x
      go ctx inf (Add x y) = do x' <- go (CtxAdd L y ctx) inf x
                                y' <- go (CtxAdd R x ctx) inf y
                                ra <- reprAdd
                                rac <- reprAddCombine
                                Just $ rac (ra x y ctx) x' x y' y
      go ctx inf (Mul x y) = do x' <- go (CtxMul L y ctx) inf x
                                y' <- go (CtxMul R x ctx) inf y
                                rm <- reprMul
                                rmc <- reprMulCombine
                                Just $ rmc (rm x y ctx) x' x y' y
      go ctx inf (Sub x y) = do x' <- go (CtxSub L y ctx) inf x
                                y' <- go (CtxSub R x ctx) inf y
                                rs <- reprSub
                                rsc <- reprSubCombine
                                Just $ rsc (rs x y ctx) x' x y' y
      go ctx inf (Frac x y) = do x' <- go (CtxFrac L y ctx) inf x
                                 y' <- go (CtxFrac R x ctx) inf y
                                 rf <- reprFrac
                                 rfc <- reprFracCombine
                                 Just $ rfc (rf x y ctx) x' x y' y
      go ctx inf (Scale b o r) = reprScale inf b o r ctx
      go ctx inf (ChangeCase   mbCase   x) = go ctx (inf {iCase   = mbCase  }) x
      go ctx inf (ChangeGender mbGender x) = go ctx (inf {iGender = mbGender}) x
      go ctx inf (ChangeNumber mbNumber x) = go ctx (inf {iNumber = mbNumber}) x


--------------------------------------------------------------------------------
-- Representation of numerals
--------------------------------------------------------------------------------

-- | A representation for numerals.
--
-- A 'Repr' contains all the information on how to render an
-- 'Exp'ression to a 'Text' value.
data Repr =
    Repr
    { -- | Representation for unknown values.
      reprUnknown :: Maybe Text
      -- | Renders a literal value. Not necessarily defined for every
      -- value.
    , reprValue :: Inflection -> Integer -> Maybe (Ctx Exp -> Text)
      -- | Renders a negation. This concerns the negation itself, not
      -- the thing being negated.
    , reprNeg :: Maybe (Exp -> Ctx Exp -> Text)
      -- | Renders an addition. This concerns the addition itself, not
      -- the things being added. For example: In \"one hundred and
      -- eighty\" this function would be responsible for rendering the
      -- \"and\".
    , reprAdd :: Maybe (Exp -> Exp -> Ctx Exp -> Text)
      -- | Renders a multiplication. This concerns the multiplication
      -- itself, not the things being multiplied.
    , reprMul :: Maybe (Exp -> Exp -> Ctx Exp -> Text)
      -- | Renders a subtraction. This concerns the subtraction
      -- itself, not the things being subtracted.
    , reprSub :: Maybe (Exp -> Exp -> Ctx Exp -> Text)
      -- | Renders a fraction. This concerns the fraction itself, not
      -- the numerator or the denominator.
    , reprFrac :: Maybe (Exp -> Exp -> Ctx Exp -> Text)
      -- | Renders a step in a scale of large values.
    , reprScale :: ScaleRepr
      -- | Combines a negation and the thing being negated. For
      -- example: this would combine \"minus\" and \"three\" into
      -- \"minus three\".
    , reprNegCombine :: Maybe (Text -> Text -> Exp -> Text)
      -- | Combines an addition and the things being added.
    , reprAddCombine :: Maybe (Text -> Text -> Exp -> Text -> Exp -> Text)
      -- | Combines a multiplication and the things being multiplied.
    , reprMulCombine :: Maybe (Text -> Text -> Exp -> Text -> Exp -> Text)
      -- | Combines a subtraction and the things being subtracted.
    , reprSubCombine :: Maybe (Text -> Text -> Exp -> Text -> Exp -> Text)
      -- | Combines a fraction and the numerator and denominator.
    , reprFracCombine :: Maybe (Text -> Text -> Exp -> Text -> Exp -> Text)
    }

-- | Function that renders the representation of a step in a scale of
-- large values. The value represented by the step is 10 ^ (rank *
-- base + offset).
type ScaleRepr = Inflection
               -> Integer -- ^ Base.
               -> Integer -- ^ Offset.
               -> Exp -- ^ Rank.
               -> Ctx Exp -- ^ Rank context.
               -> Maybe Text

-- | The default representation.
--
-- Only the combining functions are defined. The rest are either
-- 'Nothing' or always produce 'Nothing'.
defaultRepr :: Repr
defaultRepr =
    Repr { reprUnknown = Nothing
         , reprValue   = \_ _ -> Nothing
         , reprNeg     = Nothing
         , reprAdd     = Nothing
         , reprMul     = Nothing
         , reprSub     = Nothing
         , reprFrac    = Nothing
         , reprScale   = \_ _ _ _ _ -> Nothing
         , reprNegCombine  = Just $ \n x _     -> n <> x
         , reprAddCombine  = Just $ \a x _ y _ -> x <> a <> y
         , reprMulCombine  = Just $ \m x _ y _ -> x <> m <> y
         , reprSubCombine  = Just $ \s x _ y _ -> x <> s <> y
         , reprFracCombine = Just $ \f n _ d _ -> n <> f <> d
         }


--------------------------------------------------------------------------------
-- Context of expressions
--------------------------------------------------------------------------------

-- | A context in which an 'Exp'ression appears.
data Ctx a
     -- | The empty context. Used for top level expressions.
   = CtxEmpty
     -- | Negation context.
   | CtxNeg (Ctx a)
     -- | Addition context.
   | CtxAdd Side a (Ctx a)
     -- | Multiplication context.
   | CtxMul Side a (Ctx a)
     -- | Subtraction context.
   | CtxSub Side a (Ctx a)
     -- | Fraction context.
   | CtxFrac Side a (Ctx a)
     -- | Scale context.
   | CtxScale (Ctx a)
     deriving (Eq, Show)

posIndex :: Ctx a -> Integer
posIndex c = go 0 c
    where
      go :: Integer -> Ctx a -> Integer
      go acc CtxEmpty = acc
      go acc (CtxNeg nc) = go acc nc
      go acc (CtxAdd  as _ ac) = go (acc + if as == L then -1 else 1) ac
      go acc (CtxMul  ms _ mc) = go (acc + if ms == L then -1 else 1) mc
      go acc (CtxSub  ss _ sc) = go (acc + if ss == L then -1 else 1) sc
      go acc (CtxFrac fs _ fc) = go (acc + if fs == L then -1 else 1) fc
      go acc (CtxScale  sc) = go acc sc

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
isOutside :: Side -> Ctx a -> Bool
isOutside s c = go c
    where
      go :: Ctx a -> Bool
      go CtxEmpty = True
      go (CtxNeg nc) = go nc
      go (CtxAdd  as _ ac) | as == s   = go ac
                           | otherwise = False
      go (CtxMul  ms _ mc) | ms == s   = go mc
                           | otherwise = False
      go (CtxSub  ss _ sc) | ss == s   = go sc
                           | otherwise = False
      go (CtxFrac fs _ fc) | fs == s   = go fc
                           | otherwise = False
      go (CtxScale  sc) = go sc
