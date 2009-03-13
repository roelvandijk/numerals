{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module Text.Numeral
    ( -- *Types
      NumConfig(..)
    , NumSymbol(..)
    , SymbolType(..)
    , SymbolContext(..)
    , Gender(..)

      -- *Cardinals
    , cardinal
    , findSym

      -- *Smart NumSymbol constructors
    , term, add, mul
    , termG, addG, mulG

      -- *Symbol representation helper functions
    , gender, genderN
    , tenForms, tenFormsG, tenForms', mulForms
    )
    where

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data NumConfig s = NumConfig { ncCardinal :: Integer -> Maybe (NumSymbol s)
                             , ncNeg      :: s -> s
                             , ncOne      :: (Integer, s) -> s
                             , ncAdd      :: (Integer, s) -> (Integer, s) -> s
                             , ncMul      :: (Integer, s) -> (Integer, s) -> s
                             }

data NumSymbol s = NumSym { symType  :: SymbolType
                          , symVal   :: Integer
                          , symScope :: Integer
                          , symRepr  :: Gender -> SymbolContext -> s
                          }

data SymbolType = Terminal | Add | Mul deriving Show

data SymbolContext = EmptyContext
                   | LA Integer SymbolContext
                   | RA Integer SymbolContext
                   | LM Integer SymbolContext
                   | RM Integer SymbolContext
                     deriving Show

-- | Grammatical gender
data Gender = Neuter | Masculine | Feminine deriving Show

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

cardinal :: NumConfig s -> Gender -> Integer -> Maybe s
cardinal NumConfig {..} g x | x < 0     = fmap ncNeg $ go EmptyContext $ abs x
                            | x == 0    = fmap (\sym -> symRepr sym g EmptyContext) $ ncCardinal 0
                            | otherwise = go EmptyContext x
    where go ctx n = do (NumSym _ v _ rv) <- ncCardinal n
                        case n `divMod` v of
                          (1, 0) -> return $ ncOne (v, rv g ctx)
                          (1, r) -> do rs <- go (RA v ctx) r
                                       return $ (v, ncOne (v, rv g (LA r ctx))) `ncAdd` (r, rs)
                          (q, r) | q >= v    -> Nothing
                                 | otherwise -> do qs <- go (LM v ctx) q
                                                   if r == 0
                                                     then return $ (q, qs) `ncMul` (v, rv g (RM q ctx))
                                                     else do let qv = q * v
                                                             rs <- go (RA qv ctx) r
                                                             return $ (qv, (q, qs) `ncMul` (v, rv g (RM q (LA qv ctx)))) `ncAdd` (r, rs)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

findSym :: [NumSymbol s] -> Integer -> Maybe (NumSymbol s)
findSym []     _ = Nothing
findSym (e:es) n = go e e es
    where go :: NumSymbol s -> NumSymbol s -> [NumSymbol s] -> Maybe (NumSymbol s)
          go a m [] = stop a m
          go a m (x@(NumSym t v _ _) : xs)
              | v == n    = Just x
              | otherwise = case t of
                              Terminal        -> go a m xs
                              Add | v > n     -> stop a m
                                  | otherwise -> go x m xs
                              Mul | v > n     -> stop a m
                                  | otherwise -> go a x xs

          stop :: NumSymbol s -> NumSymbol s -> Maybe (NumSymbol s)
          stop a@(NumSym {..}) m | n < symVal + symScope = return a
                                 | otherwise             = return m

-------------------------------------------------------------------------------
-- Smart NumSymbol constructors
-------------------------------------------------------------------------------

termG :: Integer -> (Gender -> SymbolContext -> s) -> NumSymbol s
termG val fs = NumSym Terminal val 1 fs

addG :: Integer -> Integer -> (Gender -> SymbolContext -> s) -> NumSymbol s
addG scope val fs = NumSym Add scope val fs

mulG :: Integer -> (Gender -> SymbolContext -> s) -> NumSymbol s
mulG val fs = NumSym Mul val val fs

term :: Integer -> (SymbolContext -> s) -> NumSymbol s
term val fs = termG val $ const fs

add :: Integer -> Integer -> (SymbolContext -> s) -> NumSymbol s
add scope val fs = addG scope val $ const fs

mul :: Integer -> (SymbolContext -> s) -> NumSymbol s
mul val fs = mulG val $ const fs

-------------------------------------------------------------------------------
-- Symbol representation helper functions
-------------------------------------------------------------------------------

-- Differentiate between masculine and feminine genders. Other genders
-- default to masculine.
gender :: s -> s -> (Gender -> s)
gender m _ Masculine = m
gender _ f Feminine  = f
gender m _ _         = m

-- Differentiate between neuter, masculine and feminine genders
genderN :: s -> s -> s -> (Gender -> s)
genderN n _ _ Neuter    = n
genderN _ m _ Masculine = m
genderN _ _ f Feminine  = f


-- |Constructs a symbol representation based on the relation of the
--  symbol with the number 10.
--  The chosen representation depends on the context in which the
--  symbol is used:
--    d) default:        x
--    a) additive:       10 + x
--    m) multiplicative: x * 10
tenForms :: s -> s -> s -> (SymbolContext -> s)
tenForms d a m ctx = case ctx of
                       RA 10 _ -> a
                       LM 10 _ -> m
                       _       -> d

tenFormsG :: (Gender -> s) -> (Gender -> s) -> (Gender -> s) -> (Gender -> SymbolContext -> s)
tenFormsG d a m g ctx = case ctx of
                          RA 10 _ -> a g
                          LM 10 _ -> m g
                          _       -> d g

-- |Constructs a symbol representation based on the relation of the
--  symbol with the number 10.
--  The chosen representation depends on the context in which the
--  symbol is used:
--    d)  default:        x
--    a)  additive:       10 + x
--    mt) multiplicative: x * 10
--    mh) multiplicative: x * 100
tenForms' :: s -> s -> s -> s -> (SymbolContext -> s)
tenForms' d a mt mh ctx = case ctx of
                            RA 10  _ -> a
                            LM 10  _ -> mt
                            LM 100 _ -> mh
                            _        -> d

mulForms :: s -> s -> (SymbolContext -> s)
mulForms _ p (RM {}) = p
mulForms s _ _       = s
