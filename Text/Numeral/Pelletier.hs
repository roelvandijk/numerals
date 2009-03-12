{-# LANGUAGE OverloadedStrings #-}

module Text.Numeral.Pelletier
    ( longScale
    , longScalePlural
    , shortScale
    , shortScalePlural
    ) where

import Data.String
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Misc (d, untilNothing, weave, withSnd)


bigCardinal :: (IsString s, Joinable s) => Integer -> Maybe s
bigCardinal = cardinal bigNum Masculine

longScale :: (IsString s, Joinable s) => s -> s -> [NumSymbol s]
longScale a b = longScalePlural a a b b

longScalePlural :: (IsString s, Joinable s) => s -> s -> s -> s -> [NumSymbol s]
longScalePlural a as b bs = untilNothing $ weave (map illion [1..]) (map illiard [1..])
    where illion  n = fmap (\s -> mul (d 6 ^ n)       $ mulForms (s <> a) (s <> as)) $ bigCardinal n
          illiard n = fmap (\s -> mul (d 6 ^ n * d 3) $ mulForms (s <> b) (s <> bs)) $ bigCardinal n

shortScale :: (IsString s, Joinable s) => s -> [NumSymbol s]
shortScale a = shortScalePlural a a

shortScalePlural :: (IsString s, Joinable s) => s -> s -> [NumSymbol s]
shortScalePlural a as = untilNothing $ map illion [1..]
    where illion n = fmap (\s -> mul (d 3 * (d 3 ^ n)) $ mulForms (s <> a) (s <> as)) $ bigCardinal n

-------------------------------------------------------------------------------
-- Big Num 'language'
-------------------------------------------------------------------------------

bigNumTable :: (IsString s, Joinable s) => [NumSymbol s]
bigNumTable = [ term 0     $ const "nulla"
              , term 1     $ forms "m"     "un"       "un"       "mi"      "mi"
              , term 2     $ forms "b"     "duo"      "duo"      "vi"      "du"
              , term 3     $ forms "tr"    "tre"      "tres"     "tri"     "tre"
              , term 4     $ forms "quadr" "quattuor" "quattuor" "quadra"  "quadrin"
              , term 5     $ forms "quint" "quin"     "quinqua"  "quinqua" "quin"
              , term 6     $ forms "sext"  "sex"      "ses"      "sexa"    "ses"
              , term 7     $ forms "sept"  "septen"   "septem"   "septua"  "septin"
              , term 8     $ forms "oct"   "octo"     "octo"     "octo"    "octin"
              , term 9     $ forms "non"   "novem"    "novem"    "nona"    "non"
              , mul  10    $ \ctx -> case ctx of
                                        RM {} -> "gint"
                                        _     -> "dec"
              , mul  100   $ \ctx -> case ctx of
                                        RM n _ | n `elem` [2, 3, 6] -> "cent"
                                               | otherwise          -> "gent"
                                        _                           -> "cent"
              , mul  1000  $ const "mill"
              , mul  10000 $ const "myr"
              ]
    where forms d a1 a2 m1 m2 ctx = case ctx of
                                      RA 10  _ -> a1
                                      RA _   _ -> a2
                                      LM 100 _ -> m2
                                      LM _   _ -> m1
                                      _        -> d

bigNum :: (IsString s, Joinable s) => NumConfig s
bigNum = NumConfig { ncNeg      = error "bigNumNeg: undefined"
                   , ncOne      = snd
                   , ncAdd      = withSnd . flip $ (<>)
                   , ncMul      = withSnd (<>)
                   , ncCardinal = findSym bigNumTable
                   }

