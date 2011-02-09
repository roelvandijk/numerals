-- -*- coding: utf-8 -*-

{-# LANGUAGE CPP, OverloadedStrings #-}

module Text.Numeral.Language.FR (fr) where

import Data.String
import Text.Numeral
import Text.Numeral.Joinable
import Text.Numeral.Pelletier (longScalePlural)


-- Sources:
--  http://www.cliffsnotes.com/WileyCDA/CliffsReviewTopic/Numbers.topicArticleId-25559,articleId-25469.html

fr :: (IsString s, Joinable s) => NumConfig s
fr = NumConfig { ncNeg      = ("moins" <+>)
               , ncOne      = snd
               , ncAdd      = frAdd
               , ncMul      = frMul
               , ncCardinal = findSym frTable
               }

frAdd :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
frAdd (x, x') (y, y') | x == 10 && y < 7 = y' <> x'
                      | x < 80 && y == 1 = x' <+> "et" <+> y'
                      | x < 100          = x' <-> y'
                      | otherwise        = x' <+> y'

frMul :: (IsString s, Joinable s) => (Integer, s) -> (Integer, s) -> s
frMul (_, x') (y, y') | y == 10   = x' <> y'
                      | otherwise = x' <+> y'

frTable :: (IsString s, Joinable s) => [NumSymbol s]
frTable = [ term  0        $ const "zéro"
          , termG 1        $ tenFormsG (gender "un" "une") (const "on") (const "un")
          , term  2        $ tenForms "deux"   "deux"   "dou"
          , term  3        $ tenForms "trois"  "trei"   "tren"
          , term  4        $ tenForms "quatre" "quator" "quar"
          , term  5        $ tenForms "cinq"   "quin"   "cinqu"
          , term  6        $ tenForms "six"    "sei"    "soix"
          , term  7        $ const "sept"
          , term  8        $ const "huit"
          , term  9        $ const "neuf"
          , mul   10       $ \ctx -> case ctx of
                                        LA n _ | n < 7     -> "ze"
                                               | otherwise -> "dix"
                                        RM 3 _ -> "te"
                                        RM _ _ -> "ante"
                                        _      -> "dix"
          , add   20    10 $ const "vingt"
          , add   60    20 $ const "soixante"
          , term  71       $ const "soixante et onze"
          , term  80       $ const "quatre-vingts"
          , add   80    20 $ const "quatre-vingt"
          , mul   100      $ let c = "cent"
                             in \ctx -> case ctx of
                                          RM _ (LA _ _) -> c
                                          RM _ (LM _ _) -> c
                                          RM _ _        -> c <> "s"
                                          _             -> c
          , mul   1000     $ const "mille"
          ] ++ longScalePlural "illion" "illions" "illiard" "illiards"
