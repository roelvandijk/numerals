{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
[@ISO639-1@]        fi

[@ISO639-2B@]       fin

[@ISO639-3@]        fin

[@Native name@]     suomi

[@English name@]    Finnish
-}

module Text.Numeral.Language.FIN
    ( -- * Language entry
      entry
      -- * Inflection
    , Inflection
      -- * Conversions
    , cardinal
    , ordinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), fix, flip )
import "base" Data.Maybe    ( Maybe(Just) )
import "base" Prelude       ( Num, Integral, (-), negate, divMod )
import "base-unicode-symbols" Data.Bool.Unicode     ( (∧) )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤) )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum  as BN
import qualified "this" Text.Numeral.Exp     as E
import qualified "this" Text.Numeral.Grammar as G
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )


-------------------------------------------------------------------------------
-- FIN
-------------------------------------------------------------------------------

entry ∷ Entry
entry = emptyEntry
    { entIso639_1    = Just "fi"
    , entIso639_2    = ["fin"]
    , entIso639_3    = Just "fin"
    , entNativeNames = ["suomi"]
    , entEnglishName = Just "Finnish"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    , entOrdinal     = Just Conversion
                       { toNumeral   = ordinal
                       , toStructure = struct
                       }
    }

type Inflection i =
  ( G.Singular i
  , G.Plural i
  , G.Abessive i
  , G.Accusative i
  , G.Comitative i
  , G.Delative i
  , G.Distributive i
  , G.DistributiveTemporal i
  , G.Essive i
  , G.Genitive i
  , G.Instructive i
  , G.Lative i
  , G.LocativeInessive i
  , G.LocativeElative i
  , G.LocativeIllative i
  , G.LocativeAdessive i
  , G.LocativeAblative i
  , G.LocativeAllative i
  , G.Multiplicative i
  , G.Nominative i
  , G.Partitive i
  , G.Sublative i
  , G.SuperEssive i
  , G.Translative i
  )

cardinal ∷ (Inflection i, Integral α, E.Scale α)
         ⇒ i → α → Maybe Text
cardinal inf = cardinalRepr inf ∘ struct

ordinal ∷ (Inflection i, Integral α, E.Scale α)
        ⇒ i → α → Maybe Text
ordinal inf = ordinalRepr inf ∘ struct

struct ∷ ( Integral α, E.Scale α
         , E.Unknown β, E.Lit β, E.Add β, E.Mul β
         , E.Inflection β, E.Scale β
         , G.Singular   (E.Inf β)
         , G.Nominative (E.Inf β)
         , G.Accusative (E.Inf β)
         , G.Partitive  (E.Inf β)
         )
       ⇒ α → β
struct = fix $ rule `combine` pelletierScale R L BN.rule
    where
      rule = findRule (   0, lit)
                    [ (  11, add 10 L)
                    , (  20, fi_mul 10)
                    , ( 100, step  100   10 R L)
                    , (1000, step 1000 1000 R L)
                    ]
                      (dec 6 - 1)

-- | Like the normal 'mul' rule with the difference that the value
-- that is multiplied is changed to the partitive case.
fi_mul ∷ ( Integral α
         , E.Add β, E.Mul β, E.Inflection β
         , G.Singular   (E.Inf β)
         , G.Nominative (E.Inf β)
         , G.Accusative (E.Inf β)
         , G.Partitive  (E.Inf β)
         )
       ⇒ α → Rule α β
fi_mul val =
    \f n → let (m, a) = n `divMod` val
               mval = E.mul (f m) (E.inflection toPartitive $ f val)
           in if a ≡ 0
              then mval
              else (flip E.add) (f a) mval
  where
    toPartitive ∷ (G.Singular i, G.Nominative i, G.Accusative i, G.Partitive i)
                ⇒ i → i
    toPartitive inf | G.isSingular inf ∧ G.isNominative inf = G.partitive inf
                    | G.isSingular inf ∧ G.isAccusative inf = G.partitive inf
                    | otherwise = inf

bounds ∷ (Integral α) ⇒ (α, α)
bounds = let x = dec 15 - 1 in (negate x, x)

cardinalRepr ∷ (Inflection i) ⇒ i → Exp i → Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \inf n → M.lookup n (syms inf)
               , reprScale = BN.pelletierRepr
                               (BN.quantityName "iljoona" "iljoonaa")
                               (BN.quantityName "iljardi" "iljardia")
                               []
               , reprAdd   = Just $ \_ _ _ → ""
               , reprMul   = Just $ \_ _ _ → ""
               }
    where
      syms inf =
          M.fromList
          [ (0, \c → infForms inf c
                              -- singular
                              "nolla"    {- nom -} "nolla"    {- acc -}
                              "nollan"   {- gen -} "nollaa"   {- ptv -}
                              "nollana"  {- ess -} "nollaksi" {- transl -}
                              "nollassa" {- ine -} "nollasta" {- ela -}
                              "nollaan"  {- ill -} "nollalla" {- ade -}
                              "nollalta" {- abl -} "nollalle" {- all -}
                              "nollatta" {- abe -}
                              "?"        {- other -}
                              -- plural
                              "nollat"   {- nom -} "nollat"   {- acc -}
                              "nollien"  {- gen -} "nollia"   {- ptv -}
                              "nollina"  {- ess -} "nolliksi" {- transl -}
                              "nollissa" {- ine -} "nollista" {- ela -}
                              "nolliin"  {- ill -} "nollilla" {- ade -}
                              "nollilta" {- abl -} "nollille" {- all -}
                              "nollitta" {- abe -} "nolline"  {- com -}
                              "nollin"   {- instr -}
                              "?"        {- other -}
            )
         , (1, \c → infForms inf c
                             -- singular
                             "yksi"     {- nom -} "yksi"     {- acc -}
                             "yhden"    {- gen -} "yhtä"     {- ptv -}
                             "yhtenä"   {- ess -} "yhdeksi"  {- transl -}
                             "yhdessä"  {- ine -} "yhdestä"  {- ela -}
                             "yhteen"   {- ill -} "yhdellä"  {- ade -}
                             "yhdeltä"  {- abl -} "yhdelle"  {- all -}
                             "yhdettä"  {- abe -}
                             (case inf of
                                _ | G.isSuperEssive    inf → "yhtäällä"
                                  | G.isDelative       inf → "yhtäältä"
                                  | G.isSublative      inf → "yhtäälle"
                                  | G.isLative         inf → "yhä"
                                  | G.isMultiplicative inf → "yhdesti"
                                  | otherwise              → "?"
                             )
                             -- plural
                             "yhdet"    {- nom -} "yhdet"    {- acc -}
                             "yksien"   {- gen -} "yksiä"    {- ptv -}
                             "yksinä"   {- ess -} "yksiksi"  {- transl -}
                             "yksissä"  {- ine -} "yksistä"  {- ela -}
                             "yksiin"   {- ill -} "yksillä"  {- ade -}
                             "yksiltä"  {- abl -} "yksille"  {- all -}
                             "yksittä"  {- abe -} "yksine"   {- com -}
                             "yksin"    {- instr -}
                             (case inf of
                                _ | G.isDistributive inf → "yksittäin"
                                  | otherwise            → "?"
                             )
           )
          , (2, \c → infForms inf c
                              -- singular
                              "kaksi"    {- nom -} "kaksi"    {- acc -}
                              "kahden"   {- gen -} "kahta"    {- ptv -}
                              "kahtena"  {- ess -} "kahdeksi" {- transl -}
                              "kahdessa" {- ine -} "kahdesta" {- ela -}
                              "kahteen"  {- ill -} "kahdella" {- ade -}
                              "kahdelta" {- abl -} "kahdelle" {- all -}
                              "kahdetta" {- abe -}
                              (case inf of
                                 _ | G.isInstructive    inf → "kahden"
                                   | G.isSuperEssive    inf → "kahtaalla"
                                   | G.isDelative       inf → "kahtaalta"
                                   | G.isSublative      inf → "kahtaalle"
                                   | G.isLative         inf → "kahtia"
                                   | G.isMultiplicative inf → "kahdesti"
                                   | otherwise              → "?"
                              )
                              -- plural
                              "kahdet"   {- nom -} "kahdet"   {- acc -}
                              "kaksien"  {- gen -} "kaksia"   {- ptv -}
                              "kaksina"  {- ess -} "kaksiksi" {- transl -}
                              "kaksissa" {- ine -} "kaksista" {- ela -}
                              "kaksiin"  {- ill -} "kaksilla" {- ade -}
                              "kaksilta" {- abl -} "kaksille" {- all -}
                              "kaksitta" {- abe -} "kaksine"  {- com -}
                              "kaksin"   {- instr -}
                              (case inf of
                                 _ | G.isDistributive inf → "kaksittain"
                                   | otherwise            → "?"
                              )
            )
          , (3, \c → infForms inf c
                              -- singular
                              "kolme"    {- nom -} "kolme"    {- acc -}
                              "kolmen"   {- gen -} "kolmea"   {- ptv -}
                              "kolmena"  {- ess -} "kolmeksi" {- transl -}
                              "kolmessa" {- ine -} "kolmesta" {- ela -}
                              "kolmeen"  {- ill -} "kolmella" {- ade -}
                              "kolmelta" {- abl -} "kolmelle" {- all -}
                              "kolmetta" {- abe -}
                              (case inf of
                                 _ | G.isInstructive    inf → "kolmen"
                                   | G.isLative         inf → "kolmia"
                                   | G.isMultiplicative inf → "kolmesti"
                                   | otherwise              → "?"
                              )
                              -- plural
                              "kolmet"   {- nom -} "kolmet"   {- acc -}
                              "kolmien"  {- gen -} "kolmia"   {- ptv -}
                              "kolmina"  {- ess -} "kolmiksi" {- transl -}
                              "kolmissa" {- ine -} "kolmista" {- ela -}
                              "kolmiin"  {- ill -} "kolmilla" {- ade -}
                              "kolmilta" {- abl -} "kolmille" {- all -}
                              "kolmitta" {- abe -} "kolmine"  {- com -}
                              "kolmin"   {- instr -}
                              (case inf of
                                 _ | G.isDistributive         inf → "kolmittain"
                                   | G.isDistributiveTemporal inf → "kolmisin"
                                   | otherwise                    → "?"
                              )
            )
          , (4, \c → infForms inf c
                              -- singular
                              "neljä"    {- nom -} "neljä"    {- acc -}
                              "neljän"   {- gen -} "neljää"   {- ptv -}
                              "neljänä"  {- ess -} "neljäksi" {- transl -}
                              "neljässä" {- ine -} "neljästä" {- ela -}
                              "neljään"  {- ill -} "neljällä" {- ade -}
                              "neljältä" {- abl -} "neljälle" {- all -}
                              "neljättä" {- abe -}
                              "?"        {- other -}
                              -- plural
                              "neljät"   {- nom -} "neljät"   {- acc -}
                              "neljien"  {- gen -} "neljiä"   {- ptv -}
                              "neljinä"  {- ess -} "neljiksi" {- transl -}
                              "neljissä" {- ine -} "neljistä" {- ela -}
                              "neljiin"  {- ill -} "neljillä" {- ade -}
                              "neljiltä" {- abl -} "neljille" {- all -}
                              "neljittä" {- abe -} "neljine"  {- com -}
                              "neljin"   {- instr -}
                              "?"        {- other -}
            )
          , (5, \c → infForms inf c
                              -- singular
                              "viisi"    {- nom -} "viisi"    {- acc -}
                              "viiden"   {- gen -} "viittä"   {- ptv -}
                              "viitenä"  {- ess -} "viideksi" {- transl -}
                              "viidessä" {- ine -} "viidestä" {- ela -}
                              "viiteen"  {- ill -} "viidellä" {- ade -}
                              "viideltä" {- abl -} "viidelle" {- all -}
                              "viidettä" {- abe -}
                              "?"        {- other -}
                              -- plural
                              "viidet"   {- nom -} "viidet"   {- acc -}
                              "viisien"  {- gen -} "viisiä"   {- ptv -}
                              "viisinä"  {- ess -} "viisiksi" {- transl -}
                              "viisissä" {- ine -} "viisistä" {- ela -}
                              "viisiin"  {- ill -} "viisillä" {- ade -}
                              "viisiltä" {- abl -} "viisille" {- all -}
                              "viisittä" {- abe -} "viisine"  {- com -}
                              "viisin"   {- instr -}
                              "?"        {- other -}
            )
          , (6, \c → infForms inf c
                              -- singular
                              "kuusi"    {- nom -} "kuusi"    {- acc -}
                              "kuuden"   {- gen -} "kuutta"   {- ptv -}
                              "kuutena"  {- ess -} "kuudeksi" {- transl -}
                              "kuudessa" {- ine -} "kuudesta" {- ela -}
                              "kuuteen"  {- ill -} "kuudella" {- ade -}
                              "kuudelta" {- abl -} "kuudelle" {- all -}
                              "kuudetta" {- abe -}
                              "?"        {- other -}
                              -- plural
                              "kuudet"   {- nom -} "kuudet"   {- acc -}
                              "kuusien"  {- gen -} "kuusia"   {- ptv -}
                              "kuusina"  {- ess -} "kuusiksi" {- transl -}
                              "kuusissa" {- ine -} "kuusista" {- ela -}
                              "kuusiin"  {- ill -} "kuusilla" {- ade -}
                              "kuusilta" {- abl -} "kuusille" {- all -}
                              "kuusitta" {- abe -} "kuusine"  {- com -}
                              "kuusin"   {- instr -}
                              "?"        {- other -}
            )
          , (7, \c → infForms inf c
                              -- singular
                              "seitsemän"   {- nom -} "seitsemän"   {- acc -}
                              "seitsemän"   {- gen -} "seitsemää"   {- ptv -}
                              "seitsemänä"  {- ess -} "seitsemäksi" {- transl -}
                              "seitsemässä" {- ine -} "seitsemästä" {- ela -}
                              "seitsemään"  {- ill -} "seitsemällä" {- ade -}
                              "seitsemältä" {- abl -} "seitsemälle" {- all -}
                              "seitsemättä" {- abe -}
                              "?"           {- other -}
                              -- plural
                              "seitsemät"   {- nom -} "seitsemät"   {- acc -}
                              "seitsemien"  {- gen -} "seitsemiä"   {- ptv -}
                              "seitseminä"  {- ess -} "seitsemiksi" {- transl -}
                              "seitsemissä" {- ine -} "seitsemistä" {- ela -}
                              "seitsemiin"  {- ill -} "seitsemillä" {- ade -}
                              "seitsemiltä" {- abl -} "seitsemille" {- all -}
                              "seitsemittä" {- abe -} "seitsemine"  {- com -}
                              "seitsemin"   {- instr -}
                              "?"           {- other -}
            )
          , (8, \c → infForms inf c
                              -- singular
                              "kahdeksan"   {- nom -} "kahdeksan"   {- acc -}
                              "kahdeksan"   {- gen -} "kahdeksaa"   {- ptv -}
                              "kahdeksana"  {- ess -} "kahdeksaksi" {- transl -}
                              "kahdeksassa" {- ine -} "kahdeksasta" {- ela -}
                              "kahdeksaan"  {- ill -} "kahdeksalla" {- ade -}
                              "kahdeksalta" {- abl -} "kahdeksalle" {- all -}
                              "kahdeksatta" {- abe -}
                              "?"           {- other -}
                              -- plural
                              "kahdeksat"   {- nom -} "kahdeksat"   {- acc -}
                              "kahdeksien"  {- gen -} "kahdeksia"   {- ptv -}
                              "kahdeksina"  {- ess -} "kahdeksiksi" {- transl -}
                              "kahdeksissa" {- ine -} "kahdeksista" {- ela -}
                              "kahdeksiin"  {- ill -} "kahdeksilla" {- ade -}
                              "kahdeksilta" {- abl -} "kahdeksille" {- all -}
                              "kahdeksitta" {- abe -} "kahdeksine"  {- com -}
                              "kahdeksin"   {- instr -}
                              "?"           {- other -}
            )
          , (9, \c → infForms inf c
                              -- singular
                              "yhdeksän"   {- nom -} "yhdeksän"   {- acc -}
                              "yhdeksän"   {- gen -} "yhdeksää"   {- ptv -}
                              "yhdeksänä"  {- ess -} "yhdeksäksi" {- transl -}
                              "yhdeksässä" {- ine -} "yhdeksästä" {- ela -}
                              "yhdeksään"  {- ill -} "yhdeksällä" {- ade -}
                              "yhdeksältä" {- abl -} "yhdeksälle" {- all -}
                              "yhdeksättä" {- abe -}
                              "?"          {- other -}
                              -- plural
                              "yhdeksät"   {- nom -} "yhdeksät"   {- acc -}
                              "yhdeksien"  {- gen -} "yhdeksiä"   {- ptv -}
                              "yhdeksinä"  {- ess -} "yhdeksiksi" {- transl -}
                              "yhdeksissä" {- ine -} "yhdeksistä" {- ela -}
                              "yhdeksiin"  {- ill -} "yhdeksillä" {- ade -}
                              "yhdeksiltä" {- abl -} "yhdeksille" {- all -}
                              "yhdeksittä" {- abe -} "yhdeksine"  {- com -}
                              "yhdeksin"   {- instr -}
                              "?"          {- other -}
            )

          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ → "toista" -- singular partitive ordinal 2
                        _ → infForms inf c
                              -- singular
                              "kymmenen"   {- nom -} "kymmenen"   {- acc -}
                              "kymmenen"   {- gen -} "kymmentä"   {- ptv -}
                              "kymmenenä"  {- ess -} "kymmeneksi" {- transl -}
                              "kymmenessä" {- ine -} "kymmenestä" {- ela -}
                              "kymmeneen"  {- ill -} "kymmenellä" {- ade -}
                              "kymmeneltä" {- abl -} "kymmenelle" {- all -}
                              "kymmenettä" {- abe -}
                              "?"          {- other -}
                              -- plural
                              "kymmenet"   {- nom -} "kymmenet"   {- acc -}
                              "kymmenien"  {- gen -} "kymmeniä"   {- ptv -}
                              "kymmeninä"  {- ess -} "kymmeniksi" {- transl -}
                              "kymmenissä" {- ine -} "kymmenistä" {- ela -}
                              "kymmeniin"  {- ill -} "kymmenillä" {- ade -}
                              "kymmeniltä" {- abl -} "kymmenille" {- all -}
                              "kymmenittä" {- abe -} "kymmenine"  {- com -}
                              "kymmenin"   {- instr -}
                              "?"          {- other -}
            )
          , (100, \c → case c of
                         CtxMul _ (Lit n) _
                             | n ≤ 9        → "sataa"
                         _ → infForms inf c
                               -- singular
                               "sata"    {- nom -} "sata"    {- acc -}
                               "sadan"   {- gen -} "sataa"   {- ptv -}
                               "satana"  {- ess -} "sadaksi" {- transl -}
                               "sadassa" {- ine -} "sadasta" {- ela -}
                               "sataan"  {- ill -} "sadalla" {- ade -}
                               "sadalta" {- abl -} "sadalle" {- all -}
                               "sadatta" {- abe -}
                               "?"       {- other -}
                               -- plural
                               "sadat"    {- nom -} "sadat"    {- acc -}
                               "satojen"  {- gen -} "satoja"   {- ptv -}
                               "satoina"  {- ess -} "sadoiksi" {- transl -}
                               "sadoissa" {- ine -} "sadoista" {- ela -}
                               "satoihin" {- ill -} "sadoilla" {- ade -}
                               "sadoilta" {- abl -} "sadoille" {- all -}
                               "sadoitta" {- abe -} "satoine"  {- com -}
                               "sadoin"   {- instr -}
                               "?"        {- other -}
            )
          , (1000, \c → case c of
                          CtxMul {} → "tuhatta"
                          _ → infForms inf c
                                -- singular
                                "tuhat"      {- nom -} "tuhat"      {- acc -}
                                "tuhannen"   {- gen -} "tuhatta"    {- ptv -}
                                "tuhantena"  {- ess -} "tuhanneksi" {- transl -}
                                "tuhannessa" {- ine -} "tuhannesta" {- ela -}
                                "tuhanteen"  {- ill -} "tuhannella" {- ade -}
                                "tuhannelta" {- abl -} "tuhannelle" {- all -}
                                "tuhannetta" {- abe -}
                                "?"          {- other -}
                                -- plural
                                "tuhannet"   {- nom -} "tuhannet"   {- acc -}
                                "tuhansien"  {- gen -} "tuhansia"   {- ptv -}
                                "tuhansina"  {- ess -} "tuhansiksi" {- transl -}
                                "tuhansissa" {- ine -} "tuhansista" {- ela -}
                                "tuhansiin"  {- ill -} "tuhansilla" {- ade -}
                                "tuhansilta" {- abl -} "tuhansille" {- all -}
                                "tuhansitta" {- abe -} "tuhansine"  {- com -}
                                "tuhansin"   {- instr -}
                                "?"          {- other -}
            )
          ]

ordinalRepr ∷ (Inflection i) ⇒ i → Exp i → Maybe Text
ordinalRepr = render defaultRepr
              { reprValue = \inf n → M.lookup n (syms inf)
              , reprScale = BN.pelletierRepr
                              (BN.quantityName "iljoona" "iljoonaa")
                              (BN.quantityName "iljardi" "iljardia")
                              []
              , reprAdd   = Just $ \_ _ _ → ""
              , reprMul   = Just $ \_ _ _ → ""
              }
    where
      syms inf =
          M.fromList
          [ (0, \c → infForms inf c
                              -- singular
                              "nollas"      {- nom -} "nollas"      {- acc -}
                              "nollannen"   {- gen -} "nollatta"    {- ptv -}
                              "nollantena"  {- ess -} "nollanneksi" {- transl -}
                              "nollannessa" {- ine -} "nollannesta" {- ela -}
                              "nollanteen"  {- ill -} "nollannella" {- ade -}
                              "nollannelta" {- abl -} "nollannelle" {- all -}
                              "nollannetta" {- abe -}
                              "?"           {- other -}
                              -- plural
                              "nollannet"   {- nom -} "nollannet"   {- acc -}
                              "nollansien"  {- gen -} "nollansia"   {- ptv -}
                              "nollansina"  {- ess -} "nollansiksi" {- transl -}
                              "nollansissa" {- ine -} "nollansista" {- ela -}
                              "nollansiin"  {- ill -} "nollansilla" {- ade -}
                              "nollansilta" {- abl -} "nollansille" {- all -}
                              "nollansitta" {- abe -} "nollansine"  {- com -}
                              "nollansin"   {- instr -}
                              "?"           {- other -}
            )
          , (1, \c → case c of
                       CtxAdd _ (Lit 10) _ →
                           infForms inf c
                              -- singular
                              "yhdes"      {- nom -} "yhdes"      {- acc -}
                              "yhdennen"   {- gen -} "yhdettä"    {- ptv -}
                              "yhdennessä" {- ess -} "yhdenneksi" {- transl -}
                              "yhdennessä" {- ine -} "yhdennestä" {- ela -}
                              "yhdenteen"  {- ill -} "yhdennellä" {- ade -}
                              "yhdenneltä" {- abl -} "yhdennelle" {- all -}
                              "yhdennettä" {- abe -}
                              "?"          {- other -}
                              -- plural
                              "yhdennet"   {- nom -} "yhdennet"   {- acc -}
                              "yhdensien"  {- gen -} "yhdensiä"   {- ptv -}
                              "yhdensinä"  {- ess -} "yhdensiksi" {- transl -}
                              "yhdensissä" {- ine -} "yhdensistä" {- ela -}
                              "yhdensiin"  {- ill -} "yhdensillä" {- ade -}
                              "yhdensitlä" {- abl -} "yhdensiin"  {- all -}
                              "yhdensittä" {- abe -} "yhdensine"  {- com -}
                              "yhdensin"   {- instr -}
                              "?"          {- other -}
                       _ → infForms inf c
                              -- singular
                              "ensimmäinen"   {- nom -} "ensimmäinen"   {- acc -}
                              "ensimmäisen"   {- gen -} "ensimmäistä"   {- ptv -}
                              "ensimmmäisenä" {- ess -} "ensimmäiseksi" {- transl -}
                              "ensimmäisessä" {- ine -} "ensimmäisestä" {- ela -}
                              "ensimmäiseen"  {- ill -} "ensimmäisellä" {- ade -}
                              "ensimmäiseltä" {- abl -} "ensimmäiselle" {- all -}
                              "ensimmäisettä" {- abe -}
                              "?"             {- other -}
                              -- plural
                              "ensimmäiset"   {- nom -} "ensimmäiset"   {- acc -}
                              "ensimmäisten"  {- gen -} "ensimmäisiä"   {- ptv -}
                              "ensimmäisinä"  {- ess -} "ensimmäisiksi" {- transl -}
                              "ensimmäisissä" {- ine -} "ensimmäisistä" {- ela -}
                              "ensimmäisiin"  {- ill -} "ensimmäisillä" {- ade -}
                              "ensimmäisiltä" {- abl -} "ensimmäisille" {- all -}
                              "ensimmäisittä" {- abe -} "ensimmäisine"  {- com -}
                              "ensimmäisin"   {- instr -}
                              "?"             {- other -}
            )
          , (2, \c → case c of
                       CtxAdd _ (Lit 10) _ →
                           infForms inf c
                              -- singular
                              "kahdes"      {- nom -} "kahdes"      {- acc -}
                              "kahdennen"   {- gen -} "kahdetta"    {- ptv -}
                              "kahdentena"  {- ess -} "kahdenneksi" {- transl -}
                              "kahdennessa" {- ine -} "kahdennesta" {- ela -}
                              "kahdenteen"  {- ill -} "kahdennella" {- ade -}
                              "kahdennelta" {- abl -} "kahdennelle" {- all -}
                              "kahdennetta" {- abe -}
                              "?"           {- other -}
                              -- plural
                              "kahdennet"   {- nom -} "kahdennet"   {- acc -}
                              "kahdensien"  {- gen -} "kahdensia"   {- ptv -}
                              "kahdensina"  {- ess -} "kahdensiksi" {- transl -}
                              "kahdensissa" {- ine -} "kahdensista" {- ela -}
                              "kahdensiin"  {- ill -} "kahdensilla" {- ade -}
                              "kahdensilta" {- abl -} "kahdensille" {- all -}
                              "kahdensitta" {- abe -} "kahdensine"  {- com -}
                              "kahdensin"   {- instr -}
                              "?"           {- other -}
                       _ → infForms inf c
                              -- singular
                              "toinen"   {- nom -} "toinen"   {- acc -}
                              "toisen"   {- gen -} "toista"   {- ptv -}
                              "toisena"  {- ess -} "toiseksi" {- transl -}
                              "toisessa" {- ine -} "toisesta" {- ela -}
                              "toiseen"  {- ill -} "toisella" {- ade -}
                              "toiselta" {- abl -} "toiselle" {- all -}
                              "toisetta" {- abe -}
                              "?"        {- other -}
                              -- plural
                              "toiset"   {- nom -} "toiset"   {- acc -}
                              "toisten"  {- gen -} "toisia"   {- ptv -}
                              "toisina"  {- ess -} "toisiksi" {- transl -}
                              "toisissa" {- ine -} "toisista" {- ela -}
                              "toisiin"  {- ill -} "toisilla" {- ade -}
                              "toisilta" {- abl -} "toisille" {- all -}
                              "toisitta" {- abe -} "toisine"  {- com -}
                              "toisin"   {- instr -}
                              "?"        {- other -}
            )
          , (3, \c → infForms inf c
                              -- singular
                              "kolmas"      {- nom -} "kolmas"      {- acc -}
                              "kolmannen"   {- gen -} "kolmatta"    {- ptv -}
                              "kolmantena"  {- ess -} "kolmanneksi" {- transl -}
                              "kolmannessa" {- ine -} "kolmannesta" {- ela -}
                              "kolmanteen"  {- ill -} "kolmannella" {- ade -}
                              "kolmannelta" {- abl -} "kolmannelle" {- all -}
                              "kolmannetta" {- abe -}
                              "?"           {- other -}
                              -- plural
                              "kolmannet"   {- nom -} "kolmannet"   {- acc -}
                              "kolmansien"  {- gen -} "kolmansia"   {- ptv -}
                              "kolmansina"  {- ess -} "kolmansiksi" {- transl -}
                              "kolmansissa" {- ine -} "kolmansista" {- ela -}
                              "kolmansiin"  {- ill -} "kolmansilla" {- ade -}
                              "kolmansilta" {- abl -} "kolmansille" {- all -}
                              "kolmansitta" {- abe -} "kolmansine"  {- com -}
                              "kolmansin"   {- instr -}
                              "?"           {- other -}
            )
          , (4, \c → infForms inf c
                              -- singular
                              "neljäs"      {- nom -} "neljäs"      {- acc -}
                              "neljännen"   {- gen -} "neljättä"    {- ptv -}
                              "neljäntenä"  {- ess -} "neljänneksi" {- transl -}
                              "neljännessä" {- ine -} "neljännestä" {- ela -}
                              "neljänteen"  {- ill -} "neljännellä" {- ade -}
                              "neljänneltä" {- abl -} "neljännelle" {- all -}
                              "neljännettä" {- abe -}
                              (case inf of
                                 _ | G.isComitative  inf → "neljänsine"
                                   | G.isInstructive inf → "neljänsin"
                                   | otherwise → "?"
                              )
                              -- plural
                              "neljännet"   {- nom -} "neljännet"   {- acc -}
                              "neljänsien"  {- gen -} "neljänsiä"   {- ptv -}
                              "neljänsinä"  {- ess -} "neljänsiksi" {- transl -}
                              "neljänsissä" {- ine -} "neljänsistä" {- ela -}
                              "neljänsiin"  {- ill -} "neljänsillä" {- ade -}
                              "neljänsiltä" {- abl -} "neljänsille" {- all -}
                              "neljänsittä" {- abe -} "neljänsine"  {- com -}
                              "neljänsin"   {- instr -}
                              "?"           {- other -}
            )
          , (5, \c → infForms inf c
                              -- singular
                              "viides"      {- nom -} "viides"      {- acc -}
                              "viidennen"   {- gen -} "viidettä"    {- ptv -}
                              "viidentenä"  {- ess -} "viidenneksi" {- transl -}
                              "viidennessä" {- ine -} "viidennestä" {- ela -}
                              "viidenteen"  {- ill -} "viidennellä" {- ade -}
                              "viidenneltä" {- abl -} "viidennelle" {- all -}
                              "viidennettä" {- abe -}
                              "?"           {- other -}
                              -- plural
                              "viidennet"   {- nom -} "viidennet"   {- acc -}
                              "viidensien"  {- gen -} "viidensiä"   {- ptv -}
                              "viidensinä"  {- ess -} "viidensiksi" {- transl -}
                              "viidensissä" {- ine -} "viidensistä" {- ela -}
                              "viidensiin"  {- ill -} "viidensillä" {- ade -}
                              "viidensiltä" {- abl -} "viidensille" {- all -}
                              "viidensittä" {- abe -} "viidensine"  {- com -}
                              "viidensin"   {- instr -}
                              "?"           {- other -}
            )
          , (6, \c → infForms inf c
                              -- singular
                              "kuudes"      {- nom -} "kuudes"      {- acc -}
                              "kuudennen"   {- gen -} "kuudetta"    {- ptv -}
                              "kuudentena"  {- ess -} "kuudenneksi" {- transl -}
                              "kuudennessa" {- ine -} "kuudennesta" {- ela -}
                              "kuudenteen"  {- ill -} "kuudennella" {- ade -}
                              "kuudennelta" {- abl -} "kuudennelle" {- all -}
                              "kuudennetta" {- abe -}
                              "?"           {- other -}
                              -- plural
                              "kuudennet"   {- nom -} "kuudennet"   {- acc -}
                              "kuudensien"  {- gen -} "kuudensia"   {- ptv -}
                              "kuudensina"  {- ess -} "kuudensiksi" {- transl -}
                              "kuudensissa" {- ine -} "kuudensista" {- ela -}
                              "kuudensiin"  {- ill -} "kuudensilla" {- ade -}
                              "kuudensilta" {- abl -} "kuudensille" {- all -}
                              "kuudensitta" {- abe -} "kuudensine"  {- com -}
                              "kuudensin"   {- instr -}
                              "?"           {- other -}
            )
          , (7, \c → infForms inf c
                              -- singular
                              "seitsemäs"      {- nom -} "seitsemäs"      {- acc -}
                              "seitsemännen"   {- gen -} "seitsemättä"    {- ptv -}
                              "seitsemäntenä"  {- ess -} "seitsemänneksi" {- transl -}
                              "seitsemännessä" {- ine -} "seitsemännestä" {- ela -}
                              "seitsemänteen"  {- ill -} "seitsemännellä" {- ade -}
                              "seitsemänneltä" {- abl -} "seitsemännelle" {- all -}
                              "seitsemännettä" {- abe -}
                              "?"              {- other -}
                              -- plural
                              "seitsemännet"   {- nom -} "seitsemännet"   {- acc -}
                              "seitsemänsien"  {- gen -} "seitsemänsiä"   {- ptv -}
                              "seitsemänsinä"  {- ess -} "seitsemänsiksi" {- transl -}
                              "seitsemänsissä" {- ine -} "seitsemänsistä" {- ela -}
                              "seitsemänsiin"  {- ill -} "seitsemänsillä" {- ade -}
                              "seitsemänsiltä" {- abl -} "seitsemänsille" {- all -}
                              "seitsemänsittä" {- abe -} "seitsemänsine"  {- com -}
                              "seitsemänsin"   {- instr -}
                              "?"              {- other -}
            )
          , (8, \c → infForms inf c
                              -- singular
                              "kahdeksas"      {- nom -} "kahdeksas"      {- acc -}
                              "kahdeksannen"   {- gen -} "kahdeksatta"    {- ptv -}
                              "kahdeksantena"  {- ess -} "kahdeksanneksi" {- transl -}
                              "kahdeksannessa" {- ine -} "kahdeksannesta" {- ela -}
                              "kahdeksanteen"  {- ill -} "kahdeksannella" {- ade -}
                              "kahdeksannelta" {- abl -} "kahdeksannelle" {- all -}
                              "kahdeksannetta" {- abe -}
                              "?"              {- other -}
                              -- plural
                              "kahdeksannet"   {- nom -} "kahdeksannet"   {- acc -}
                              "kahdeksansien"  {- gen -} "kahdeksansia"   {- ptv -}
                              "kahdeksansina"  {- ess -} "kahdeksansiksi" {- transl -}
                              "kahdeksansissa" {- ine -} "kahdeksansista" {- ela -}
                              "kahdeksansiin"  {- ill -} "kahdeksansilla" {- ade -}
                              "kahdeksansilta" {- abl -} "kahdeksansille" {- all -}
                              "kahdeksansitta" {- abe -} "kahdeksansine"  {- com -}
                              "kahdeksansin"   {- instr -}
                              "?"              {- other -}
            )
          , (9, \c → infForms inf c
                              -- singular
                              "yhdeksäs"      {- nom -} "yhdeksäs"      {- acc -}
                              "yhdeksännen"   {- gen -} "yhdeksättä"    {- ptv -}
                              "yhdeksäntenä"  {- ess -} "yhdeksänneksi" {- transl -}
                              "yhdeksännessä" {- ine -} "yhdeksännestä" {- ela -}
                              "yhdeksänteen"  {- ill -} "yhdeksännellä" {- ade -}
                              "yhdeksänneltä" {- abl -} "yhdeksännelle" {- all -}
                              "yhdeksännettä" {- abe -}
                              "?"             {- other -}
                              -- plural
                              "yhdeksännet"   {- nom -} "yhdeksännet"   {- acc -}
                              "yhdeksänsien"  {- gen -} "yhdeksänsiä"   {- ptv -}
                              "yhdeksänsinä"  {- ess -} "yhdeksänsiksi" {- transl -}
                              "yhdeksänsissä" {- ine -} "yhdeksänsistä" {- ela -}
                              "yhdeksänsiin"  {- ill -} "yhdeksänsillä" {- ade -}
                              "yhdeksänsiltä" {- abl -} "yhdeksänsille" {- all -}
                              "yhdeksänsittä" {- abe -} "yhdeksänsine"  {- com -}
                              "yhdeksänsin"   {- instr -}
                              "?"             {- other -}
            )
          , (10, \c → case c of
                        CtxAdd _ (Lit _) _ → "toista" -- singular partitive ordinal 2
                        _ → infForms inf c
                                 -- singular
                                 "kymmenes"      {- nom -} "kymmenes"      {- acc -}
                                 "kymmenennen"   {- gen -} "kymmenettä"    {- ptv -}
                                 "kymmenentenä"  {- ess -} "kymmenenneksi" {- transl -}
                                 "kymmenennessä" {- ine -} "kymmenennestä" {- ela -}
                                 "kymmenenteen"  {- ill -} "kymmenennellä" {- ade -}
                                 "kymmenenneltä" {- abl -} "kymmenennelle" {- all -}
                                 "kymmenennettä" {- abe -}
                                 "?"             {- other -}
                                 -- plural
                                 "kymmenennet"   {- nom -} "kymmenennet"   {- acc -}
                                 "kymmenensien"  {- gen -} "kymmenensiä"   {- ptv -}
                                 "kymmenensinä"  {- ess -} "kymmenensiksi" {- transl -}
                                 "kymmenensissä" {- ine -} "kymmenensistä" {- ela -}
                                 "kymmenensiin"  {- ill -} "kymmenensillä" {- ade -}
                                 "kymmenensiltä" {- abl -} "kymmenensille" {- all -}
                                 "kymmenensittä" {- abe -} "kymmenensine"  {- com -}
                                 "kymmenensin"   {- instr -}
                                 "?"             {- other -}
            )
          , (100, \c → case c of
                         _ → infForms inf c
                               -- singular
                               "sadas"      {- nom -} "sadas"      {- acc -}
                               "sadannen"   {- gen -} "sadatta"    {- ptv -}
                               "sadantena"  {- ess -} "sadanneksi" {- transl -}
                               "sadannessa" {- ine -} "sadannesta" {- ela -}
                               "sadanteen"  {- ill -} "sadannella" {- ade -}
                               "sadannelta" {- abl -} "sadannelle" {- all -}
                               "sadannetta" {- abe -}
                               "?"          {- other -}
                               -- plural
                               "sadannet"   {- nom -} "sadannet"   {- acc -}
                               "sadansien"  {- gen -} "sadansia"   {- ptv -}
                               "sadansina"  {- ess -} "sadansiksi" {- transl -}
                               "sadansissa" {- ine -} "sadansista" {- ela -}
                               "sadansiin"  {- ill -} "sadansilla" {- ade -}
                               "sadansilta" {- abl -} "sadansille" {- all -}
                               "sadansitta" {- abe -} "sadansine"  {- com -}
                               "sadansin"   {- instr -}
                               "?"          {- other -}
            )
          , (1000, \c → case c of
                         _ → infForms inf c
                               -- singular
                               "tuhannes"      {- nom -} "tuhannes"      {- acc -}
                               "tuhannennen"   {- gen -} "tuhannetta"    {- ptv -}
                               "tuhannentena"  {- ess -} "tuhannenneksi" {- transl -}
                               "tuhannennessa" {- ine -} "tuhannennesta" {- ela -}
                               "tuhannenteen"  {- ill -} "tuhannennella" {- ade -}
                               "tuhannennelta" {- abl -} "tuhannennelle" {- all -}
                               "tuhannennetta" {- abe -}
                               "?"             {- other -}
                               -- plural
                               "tuhannennet"   {- nom -} "tuhannennet"   {- acc -}
                               "tuhannensien"  {- gen -} "tuhannensia"   {- ptv -}
                               "tuhannensina"  {- ess -} "tuhannensiksi" {- transl -}
                               "tuhannensissa" {- ine -} "tuhannensista" {- ela -}
                               "tuhannensiin"  {- ill -} "tuhannensilla" {- ade -}
                               "tuhannensilta" {- abl -} "tuhannensille" {- all -}
                               "tuhannensitta" {- abe -} "tuhannensine"  {- com -}
                               "tuhannensin"   {- instr -}
                               "?"             {- other -}
            )
          ]

infForms ∷ (Inflection i)
         ⇒ i → Ctx (Exp i)
         → Text → Text → Text → Text → Text → Text → Text → Text → Text → Text
         → Text → Text → Text → Text → Text → Text → Text → Text → Text → Text
         → Text → Text → Text → Text → Text → Text → Text → Text → Text → Text → Text
infForms inf _
         s_nom s_acc s_gen s_ptv s_ess s_transl
         s_ine s_ela s_ill s_ade s_abl s_all s_abe
         s_other
         p_nom p_acc p_gen p_ptv p_ess p_transl
         p_ine p_ela p_ill p_ade p_abl p_all p_abe p_com p_instr
         p_other
    | G.isSingular inf = singularForms
    | G.isPlural   inf = pluralForms
    | otherwise        = "?"
  where
    singularForms
        | G.isNominative       inf = s_nom
        | G.isAccusative       inf = s_acc
        | G.isGenitive         inf = s_gen
        | G.isPartitive        inf = s_ptv
        | G.isEssive           inf = s_ess
        | G.isTranslative      inf = s_transl
        | G.isLocativeInessive inf = s_ine
        | G.isLocativeElative  inf = s_ela
        | G.isLocativeIllative inf = s_ill
        | G.isLocativeAdessive inf = s_ade
        | G.isLocativeAblative inf = s_abl
        | G.isLocativeAllative inf = s_all
        | G.isAbessive         inf = s_abe
        | otherwise                = s_other
    pluralForms
        | G.isNominative       inf = p_nom
        | G.isAccusative       inf = p_acc
        | G.isGenitive         inf = p_gen
        | G.isPartitive        inf = p_ptv
        | G.isEssive           inf = p_ess
        | G.isTranslative      inf = p_transl
        | G.isLocativeInessive inf = p_ine
        | G.isLocativeElative  inf = p_ela
        | G.isLocativeIllative inf = p_ill
        | G.isLocativeAdessive inf = p_ade
        | G.isLocativeAblative inf = p_abl
        | G.isLocativeAllative inf = p_all
        | G.isAbessive         inf = p_abe
        | G.isComitative       inf = p_com
        | G.isInstructive      inf = p_instr
        | otherwise                = p_other
