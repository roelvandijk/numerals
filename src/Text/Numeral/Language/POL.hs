{-|
[@ISO639-1@]        pl

[@ISO639-2@]        pol

[@ISO639-3@]        pol

[@Native name@]     język polski

[@English name@]    Polish
-}

module Text.Numeral.Language.POL
    ( -- * Language entry
      entry
      -- * Conversions
    , cardinal
      -- * Structure
    , struct
      -- * Bounds
    , bounds
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Function ( fix )
import qualified "containers" Data.Map as M ( fromList, lookup )
import           "this" Text.Numeral
import qualified "this" Text.Numeral.BigNum as BN
import           "this" Text.Numeral.Misc ( dec )
import "this" Text.Numeral.Entry
import "text" Data.Text ( Text )

-------------------------------------------------------------------------------
-- POL
-------------------------------------------------------------------------------

entry :: Entry
entry = emptyEntry
    { entIso639_1    = Just "pl"
    , entIso639_2    = ["pol"]
    , entIso639_3    = Just "pol"
    , entNativeNames = ["język polski"]
    , entEnglishName = Just "Polish"
    , entCardinal    = Just Conversion
                       { toNumeral   = cardinal
                       , toStructure = struct
                       }
    }

-- | liczebniki główne
cardinal :: (Integral a) => Inflection -> a -> Maybe Text
cardinal inf = cardinalRepr inf . struct

struct :: (Integral a) => a -> Exp
struct = pos
       $ fix
       $ findRule (   0, lit               )
                [ (  11, add    10      L  )
                , (  20, step   10   10 R L)
                , ( 100, step  100   10 R L)
                , (1000, step 1000 1000 R L)
                ]
                  (dec 6 - 1)
         `combine` pelletierScale R L BN.rule

bounds :: (Integral a) => (a, a)
bounds = let x = dec 60000 - 1 in (negate x, x)

cardinalRepr :: Inflection -> Exp -> Maybe Text
cardinalRepr = render defaultRepr
               { reprValue = \_ n -> M.lookup n syms
               , reprScale = BN.pelletierRepr (quantityRepr "ilion"  "iliony"  "ilionów")
                                              (quantityRepr "iliard" "iliardy" "iliardów")
                                              []
               , reprAdd   = Just (⊞)
               , reprMul   = Just (⊡)
               }
    where
      (Lit n ⊞ Lit 10) _ | n <= 9 = ""
      (_     ⊞ _     ) _          = " "

      (_ ⊡ Lit 10 ) _ = ""
      (_ ⊡ Lit 100) _ = ""
      (_ ⊡ _      ) _ = " "

      quantityRepr :: Text -> Text -> Text -> BN.PostfixRepr
      quantityRepr s p1 p2 _ ctx =
          case ctx of
            CtxMul _ (Lit 1) _ -> s
            CtxMul _ (Lit n) _ | n <= 4 -> p1
            CtxMul {}          -> p2
            _                  -> s

      syms =
          M.fromList
          [ (0,  const                         "zero")
          , (1,  \c -> case c of
                        CtxAdd _ (Lit 10)  _ -> "jede"
                        _                    -> "jeden"
            )
          , (2,  \c -> case c of
                        CtxMul _ (Lit 100) _ -> "dwie"
                        _                    -> "dwa"
            )
          , (3, const                          "trzy")
          , (4,  \c -> case c of
                        CtxAdd _ (Lit 10)  _ -> "czter"
                        CtxMul _ (Lit 10)  _ -> "czter"
                        _                    -> "cztery"
            )
          , (5,  \c -> case c of
                        CtxAdd _ (Lit 10)  _ -> "pięt"
                        _                    -> "pięć"
            )
          , (6,  \c -> case c of
                        CtxAdd _ (Lit 10)  _ -> "szes"
                        _                    -> "sześć"
            )
          , (7,  const                         "siedem")
          , (8,  const                         "osiem")
          , (9,  \c -> case c of
                        CtxAdd _ (Lit 10)  _ -> "dziewięt"
                        _                    -> "dziewięć"
            )
          , (10,  \c -> case c of
                         CtxAdd R (Lit n) _
                             | n <= 9 -> "naście"
                         CtxMul R (Lit n) _
                             | n == 2     -> "dzieścia"
                             | n >= 5     -> "dziesiąt"
                             | otherwise -> "dzieści"
                         _               -> "dziesięć"
            )
          , (100, \c -> case c of
                         CtxMul _ (Lit n) _
                             | n == 2 -> "ście"
                             | n <= 4 -> "sta"
                             | n <= 9 -> "set"
                         _            -> "sto"
            )
          , (1000, \c -> case c of
                          CtxMul _ (Lit n) _
                              | n <= 4 -> "tysiące"
                          CtxMul {}    -> "tysięcy"
                          _            -> "tysiąc"
            )
          ]
