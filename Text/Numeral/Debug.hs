{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Text.Numeral.Debug where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( forM_ )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Monoid.Unicode   ( (⊕) )

-- from numerals:
import Text.Numeral
import qualified Text.Numeral.Language.NL as NL
import qualified Text.Numeral.Language.DE as DE
import qualified Text.Numeral.Language.FR as FR
import qualified Text.Numeral.Language.EN as EN
import qualified Text.Numeral.Language.EO as EO
import qualified Text.Numeral.Language.JA as JA


--------------------------------------------------------------------------------
-- Debug and test stuff
--------------------------------------------------------------------------------

test ∷ (Rules, Repr String) → [Integer] → IO ()
test (rules, repr) xs = forM_ xs $ f ∘ ( test' rules
                                                repr
                                          ∷ Integer → (Integer, Val Exp, Maybe String)
                                        )
    where
      f (n, e, ms) = putStrLn $ show n ⊕ " - " ⊕ show e ⊕ " - " ⊕ maybe "error" id ms

test' ∷ (Num α, Monoid s, IsString s)
     ⇒ Rules → Repr s → Integer → (Integer, Val α, Maybe s)
test' rules repr n = ( n
                     , deconstruct rules n
                     , cardinal repr $ deconstruct rules n
                     )
