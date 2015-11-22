module Text.Numeral.Render.Utils
    ( addCtx
    , mulCtx
    , outsideCtx
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "this" Text.Numeral


--------------------------------------------------------------------------------
-- Utility functions for rendering numerals
--------------------------------------------------------------------------------

addCtx :: Integer -> s -> (Ctx Exp -> s) -> Ctx Exp -> s
addCtx val match other ctx =
    case ctx of
      CtxAdd _ (Lit n) _ | val == n -> match
      _ -> other ctx

mulCtx :: Integer -> s -> (Ctx Exp -> s) -> Ctx Exp -> s
mulCtx val match other ctx =
    case ctx of
      CtxMul _ (Lit n) _ | val == n -> match
      _ -> other ctx

outsideCtx :: Side -> s -> (Ctx Exp -> s) -> Ctx Exp -> s
outsideCtx side match other ctx =
    if isOutside side ctx
    then match
    else other ctx
