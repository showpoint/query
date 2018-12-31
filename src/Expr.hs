{-# LANGUAGE FlexibleContexts #-}
module Expr (
  exec,
  module X
    ) where
  import Core.Types as X
  import Expr.Deep as X
  import Expr.Expr as X
  import Expr.Literal as X
  import Expr.Name as X
  import Expr.Type as X

  exec :: (LiteralC String a, NameC Name a, ExprC a) => Expr a -> a
  exec = exec' . unExpr
    where
      exec' (Lit n) = lit n
      exec' (Var n) = var n
      exec' (Let n e) = let_ n (exec e)
      exec' (Add l r) = add (exec l) (exec r)
      exec' (Next l r) = next (exec l) (exec r)
