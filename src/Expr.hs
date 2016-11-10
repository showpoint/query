--{-# LANGUAGE DeriveFunctor #-}
module Expr where
  import Core.Types

  data ExprF a
    = And a a
    | Or a a
    | Not a
    deriving (Show)

  data Expr a = Expr { unExpr :: ExprF (Expr a) }
    deriving (Show)
