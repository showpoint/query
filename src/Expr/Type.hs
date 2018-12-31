{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Expr.Type where
  import Core.Types

  data ExprF a
    = Lit String
    | Var Name
    | Let Name a
    | Add a a
    | Next a a
    deriving (Show)

  newtype Expr a = Expr { unExpr :: ExprF (Expr a)}
    deriving (Show)

  deriving instance Functor ExprF
  deriving instance Foldable ExprF
  deriving instance Traversable ExprF
