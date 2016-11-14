{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Expr where
  import Core.Types

  data ExprF a
    = Lit String
    | Var Name
    | Let Name a
    | Add a a
    | Next a a
    deriving (Show)

  data Expr a = Expr { unExpr :: ExprF (Expr a)}

  deriving instance Functor ExprF
  deriving instance Foldable ExprF
  deriving instance Traversable ExprF

  class LiteralC l r where
    lit :: l -> r

  class NameC n r where
    var :: n -> r
    let_ :: n -> r -> r

  class ExprC r where
    add :: r -> r -> r
    next :: r -> r -> r

  instance LiteralC String (Expr a) where
    lit = Expr . Lit

  instance NameC Name (Expr a) where
    var = Expr . Var
    let_ n = Expr . Let n

  instance ExprC (Expr a) where
    add l = Expr . Add l
    next l = Expr . Next l

  instance LiteralC String String where
    lit = show

  instance {-# OVERLAPPABLE #-} LiteralC a String where
    lit _ = "lit @"

  instance NameC Name String where
    var n = n
    let_ n r = n ++ " = " ++ r

  instance {-# OVERLAPPABLE #-} NameC a String where
    var _ = "var @"
    let_ _ r = "@ = " ++ r

  instance ExprC String where
    add l r = l ++ " + " ++ r
    next l r = l ++ "; " ++ r

  exec :: (LiteralC String a, NameC Name a, ExprC a) => Expr a -> a
  exec = exec' . unExpr
    where
      exec' (Lit n) = lit n
      exec' (Var n) = var n
      exec' (Let n e) = let_ n (exec e)
      exec' (Add l r) = add (exec l) (exec r)
      exec' (Next l r) = next (exec l) (exec r)
