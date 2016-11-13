{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Expr where
  import Core.Types

  data Expr
    = Lit String
    | Var Name
    | Let Name Expr
    | Add Expr Expr
    | Next Expr Expr
    deriving (Show)

  class ExprC r where
    lit :: String -> r
    var :: Name -> r
    let_ :: Name -> r -> r
    add :: r -> r -> r
    next :: r -> r -> r

  instance ExprC Expr where
    lit = Lit
    var = Var
    let_ = Let
    add = Add
    next = Next

  instance ExprC String where
    lit = show
    var n = n
    let_ n r = n ++ " = " ++ r
    add l r = l ++ " + " ++ r
    next l r = l ++ "; " ++ r

  exec :: ExprC a => Expr -> a
  exec (Lit n) = lit n
  exec (Var n) = var n
  exec (Let n e) = let_ n (exec e)
  exec (Add l r) = add (exec l) (exec r)
  exec (Next l r) = next (exec l) (exec r)
