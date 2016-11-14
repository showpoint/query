{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Expr where
  import Core.Types

  data Expr
    = Lit String
    | Var Name
    | Let Name Expr
    | Add Expr Expr
    | Next Expr Expr
    deriving (Show)

  class LiteralC l r where
    lit :: l -> r

  class NameC n r where
    var :: n -> r
    let_ :: n -> r -> r

  class ExprC r where
    add :: r -> r -> r
    next :: r -> r -> r

  instance LiteralC String Expr where
    lit = Lit

  instance NameC Name Expr where
    var = Var
    let_ = Let

  instance ExprC Expr where
    add = Add
    next = Next

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

  --exec :: ExprC a => Expr -> a
  exec (Lit n) = lit n
  exec (Var n) = var n
  exec (Let n e) = let_ n (exec e)
  exec (Add l r) = add (exec l) (exec r)
  exec (Next l r) = next (exec l) (exec r)
