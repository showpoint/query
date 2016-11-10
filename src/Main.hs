{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Main where
  import Core.Types

  class ExprC r where
    lit :: String -> r
    var :: Name -> r
    let_ :: Name -> r -> r
    add :: r -> r -> r

  class QueryC r where
    recQ :: r -> r
    anyQ :: r
    andQ :: r -> r -> r
    orQ :: r -> r -> r

  instance ExprC String where
    lit c = c
    var n = n
    let_ n r = n ++ " = " ++ r
    add l r = l ++ " + " ++ r

  data Expr
    = Lit String
    | Var Name
    | Let Name Expr
    | Add Expr Expr
    deriving (Show)

  data Query a
    = RecQ a
    | AnyQ
    | AndQ a a
    | OrQ a a

  instance ExprC (Expr -> Bool) where
    lit c = \case
      Lit c' -> c == c'
      _ -> False
    var n = \case
      Var n' -> n == n'
      _ -> False
    let_ n qe = \case
      Let n' e -> n == n' || qe e
      _ -> False
    add ql qr = \case
      Add l r -> ql l && qr r
      _ -> False

  instance QueryC (Expr -> Bool) where
    recQ q = \case
      e@(Let _ e') -> q e || recQ q e'
      e@(Add l r) -> q e || recQ q l || recQ q r
      e -> q e
    anyQ = const True
    andQ l r e = l e && r e
    orQ l r e = l e || r e

  instance ExprC Expr where
    lit = Lit
    var = Var
    let_ = Let
    add = Add

  expr = let_ "a" (var "c" `add` (lit "2" `add` lit "1"))

  test = recQ (lit "2")

  main :: IO ()
  main = do
    let e = expr :: Expr
    print e
    print (expr :: String)
    print (test e :: Bool)
