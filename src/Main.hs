{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Main where
  import Data.Monoid
  import Core.Types
  import Expr
  import Query

  instance ExprC (Expr -> Bool) where
    lit c = \case Lit c' -> c == c'; _ -> False
    var n = \case Var n' -> n == n'; _ -> False
    let_ n qe = \case Let n' e -> n == n' && qe e; _ -> False
    add ql qr = \case Add l r -> ql l && qr r; _ -> False
    next ql qr = \case Next l r -> ql l && qr r; _ -> False

  instance QueryC (Expr -> Bool) where
    recQ q = \case
      e@(Let _ e') -> q e || recQ q e'
      e@(Add l r) -> q e || recQ q l || recQ q r
      e@(Next l r) -> q e || recQ q l || recQ q r
      e -> q e
    anyQ = const True
    andQ l r e = l e && r e
    orQ l r e = l e || r e

  instance ExprC (Expr -> [Expr]) where
    lit c = \case
      e@(Lit c') | c == c' -> [e]
      _ -> mempty
    var n = \case
      e@(Var n') | n == n' -> [e]
      _ -> mempty
    let_ n q = \case
      e@(Let n' e') | n == n' -> case q e' of
        [] -> mempty
        _ -> [e]
      _ -> mempty
    add l r = \case
      e@(Add el er) -> case (l el, r er) of
        ([], _) -> mempty
        (_, []) -> mempty
        _ -> [e]
      _ -> mempty
    next l r = \case
      e@(Next el er) -> case (l el, r er) of
        ([], _) -> mempty
        (_, []) -> mempty
        _ -> [e]
      _ -> mempty

  instance QueryC (Expr -> [Expr]) where
    recQ q = \case
      e@(Let _ e') -> q e <> recQ q e'
      e@(Add l r) -> q e <> recQ q l <> recQ q r
      e@(Next l r) -> q e <> recQ q l <> recQ q r
      e -> q e

    anyQ = (:[])

    andQ l r e = case (l e, r e) of
      ([], _) -> mempty
      (_, []) -> mempty
      _ -> [e]

    orQ l r e = case (l e, r e) of
      ([], []) -> mempty
      _ -> [e]

  class FindC r where
    havingQ :: r -> r

  instance FindC String where
    havingQ q = "having (" <> q <> ")"

  instance FindC (Expr -> [Expr]) where
    havingQ q e = case recQ q e of
      [] -> []
      _ -> [e]

  expr = let_ "a" ((var "c" `add` var "c") `add` (lit "1" `add` var "b"))
    `next`
      let_ "c" (var "a" `add` var "b")

  match = recQ (let_ "c" (recQ (var "b")))

  main :: IO ()
  main = do
    let e = expr :: Expr
    putStrLn (expr :: String)
    putStrLn (match :: String)
    mapM_ (putStrLn . exec) (match e :: [Expr])
