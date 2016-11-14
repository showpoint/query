{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Main where
  import Data.Maybe
  import Data.Monoid
  import Core.Types
  import Expr
  import Query
{-}
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
-}
  instance QueryC (Expr -> Maybe [Expr]) where
    recQ q = \case
      e@(Let _ e') -> q e <> recQ q e'
      e@(Add l r) -> q e <> recQ q l <> recQ q r
      e@(Next l r) -> q e <> recQ q l <> recQ q r
      e -> q e

    anyQ = Just . pure

    andQ l r e = do
      rl <- l e
      rr <- r e
      return [e]

    orQ l r e = l e <> r e

  instance LiteralC String (Expr -> Maybe [Expr]) where
    lit c = lit (c ==)

  instance LiteralC (String -> Bool) (Expr -> Maybe [Expr]) where
    lit f = \case
      e@(Lit c) | f c -> pure [e]
      _ -> mempty

  instance NameC String (Expr -> Maybe [Expr]) where
    var n = var (n ==)
    let_ n = let_ (n ==)

  instance NameC (Name -> Bool) (Expr -> Maybe [Expr]) where
    var f = \case
      e@(Var n) | f n -> pure [e]
      _ -> mempty
    let_ f q = \case
      e@(Let n e') | f n -> q e' >> return [e]
      _ -> mempty

  instance ExprC (Expr -> Maybe [Expr]) where
    add l r = \case
      e@(Add el er) -> l el >> r er >> return [e]
      _ -> mempty
    next l r = \case
      e@(Next el er) -> l el >> r er >> return [e]
      _ -> mempty

  expr = let_ "a" ((var "c" `add` var "c") `add` (lit "1" `add` var "b"))
    `next`
      let_ "c" (var "a" `add` var "b")

  anyV = const True :: String -> Bool

  match = recQ ((var anyV `orQ` lit anyV) `add` recQ (var "b"))

  main :: IO ()
  main = do
    let e = expr :: Expr
    putStrLn (expr :: String)
    putStrLn (match :: String)
    case match e :: Maybe [Expr] of
      Just es -> mapM_ (putStrLn . exec) es
      Nothing -> putStrLn "Nothing found."
