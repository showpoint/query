{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where
  import Prelude hiding (any)
  import Data.Monoid hiding (Any)

  data ExprF v a
    = Lit v
    | Var v
    | Let a a
    | Add a a
    | AndNext a a
    deriving (Show, Functor)

  data QueryF e a
    = Match e
    | Has a
    | Any
    deriving (Show, Functor)

  newtype Expr v a = Expr { unExpr :: ExprF v (Expr v a)} deriving (Show, Functor)
  newtype Query e a = Query { unQuery :: QueryF e (Query e a)} deriving (Show, Functor)
  newtype ExprQ v a = ExprQ { unExprQ :: ExprF v (QueryE v a)} deriving (Show, Functor)
  newtype QueryE v a = QueryE { unQueryE :: QueryF (ExprQ v a) (QueryE v a)} deriving (Show)

  instance Functor (QueryE v) where
    fmap f (QueryE (Match e)) = QueryE (Match (fmap f e))
    fmap f (QueryE (Has a)) = QueryE (Has (fmap f a))
    fmap f (QueryE Any) = QueryE Any
  
  class ExprC v a | a -> v where
    lit :: v -> a
    var :: v -> a
    let_ :: a -> a -> a
    add :: a -> a -> a
    andNext :: a -> a -> a

  instance ExprC String (Expr String a) where
    lit = Expr . Lit
    var = Expr . Var
    let_ l r = Expr (Let l r)
    add l r = Expr (Add l r)
    andNext l r = Expr (AndNext l r)

  instance ExprC String (ExprQ String a) where
    lit = ExprQ . Lit
    var = ExprQ . Var
    let_ l r = ExprQ (Let l r)
    add l r = ExprQ (Add l r)
    andNext l r = QueryE (Match (ExprQ (AndNext l r)))

  instance ExprC String (QueryE String a) where
    lit = QueryE . Match . ExprQ . Lit
    var = QueryE . Match . ExprQ . Var
    let_ l r = QueryE (Match (ExprQ (Let l r)))
    add l r = QueryE (Match (ExprQ (Add l r)))
    andNext l r = QueryE (Match (ExprQ (AndNext l r)))

  instance ExprC String String where
    lit = mappend "lit "
    var = mappend "var "
    let_ l r = l <> " = " <> r
    add l r = l <> " + " <> r
    andNext l r = l <> "; " <> r
    
  class QueryC e a | a -> e where
    any :: a

  instance QueryC a (QueryE String a) where
    any = QueryE Any

  instance QueryC String String where
    any = "any"

  class PrintF a where
    printF :: a -> String

  instance PrintF a => PrintF (ExprF String a) where
    printF (Lit a) = "lit " <> a
    printF (Var a) = "var " <> a
    printF (Let l r) = printF l <> " = " <> printF r
    printF (Add l r) = printF l <> " + " <> printF r
    printF (AndNext l r) = printF l <> "; " <> printF r

  instance (PrintF a, PrintF (ExprQ String a)) => PrintF (QueryF (ExprQ String a) a) where
    printF (Match e) = "match " <> printF e
    printF Any = "any"

  instance (PrintF a, PrintF (QueryE String a)) => PrintF (ExprQ String a) where
    printF (ExprQ a) = printF a

  instance PrintF (QueryE String a) where
    printF (QueryE (Match (ExprQ a))) = printF a
    printF (QueryE (Has a)) = printF a
    printF (QueryE Any) = "any"
    
  instance PrintF String where
    printF = id

  expr = 
      let_ (var "a") (lit "1")
    `andNext`
      let_ (var "b") (add (lit "a") (var "b"))

  q1 = var "a"
  q2 = add any (var "b")
  q3 = let_ (var "a") (add (var "b") any)

  class Monoid (m e) => Match q e m where
    match :: q -> e -> m e

  instance (Applicative m, Foldable m, Eq v, Monoid (m a), Match a a m) => Match (ExprF v a) (ExprF v a) m where
    match (Var n)       e@(Var n') | n == n' = pure e
    match (Lit n)       e@(Lit n') | n == n' = pure e
    match (Let l r)     e@(Let l' r')        = fmap (const e) (match l l' <> match r r')
    match (Add l r)     e@(Add l' r')        = if not (null (match l l' <> match r r')) then pure e else mempty
    match (AndNext l r) e@(AndNext l' r')    = if not (null (match l l' <> match r r')) then pure e else mempty

  main = do
    let e :: QueryE String String = expr
    putStrLn $ printF e
    print (q3 :: QueryE String String)
