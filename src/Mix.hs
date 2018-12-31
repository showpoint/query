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
{-# LANGUAGE TypeFamilies #-}
module Main where
  import Prelude hiding (any)
  import Control.Applicative
  import Data.Functor.Foldable
  import Data.Monoid ((<>))

  import Classes

  data ExprF a
    = Lit Name
    | Var Name
    | Let a a
    | Add a a
    | AndNext a a
    deriving (Show, Functor, Foldable, Traversable)

  data QueryF e a
    = Match e
    | Has (QueryF e a)
    | Any
    deriving (Show, Functor, Foldable, Traversable)

  type Expr = Fix ExprF
  
  newtype Query e a = Query { unQuery :: QueryF (e (Query e a)) a }

  type ExprQ a = Query ExprF a

  deriving instance Show (e (Query e a)) => Show (Query e a)

  class Match q e r where
    match :: q -> e -> r
 
  instance (Alternative f, Match a Expr (f Expr)) => Match (ExprF a) Expr (f Expr) where
    match (Lit n) e@(Fix (Lit n')) | n == n' = pure e 
    match (Var n) e@(Fix (Var n')) | n == n' = pure e 
    match (Add l r) e@(Fix (Add l' r')) = (pure . e <>) <$> match l l' <> match r r'

  instance Applicative f => Match (ExprQ a) Expr (f Expr) where
    match q e = matchQ (unQuery q) e where
      matchQ Any e = pure e
      matchQ (Has q') e = matchQ q' e
      matchQ (Match e') e = match e' e

  ppExpr = cata pp where
    pp (Lit v) = v
    pp (Var v) = v
    pp (Let l r) = l <> " = " <> r
    pp (Add l r) = l <> " + " <> r 
    pp (AndNext l r) = l <> "; " <> r

  instance ExprC Expr where
    lit = Fix . Lit
    var = Fix . Var
    let_ l = Fix . Let l
    add l = Fix . Add l
    andNext l = Fix . AndNext l

  instance ExprC (ExprQ a) where
    lit = Query . Match . Lit
    var = Query . Match . Var
    let_ l = Query . Match . Let l
    add l = Query . Match . Add l
    andNext l = Query . Match . AndNext l

  instance QueryC (ExprQ a) where
    any = Query Any
    has = Query . Has . unQuery

  e1 :: Expr
  e1 = let_ (var "a") (lit "1")
    `andNext`
      let_ (var "b") (lit "2")
    `andNext`
      let_ (var "c") (add (var "a") (var "b"))

  qa = add any any
  qb = has (var "a")
  qc = any

  q1 = let_ qc (add qa qb)

  main :: IO ()
  main = do
    putStrLn (ppExpr e1)
    print (q1 :: ExprQ String)
