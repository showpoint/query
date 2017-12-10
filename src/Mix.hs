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

  qa = Query (Match (Add (Query Any) (Query Any)))
  qb = Query (Has (Match (Var "a")))
  qc = Query Any

  q1 = Query (Match (Let qc (Query (Match (Add qa qb)))))

  main :: IO ()
  main = do
    putStrLn (ppExpr e1)
    -- print q1
