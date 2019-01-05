{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Main where
  import Prelude hiding (any)
  import Control.Applicative
  import Control.Monad
  import Control.Monad.Writer.Strict hiding (any, Any)
  import Data.Functor.Foldable
  import Data.Monoid ((<>))
  import Text.Show.Deriving
 
  type Name = String

  class ExprC a where
    lit :: Name -> a
    var :: Name -> a
    let_ :: a -> a -> a
    add :: a -> a -> a
    andNext :: a -> a -> a

  class QueryC a where
    any :: a
    has :: a -> a
    peek :: Name -> a -> a

  data ExprF a
    = Lit Name
    | Var Name
    | Let a a
    | Add a a
    | AndNext a a
    deriving (Show)

  $(deriveShow1 ''ExprF)

  type Expr = Fix ExprF

  instance ExprC Expr where
    lit = Fix . Lit
    var = Fix . Var
    let_ l = Fix . Let l
    add l = Fix . Add l
    andNext l = Fix . AndNext l
    
  type ExprQ a = Query ExprF a

  data QueryF e a
    = Match e
    | Peek Name (QueryF e a)
    | Has (QueryF e a)
    | Any
    deriving (Show)

  newtype Query e a = Query { unQuery :: QueryF (e (Query e a)) a }

  deriving instance Show (e (Query e a)) => Show (Query e a)

  instance ExprC (ExprQ a) where
    lit = Query . Match . Lit
    var = Query . Match . Var
    let_ l = Query . Match . Let l
    add l = Query . Match . Add l
    andNext l = Query . Match . AndNext l

  instance QueryC (ExprQ a) where
    any = Query Any
    has = Query . Has . unQuery
    peek n = Query . Peek n . unQuery

  data RMode = RFirst | RAll

  deepFirst :: Monad m => (m Bool) -> (m Bool) -> m Bool
  deepFirst a b = a >>= \r -> if r then return r else b

  deepAll :: Monad m => (m Bool) -> (m Bool) -> m Bool
  deepAll a b = (||) <$> a <*> b 

  deep s q e@(Fix (Let l' r'))     f = match q e f `s` deep s q l' f `s` deep s q r' f
  deep s q e@(Fix (Add l' r'))     f = match q e f `s` deep s q l' f `s` deep s q r' f
  deep s q e@(Fix (AndNext l' r')) f = match q e f `s` deep s q l' f `s` deep s q r' f
  deep _ q e f                       = match q e f 

  match :: MonadWriter l m => Query ExprF a -> Expr -> (Expr -> l) -> m Bool
  match (Query (Match (Lit l))) (Fix (Lit l')) _ | l == l' = pure True 
  match (Query (Match (Var n))) (Fix (Var n')) _ | n == n' = pure True

  match (Query (Match (Let v e)))     (Fix (Let v' e'))     f = (&&) <$> match v v' f <*> match e e' f
  match (Query (Match (Add l r)))     (Fix (Add l' r'))     f = (&&) <$> match l l' f <*> match r r' f
  match (Query (Match (AndNext l r))) (Fix (AndNext l' r')) f = (&&) <$> match l l' f <*> match r r' f

  match (Query (Peek n q)) e f = match (Query q) e f >>= \r -> when r (tell (f e)) >> return r

  match (Query (Has q)) e f = deep deepAll (Query q) e f

  match (Query Any) a _ = return True
  match _           _ _ = return False

  runMatch :: Query ExprF a -> Expr -> (Bool, [Expr])
  runMatch q e = runWriter (match q e pure)

  e1 :: Expr
  e1 = let_ (var "a") (lit "1")
    `andNext`
      let_ (var "b") (lit "2")
    `andNext`
      let_ (var "c") (add (var "a") (var "b"))

  qa = has (peek "a" (let_ any any))
  
  qb = has (peek "b" (var "b"))
  
  qc = has any

  q1 = let_ qc (add qa qb)

  main :: IO ()
  main = do
    print $ runMatch qa e1