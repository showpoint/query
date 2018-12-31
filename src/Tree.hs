{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Tree where
  import Data.Functor.Foldable
  import Data.Monoid

  data Expr i f
    = Lit (LitV i)
    | Var (VarV i)
    | Let (LetV i) (ExprV i f)
    | Add (ExprV i f) (ExprV i f)
    | Bind (ExprV i f) (ExprV i f)

  type family LitV i :: *
  type family VarV i :: *
  type family LetV i :: *
  type family ExprV i :: * -> *

  deriving instance Functor (ExprV i) => Functor (Expr i)
  deriving instance Applicative (ExprV i) => Applicative (Expr i)

  data R

  type instance LitV R = String
  type instance VarV R = String
  type instance LetV R = String
  type instance ExprV R = Expr R

  type instance Base (Expr R a) = Expr R

  a :: Expr R f
  a = Lit "1" `Add` Var "a"

  instance Recursive (Expr R a) where
    project (Lit a) = Lit a
    project (Var a) = Var a
    project (Let a e) = Let a <$> project e
    project (Add l r) = Add <$> project l <*> project r
    project (Bind l r) = Bind <$> project l <*> project r
