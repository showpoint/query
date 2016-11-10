--{-# OPTIONS_GHC -ddump-deriv #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Main where
  import Core.Types

  data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Not
    deriving (Eq, Ord, Show)

  data ExprF a
    = Lit String
    | Var Name
    | Let Name a
    | BinOp BinOp a a
    deriving (Show)

  data Expr a = Expr { unExpr :: ExprF (Expr a) }
    deriving (Show, Functor, Foldable)

  data QueryF a
    = ExprQ (forall b. b -> Bool) (ExprF a)
    | RecQ a
    | AnyQ
    | AndQ a a
    | OrQ a a

  data Query a = Query { unQuery :: QueryF (Query a) }

  deriving instance Functor ExprF
  deriving instance Foldable ExprF

  deriving instance Functor QueryF
  deriving instance Foldable QueryF

  match :: Query a -> Expr a -> Bool
  match q e = matchQ (unQuery q) (unExpr e)

  matchQ :: QueryF (Query a) -> ExprF (Expr a) -> Bool
  matchQ AnyQ _ = True
  matchQ (RecQ q) e = matchQ (unQuery q) e || foldl (\b e' -> matchQ (RecQ q) (unExpr e')) True e
  matchQ (AndQ l r) e = matchQ (unQuery l) e && matchQ (unQuery r) e
  matchQ (OrQ l r) e = matchQ (unQuery l) e || matchQ (unQuery r) e
  matchQ (ExprQ f q) e = f e || foldl (\b e' -> matchE f q (unExpr e')) True e

  matchE :: (forall b. b -> Bool) -> ExprF (Query a) -> ExprF (Expr a) -> Bool
  matchE f (Let _ qe) e@(Let _ ee) = f e || matchQ (unQuery qe) (unExpr ee)
  matchE f (BinOp _ ql qr) e@(BinOp _ l r) = f e || matchQ (unQuery ql) (unExpr l) || matchQ (unQuery qr) (unExpr r)
  matchE f _ e = f e

  expr = Expr (Let "A" (Expr $ Lit "1"))

  named = ExprQ (\case Var n -> True; _ -> False) (Var "")

  test = Query AnyQ

  main :: IO ()
  main = do
    print expr
    print $ match test expr
