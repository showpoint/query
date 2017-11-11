{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Expr where
  import Control.Applicative
  import Control.Monad
  import Core.Types
  import Deep

  data ExprF a
    = Lit String
    | Var Name
    | Let Name a
    | Add a a
    | Next a a
    deriving (Show)

  newtype Expr a = Expr { unExpr :: ExprF (Expr a)}
    deriving (Show)

  deriving instance Functor ExprF
  deriving instance Foldable ExprF
  deriving instance Traversable ExprF

  class LiteralC l r where
    lit :: l -> r

  class NameC n r where
    var :: n -> r
    let_ :: n -> r -> r

  class ExprC r where
    add :: r -> r -> r
    next :: r -> r -> r

  instance LiteralC String (Expr a) where
    lit = Expr . Lit

  instance NameC Name (Expr a) where
    var = Expr . Var
    let_ n = Expr . Let n

  instance ExprC (Expr a) where
    add l = Expr . Add l
    next l = Expr . Next l

  instance LiteralC String String where
    lit = show

  instance {-# OVERLAPPABLE #-} LiteralC a String where
    lit _ = "lit γ"

  instance NameC Name String where
    var n = n
    let_ n r = n ++ " = " ++ r

  instance {-# OVERLAPPABLE #-} NameC a String where
    var _ = "var γ"
    let_ _ r = "γ = " ++ r

  instance ExprC String where
    add l r = l ++ " + " ++ r
    next l r = l ++ "; " ++ r

  instance MonadPlus m => LiteralC String (Expr a -> m (Expr a)) where
    lit c = lit (== c)

  instance MonadPlus m => LiteralC (String -> Bool) (Expr a -> m (Expr a)) where
    lit f (Expr (Lit c)) | f c = pure (lit c)
    lit _ _ = empty

  instance MonadPlus m => NameC Name (Expr a -> m (Expr a)) where
    var n = var (== n)
    let_ n = let_(== n)

  instance MonadPlus m => NameC (Name -> Bool) (Expr a -> m (Expr a)) where
    var f (Expr (Var n)) | f n = pure (var n)
    var _ _ = empty

    let_ f q (Expr (Let n e')) | f n = let_ n <$> q e'
    let_ _ _ _ = empty

  instance MonadPlus m => ExprC (Expr a -> m (Expr a)) where
    add l r (Expr (Add el er)) = add <$> l el <*> r er
    add _ _ _ = empty

    next l r (Expr (Next el er)) = next <$> l el <*> r er
    next _ _ _ = empty

  instance MonadPlus m => DeepC (Expr a) m where
    deep f (Expr (Let _ e)) = f e
    deep f (Expr (Add l r)) = f l <|> f r
    deep f (Expr (Next l r)) = f l <|> f r
    deep f _ = empty

  exec :: (LiteralC String a, NameC Name a, ExprC a) => Expr a -> a
  exec = exec' . unExpr
    where
      exec' (Lit n) = lit n
      exec' (Var n) = var n
      exec' (Let n e) = let_ n (exec e)
      exec' (Add l r) = add (exec l) (exec r)
      exec' (Next l r) = next (exec l) (exec r)
