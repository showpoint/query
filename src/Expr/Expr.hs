{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Expr.Expr where
  import Control.Applicative
  import Control.Monad
  import Core.Types
  import Expr.Type

  class ExprC r where
    add :: r -> r -> r
    next :: r -> r -> r

  instance ExprC (Expr a) where
    add l = Expr . Add l
    next l = Expr . Next l

  instance ExprC String where
    add l r = l ++ " + " ++ r
    next l r = l ++ "; " ++ r

  instance MonadPlus m => ExprC (Expr a -> m (Expr a)) where
    add l r (Expr (Add el er)) = add <$> l el <*> r er
    add _ _ _ = empty

    next l r (Expr (Next el er)) = next <$> l el <*> r er
    next _ _ _ = empty
  