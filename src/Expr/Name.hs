{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Expr.Name where
  import Control.Applicative
  import Control.Monad
  import Core.Types
  import Expr.Type

  class NameC n r where
    var :: n -> r
    let_ :: n -> r -> r

  instance NameC Name (Expr a) where
    var = Expr . Var
    let_ n = Expr . Let n

  instance NameC Name String where
    var n = n
    let_ n r = n ++ " = " ++ r

  instance {-# OVERLAPPABLE #-} NameC a String where
    var _ = "var γ"
    let_ _ r = "γ = " ++ r

  instance MonadPlus m => NameC Name (Expr a -> m (Expr a)) where
    var n = var (== n)
    let_ n = let_(== n)

  instance MonadPlus m => NameC (Name -> Bool) (Expr a -> m (Expr a)) where
    var f (Expr (Var n)) | f n = pure (var n)
    var _ _ = empty

    let_ f q (Expr (Let n e')) | f n = let_ n <$> q e'
    let_ _ _ _ = empty
