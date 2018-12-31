{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Expr.Deep where
  import Control.Applicative
  import Control.Monad
  import Core.Types
  import Expr.Type

  class DeepC r m where
    deep :: (r -> m r) -> r -> m r

  instance {-# OVERLAPPABLE #-} MonadPlus m => DeepC a m where
    deep _ _ = empty

  instance MonadPlus m => DeepC (Expr a) m where
    deep f (Expr (Let _ e)) = f e
    deep f (Expr (Add l r)) = f l <|> f r
    deep f (Expr (Next l r)) = f l <|> f r
    deep _ _ = empty
