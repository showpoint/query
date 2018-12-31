{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Expr.Literal where
  import Control.Applicative
  import Control.Monad
  import Core.Types
  import Expr.Type

  class LiteralC l r where
    lit :: l -> r

  instance LiteralC String (Expr a) where
    lit = Expr . Lit

  instance LiteralC String String where
    lit = show

  instance {-# OVERLAPPABLE #-} LiteralC a String where
    lit _ = "lit γ"

  instance MonadPlus m => LiteralC String (Expr a -> m (Expr a)) where
    lit c = lit (== c)

  instance MonadPlus m => LiteralC (String -> Bool) (Expr a -> m (Expr a)) where
    lit f (Expr (Lit c)) | f c = pure (lit c)
    lit _ _ = empty
