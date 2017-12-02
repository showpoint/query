{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Deep where
  import Control.Applicative

  class DeepC r m where
    deep :: (r -> m r) -> r -> m r

  instance {-# OVERLAPPABLE #-} Alternative m => DeepC a m where
    deep _ _ = empty
