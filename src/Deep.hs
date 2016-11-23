{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Deep where
  class DeepC r m where
    deep :: (r -> m r) -> r -> m r

  instance {-# OVERLAPPABLE #-} Applicative m => DeepC r m where
    deep _ = pure
