{-# LANGUAGE MultiParamTypeClasses #-}
module Deep where
  class DeepC r m where
    deep :: (r -> m r) -> r -> m r
