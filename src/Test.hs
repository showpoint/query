module Test where
  import Control.Monad.Plus
  
  notQ q e = case q e of
    mzero -> return e
    _ -> mzero
