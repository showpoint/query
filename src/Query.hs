{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Query where
  import Data.Monoid

  data Query a
    = RecQ a
    | AnyQ
    | AndQ a a
    | OrQ a a

  class QueryC r where
    recQ :: r -> r
    anyQ :: r
    andQ :: r -> r -> r
    orQ :: r -> r -> r

  instance QueryC String where
    recQ q = "deep (" <> q <> ")"
    anyQ = "any"
    andQ l r = l <> " and " <> r
    orQ l r = l <> " or " <> r
