{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Query where
  import Control.Applicative
  import Control.Monad
  import Data.Monoid
  import Deep

  data Query e a
    = ReqQ a
    | HasQ a
    | AnyQ
    | AndQ a a
    | OrQ a a
    | NotQ a
    | MatchQ e 

  class QueryC r where
    recQ :: r -> r
    hasQ :: r -> r
    anyQ :: r
    andQ :: r -> r -> r
    orQ :: r -> r -> r
    notQ :: r -> r
    matchQ :: r -> r

  instance QueryC String where
    recQ q = "deep (" <> q <> ")"
    hasQ q = "has (" <> q <> ")"
    anyQ = "any"
    andQ l r = l <> " and " <> r
    orQ l r = l <> " or " <> r
    notQ q = "not (" <> q <> ")"
    matchQ q = q

  instance (Foldable m, DeepC a m, MonadPlus m) => QueryC (a -> m a) where
    recQ q e = q e <|> deep (recQ q) e
    hasQ q e = const e <$> recQ q e
    anyQ = pure
    andQ l r e = e <$ l e <* r e
    orQ l r e = l e <|> r e
    notQ q e = if null (q e) then pure e else empty
    matchQ q e = q e 