module Classes where
  type Name = String

  class ExprC a where
    lit :: Name -> a
    var :: Name -> a
    let_ :: a -> a -> a
    add :: a -> a -> a
    andNext :: a -> a -> a

  class QueryC a where
    any :: a
    has :: a -> a