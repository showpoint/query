{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Lang where
  import Data.Kind
  import Data.Proxy
  import GHC.TypeLits

  infix 6 ##

  data (##) (l :: Symbol) a = Get a

  instance (KnownSymbol l, Show a) => Show ((l :: Symbol) ## a) where
    showsPrec p (Get a) =
      let mp = 6 in showParen (p > mp) $
                      showsPrec (mp + 1) (symbolVal @l Proxy) .
                      showString " ## "     .
                      showsPrec (mp + 1) (show a)

  infixr 5 :::

  data HL (ts :: [ * ]) where
    Nil   :: HL '[]
    (:::) :: t -> HL ts -> HL (t ': ts)
 
  type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
    All c '[]    = ()
    All c (t:ts) = (c t, All c ts)

  instance All Show ts => Show (HL ts) where
    showsPrec _ Nil = showString "Nil"
    showsPrec p (x ::: xs) =
      let mp = 5 in showParen (p > mp) $
                      showsPrec (mp + 1) x  .
                      showString " ::: "    .
                      showsPrec (mp + 1) xs

  a :: HL ["s" ## String, "i" ## Int]
  a = Get "aaa" ::: Get 1 ::: Nil

  main = print a