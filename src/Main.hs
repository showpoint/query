--{-# OPTIONS_GHC -ddump-deriv #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where
  import Data.Monoid

  import Core.Types
  import Expr

  data Query b r
    = VarQ r
    | LetQ (Query b r) (Query b r)
    | BinOpQ b (Query b r) (Query b r)
    | NotQ (Query b r)
    | IfQ (Query b r) (Query b r) (Query b r)
    | WhileQ (Query b r) (Query b r)
    | AndThenQ (Query b r) (Query b r)
    | RecQ (Query b r)
    | StopQ
    | AnyQ
    | AndQ (Query b r) (Query b r)
    | OrQ (Query b r) (Query b r)
    deriving (Functor, Foldable, Traversable)

  match AnyQ _ = True
  match StopQ Stop = True
  match (VarQ f) (Var a) = f a
  match (LetQ qr qe) (Let r e) = match qr r && match qe e
  match (BinOpQ f ql qr) (BinOp op l r) = f op && match ql l && match qr r
  match (NotQ qe) (Not e) = match qe e
  match (IfQ qi qt qe) (If ei et ee) = match qi ei && match qt et && match qe ee
  match (WhileQ qw qe) (While w e) = match qw w && match qe e
  match (AndThenQ qf qn) (AndThen f n) = match qf f && match qn n
  match qr@(RecQ q) e@(Let _ e1) = match q e || match qr e1
  match qr@(RecQ q) e@(BinOp _ l r) = match q e || match qr l || match qr r
  match qr@(RecQ q) e@(Not e1) = match q e || match qr e1
  match qr@(RecQ q) e@(If i t e1) = match q e || match qr i || match qr t || match qr e1
  match qr@(RecQ q) e@(While w e1) = match q e || match qr w || match qr e
  match qr@(RecQ q) e@(AndThen f n) = match q e || match qr f || match qr n
  match (RecQ q) e = match q e
  match (AndQ ql qr) e = match ql e && match qr e
  match (OrQ ql qr) e = match ql e || match qr e
  match _ _ = False

  true = const True
  nameIs n = VarQ $ \(Ref n') -> n == n'
  binOpQ = BinOpQ
  assign = LetQ
  expr = AnyQ
  notQ = NotQ
  anyQ = RecQ
  andQ = AndQ
  orQ = OrQ

  test = anyQ (nameIs "A" `orQ` nameIs "B")

  t = BinOp Add (Not (BinOp And (Var (Ref "C")) (Var (Ref "A")))) (Var (Ref "B"))

  main :: IO ()
  main = print $ match test t
