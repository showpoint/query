module Expr where
  import Core.Types

  data Type
    = Boolean
    | Int
    | String
    | Other Name
    deriving (Eq, Ord, Show)

  data Scope r t
    = Def r t
    deriving Show

  data Ref
    = Ref Name
    deriving Show

  data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    deriving (Eq, Show)

  data Expr
    = Var Ref
    | Let Expr Expr
    | BinOp BinOp Expr Expr
    | Not Expr
    | If Expr Expr Expr
    | While Expr Expr
    | AndThen Expr Expr
    | Stop
    deriving (Show)
