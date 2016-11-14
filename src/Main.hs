{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Main where
  import Control.Applicative
  import Control.Monad
  import Data.Maybe
  import Data.Monoid
  import Core.Types
  import Expr
  import Query

  instance QueryC (Expr a -> Maybe (Expr a)) where
    recQ q e = case unExpr e of
      Let _ e' -> q e <|> recQ q e'
      Add l r -> q e <|> recQ q l <|> recQ q r
      Next l r -> q e <|> recQ q l <|> recQ q r
      _ -> q e

    anyQ = Just

    andQ l r e = liftM2 (const . const e) (l e) (r e)

    orQ l r e = l e <|> r e

  instance LiteralC String (Expr a -> Maybe (Expr a)) where
    lit c = lit (c ==)

  instance LiteralC (String -> Bool) (Expr a -> Maybe (Expr a)) where
    lit f e = case unExpr e of
      Lit c | f c -> pure e
      _ -> Nothing

  instance NameC Name (Maybe (Expr a)) where
    var n = pure $ Expr (Var n)
    let_ n e = Expr . Let n <$> e

  instance NameC Name (Expr a -> Maybe (Expr a)) where
    var n = var (n ==)
    let_ n = let_ (n ==)

  instance NameC (Name -> Bool) (Expr a -> Maybe (Expr a)) where
    var f e = case unExpr e of
      Var n | f n -> pure e
      _ -> Nothing
    let_ f q e = case unExpr e of
      Let n e' | f n -> let_ n (q e')
      _ -> Nothing

  instance ExprC (Expr a -> Maybe (Expr a)) where
    add l r e = case unExpr e of
      Add el er -> liftM2 add (l el) (r er)
      _ -> Nothing
    next l r e = case unExpr e of
      Next el er -> liftM2 next (l el) (r er)
      _ -> Nothing

  expr = let_ "a" ((var "c" `add` var "c") `add` (lit "1" `add` var "b"))
    `next`
      let_ "c" (var "a" `add` var "b")

  anyV = const True :: String -> Bool

  match = recQ ((var anyV `orQ` lit anyV) `add` recQ (var "b"))
  match2 = recQ (let_ "c" anyQ)

  main :: IO ()
  main = do
    let e = expr :: Expr String
    putStrLn (expr :: String)
    putStrLn (match :: String)
    case match e :: Maybe (Expr String) of
      Just e' -> putStrLn (exec e')
      Nothing -> putStrLn "Nothing found."
    putStrLn (match2 :: String)
    case match2 e :: Maybe (Expr String) of
      Just e' -> putStrLn (exec e')
      Nothing -> putStrLn "Nothing found."
