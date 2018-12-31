{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where
  import Data.Monoid
  import Expr
  import Query

  expr :: (LiteralC String r, NameC String r, ExprC r) => r
  expr = let_ "a" (lit "1" `add` lit "2")
    `next`
      let_ "b" (var "a" `add` (lit "3" `add` var "a"))
    `next`
      let_ "c" (var "b" `add` (var "a" `add` var "b") `add` lit "4")

  anyV :: String -> Bool
  anyV = const True

  match1 :: (LiteralC (String -> Bool) r, NameC (String -> Bool) r, QueryC r) => r
  match1 = recQ $ let_ anyV anyQ
  
  match2 :: (NameC String r, NameC (String -> Bool) r, ExprC r, QueryC r) => r
  match2 = recQ $ let_ anyV (hasQ (var "a" `add` var "b"))

  match3 :: (NameC (String -> Bool) r, QueryC r) => r
  match3 = recQ $ var anyV

  main :: IO ()
  main = do
    let e = expr :: Expr String
    putStrLn "Expr: "
    putStrLn (expr :: String)
    putStrLn ""

    putStrLn ("Match 1: " <> match1 :: String)
    putStrLn "- matched: "
    mapM_ putStrLn $ map exec (match1 e :: [Expr String])
    putStrLn "--"

    putStrLn $ "Match 2: " <> (match2 :: String)
    putStrLn "- matched: "
    mapM_ putStrLn $ map exec (match2 e :: [Expr String])
    putStrLn "--"

    putStrLn $ "Match 3: " <> match3
    putStrLn "- matched: "
    mapM_ putStrLn $ map exec (match3 e :: [Expr String])
    putStrLn "--"
    case match3 e :: Maybe (Expr String) of
      Just e' -> putStrLn $ "- found:" <> (exec e')
      Nothing -> putStrLn "Nothing found."
