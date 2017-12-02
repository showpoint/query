{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where
  import Data.Monoid
  import Expr
  import Query

  expr = let_ "a" (lit "1" `add` lit "2")
    `next`
      let_ "b" (var "a" `add` (lit "3" `add` var "a"))
    `next`
      let_ "c" (var "b" `add` (var "a" `add` var "b") `add` lit "4")

  anyV :: String -> Bool
  anyV = const True

  match1 = recQ $ let_ anyV (hasQ (var "a") `andQ` hasQ (lit "4"))
  match2 = recQ (notQ (hasQ (var "c")) `andQ` notQ (hasQ (let_ "c" anyQ)))
  match3 = recQ $ lit "4"

  main :: IO ()
  main = do
    let e = expr :: Expr String
    putStr "Expr: "
    putStrLn (expr :: String)
    putStr "\n"

    putStrLn $ "Match 1: " <> (match1 :: String)
    putStr "-- matched expressions: "
    print (match1 e :: [Expr String])

    putStr "-- executed: "
    print $ map exec (match1 e :: [Expr String])

    putStrLn $ "Match2: " <> (match2 :: String)
    print (match2 :: String)
    mapM_ (putStrLn . exec) (match2 e :: [Expr String])

    putStrLn $ "Match 3: " <> match3
    case match3 e :: Maybe (Expr String) of
      Just e' -> putStrLn $ " -- found:" <> (exec e')
      Nothing -> putStrLn "Nothing found."
