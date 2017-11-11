{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where
  import Control.Applicative
  import Control.Monad
  import Control.Monad.State
  import Data.Functor.Identity
  import Data.Maybe
  import Data.Monoid
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import Core.Types
  import Expr
  import Query
  import Deep

  newtype Vars a = Vars { unVars :: (Name, Expr a)}

  expr = let_ "a" (lit "1" `add` lit "2")
    `next`
      let_ "b" (var "a" `add` (lit "3" `add` var "a"))
    `next`
      let_ "c" (var "b" `add` (var "a" `add` var "b") `add` lit "4")

  anyV :: String -> Bool
  anyV = const True

  match1 = recQ $ lit anyV
  match2 = recQ (notQ (hasQ (var "c")))
  match3 = recQ $ lit "a"

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

    case match3 e :: Maybe (Expr String) of
      Just e' -> putStrLn (exec e')
      Nothing -> putStrLn "Nothing found."

    putStrLn $ "match2: " <> (match2 :: String)
    print (match2 e :: [Expr String])
    mapM_ (putStrLn . exec) (match2 e :: [Expr String])
