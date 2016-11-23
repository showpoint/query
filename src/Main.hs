{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
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

  expr = let_ "a" ((var "c" `add` var "c") `add` (lit "1" `add` var "b"))
    `next`
      let_ "c" (var "a" `add` (var "b" `add` var "c"))
    `next`
      let_ "b" (var "c" `add` (var "b" `add` var "c"))

  anyV = const True :: String -> Bool

  match1 = recQ ((var anyV `orQ` lit anyV) `add` recQ (var "b"))
  match2 = recQ (notQ (hasQ (var "c")))

  main :: IO ()
  main = do
    let e = expr :: Expr String
    putStrLn (expr :: String)
    putStrLn (match1 :: String)
    case match1 e :: Maybe (Expr String) of
      Just e' -> putStrLn (exec e')
      Nothing -> putStrLn "Nothing found."
    putStrLn (match2 :: String)
    mapM_ (putStrLn . exec) (match2 e :: [Expr String])
