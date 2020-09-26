-- | Haskell hello world, no dependencies outside base

module Main where

import           Hello.Lib

import           Data.Foldable
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> sayHello "world"
        _  -> for_ args sayHello
