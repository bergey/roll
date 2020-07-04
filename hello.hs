-- | Haskell hello world, no dependencies outside base

module Main where

import           Data.Foldable
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "hello world"
        _  -> for_ args $ \name -> putStrLn ("hello " ++ name)
