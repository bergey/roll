module Main where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util

import           System.Directory

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/hello" <.> exe]

    phony "clean" $ do
        putInfo "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    "_build/hello" <.> exe %> \out -> do
        liftIO $ createDirectoryIfMissing True "_build/objects"
        cmd_ "ghc -outputdir _build/objects -isrc app/hello.hs -o" [out]
