module Main where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           DynFlags
import           GHC
import           GHC.Paths                  (libdir)

import           Data.Functor
import           System.Directory

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/hello" <.> exe]

    phony "clean" $ do
        putInfo "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    "_build/hello" <.> exe %> \out -> do
        liftIO $ createDirectoryIfMissing True "_build/objects"
        need [ "app/roll.hs" ] -- TODO better dependency
        need [ "hello/Main.hs", "hello/src/Hello/Lib.hs" ]

        -- TODO how does shake handle Exceptions?
        -- defaultErrorHandler defaultLogAction $ do
        liftIO $ runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            setSessionDynFlags dflags
                { importPaths = "hello/src" : importPaths dflags
                , objectDir = Just "_build/objects"
                , hiDir = Just "_build/objects"
                , outputFile = Just out
                }
            target <- guessTarget "hello/Main.hs" Nothing
            setTargets [target]
            void $ load LoadAllTargets
