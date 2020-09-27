{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import DynFlags
import GHC
import GHC.Paths (libdir)
import System.Console.GetOpt as GetOpt

import Data.Functor
import System.Directory

main :: IO ()
main = shakeArgsWith shakeOptions{shakeFiles=".roll"} rollOptions $ \options targets -> pure $ Just $ do
    if "clean" `elem` targets then want ["clean"] else want ["all"]

    phony "clean" do
        putInfo "Cleaning files in .roll"
        removeFilesAfter ".roll" ["//*"]

    phony "all" do
        liftIO $ createDirectoryIfMissing True ".roll/objects"
        let outputFile = foldr (\(ExeFileName file) _ -> Just file) Nothing options
        when (isJust outputFile) (liftIO $ createDirectoryIfMissing True ".roll/bin")

        -- TODO how does shake handle Exceptions?
        -- defaultErrorHandler defaultLogAction $ do
        liftIO $ runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            setSessionDynFlags dflags
                { objectDir = Just ".roll/objects"
                , hiDir = Just ".roll/objects"
                , outputFile = outputFile
                }
            setTargets =<< for targets \t -> guessTarget t Nothing
            void $ load LoadAllTargets

rollOptions :: [OptDescr (Either String Options)]
rollOptions =
  [ GetOpt.Option "o" [] (ReqArg (Right . ExeFileName . (".roll/bin/" ++)) "FILE")
    "name of executable"
  ]

data Options = ExeFileName String
