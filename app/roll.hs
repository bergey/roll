{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Foldable
import Data.Hashable
import Data.Maybe
import Data.Traversable
import Development.Shake as Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Distribution.PackageDescription
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Verbosity as Cabal
import DynFlags
import GHC
import GHC.Generics
import GHC.Paths (libdir)
import Roll.Cabal
import Roll.Orphans
import System.Console.GetOpt as GetOpt

import Data.Functor
import System.Directory

main :: IO ()
main = shakeArgsWith shakeOptions{shakeFiles=".roll"} rollOptions $ \options targets -> pure $ Just $ do
    if "clean" `elem` targets then want ["clean"] else want ["all"]

    phony "clean" do
        putInfo "Cleaning files in .roll"
        removeFilesAfter ".roll" ["//*"]

    parseCabal <- newCache \cabalFileName -> do
      need [cabalFileName]
      verbosity' <- cabalVerbosity <$> getVerbosity
      liftIO $ readPackageDescription verbosity' cabalFileName

    buildPackage <- newCache \(BuildPackage cabalFile) -> do
      PackageDescription{..} <- parseCabal cabalFile
      for_ library \lib->
        askOracle (BuildComponent package lib)
      -- TODO replace below with implementations
      unless (null subLibraries) (liftIO . throwIO . NotImplemented $ "sub-libraries in " <> cabalFile)
      unless (null executables) (liftIO . throwIO . NotImplemented $ "executables in " <> cabalFile)
      unless (null testSuites) (putWarn ("test suites not implemented: " <> cabalFile))
      unless (null benchmarks) (putWarn ("benchmarks not implemented: " <> cabalFile))
      unless (null foreignLibs) (putWarn ("foreign libraries not implemented: " <> cabalFile))

    -- TODO need separate oracles per component type (or sum type),
    -- but most of this action should be reusable
    _buildLibrary <- addOracleCache \(BuildComponent packageId library) -> do
      let
        objectDir = ".roll/objects" </> prettyShow packageId
        hiDir = ".roll/interfaces" </> prettyShow packageId
      liftIO $ createDirectoryIfMissing True objectDir
      liftIO $ createDirectoryIfMissing True hiDir
      -- TODO how does shake handle Exceptions?
      -- defaultErrorHandler defaultLogAction $ do
      liftIO $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
          -- TODO tmp folders, copy iff build succeeds
          { objectDir = Just objectDir
          , hiDir = Just hiDir
          , outputFile = Nothing
          , importPaths = hsSourceDirs (libBuildInfo library)
          }
        setTargets =<< for (exposedModules library) \m -> guessTarget (prettyShow m) Nothing
        void $ load LoadAllTargets
      return ()

    phony "all" do
      liftIO $ createDirectoryIfMissing True ".roll/bin"
      traverse_ (buildPackage . BuildPackage) targets


rollOptions :: [OptDescr (Either String Options)]
rollOptions =
  [
    -- GetOpt.Option "o" [] (ReqArg (Right . ExeFileName . (".roll/bin/" ++)) "FILE")
    -- "name of executable"
  ]

data Options

cabalVerbosity :: Shake.Verbosity -> Cabal.Verbosity
cabalVerbosity = \case
  Silent -> silent
  Error -> normal -- or silent?
  Warn -> normal
  Info -> normal
  Verbose -> verbose
  Diagnostic -> deafening

newtype BuildPackage = BuildPackage FilePath
  deriving stock (Show, Eq, Generic)
  deriving newtype Hashable

data BuildComponent c = BuildComponent PackageIdentifier c
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)
type instance RuleResult (BuildComponent Library) = ()
