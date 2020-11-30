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
import Data.List (isSuffixOf)
import Data.Maybe
import Data.Traversable
import Development.Shake as Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Distribution.PackageDescription
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Types.UnqualComponentName
import Distribution.Verbosity as Cabal
import DynFlags
import GHC
import GHC.Generics
import GHC.Paths (libdir)
import Roll.Cabal
import Roll.Orphans
import System.Console.GetOpt as GetOpt
import System.Directory (listDirectory)

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
      for_ library \lib-> askOracle (BuildComponent package lib)
      for_ testSuites \t -> askOracle (BuildComponent package t)
      -- TODO replace below with implementations
      unless (null subLibraries) (liftIO . throwIO . NotImplemented $ "sub-libraries in " <> cabalFile)
      unless (null executables) (liftIO . throwIO . NotImplemented $ "executables in " <> cabalFile)
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
      -- TODO need hsSourceDirs
      liftIO $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
          -- TODO tmp folders, copy iff build succeeds
          { objectDir = Just objectDir
          , hiDir = Just hiDir
          , outputFile = Nothing
          , importPaths = case hsSourceDirs (libBuildInfo library) of
              [] -> importPaths dflags -- no change
              srcs -> srcs
          }
        setTargets =<< for (exposedModules library) \m -> guessTarget (prettyShow m) Nothing
        void $ load LoadAllTargets
      return ()

    _buildTestSuite <- addOracleCache \(BuildComponent packageId testSuite) -> do
      let -- TODO tmp folders, copy iff build succeeds
        objectDir = ".roll/objects" </> prettyShow packageId
        hiDir = ".roll/interfaces" </> prettyShow packageId
        binDir = ".roll/bin"
      liftIO $ createDirectoryIfMissing True objectDir
      liftIO $ createDirectoryIfMissing True hiDir
      liftIO $ createDirectoryIfMissing True binDir
      -- TODO need hsSourceDirs
      let e_target = case testInterface testSuite of
            TestSuiteExeV10 _ path -> Right path
            TestSuiteLibV09 _ moduleName -> Right (prettyShow moduleName)
            unknown -> Left (show unknown)
      case e_target of
        Left unknown -> putWarn ("unsupported test suite type: " <> show unknown)
        Right testMain -> liftIO $ runGhc (Just libdir) $ do
          dflags <- getSessionDynFlags
          setSessionDynFlags dflags
            { objectDir = Just objectDir
            , hiDir = Just hiDir
            , outputFile = Just (unUnqualComponentName (testName testSuite))
            , importPaths = case hsSourceDirs (testBuildInfo testSuite) of
                [] -> importPaths dflags -- no change
                srcs -> srcs
            }
          setTargets . (:[]) =<< guessTarget testMain Nothing
          void $ load LoadAllTargets
      return ()

    phony "all" do
      cabals <- if null targets -- TODO recursive search?  Targets from project file?
                then liftIO (listDirectory "." <&> filter (".cabal" `isSuffixOf`))
                else return targets
      traverse_ (buildPackage . BuildPackage) cabals


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
type instance RuleResult (BuildComponent TestSuite) = ()
