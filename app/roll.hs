{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import qualified Data.Set as Set
import Control.Monad.IO.Class
import Data.Function ((&))
import qualified System.Directory as Sys
import qualified Data.Map as Map
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Distribution.Types.Dependency (Dependency(..))
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
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
import System.Posix
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

    -- packageVersions :: HM.HashMap PackageName Version
    packageVersions <- liftIO do
      pinned <- fileExist "package-versions.txt"
      if pinned
        then T.readFile "package-versions.txt"
           <&> T.lines
           <&> map (T.breakOn " ==") -- TODO handle installed
           <&> map (\(n, v) -> (mkPackageName (T.unpack n), Version v))
           <&> Map.fromList
        else return mempty

    fetchPackage <- newCache \(name, Version v) -> do
      let package = unPackageName name <> "-" <> T.unpack v
      exists <- liftIO $ Sys.doesDirectoryExist package
      -- someday, in-process, don't depend on other build tools
      unless exists (command_ [] "stack" [ "unpack", package ])
      return package -- path to directory where we unpacked

    let findCabal dir = do
          -- TODO generalize to include package.yaml
          cabals <- getDirectoryFiles "" [ dir <> "/*.cabal" ]
          case cabals of
            [] -> throwM $ MissingCabal dir
            [ one ] -> return one
            _more -> throwM $ MultipleCabal dir

    buildPackage <- newCache \(BuildPackage cabalFile) -> do
      PackageDescription{..} <- parseCabal cabalFile
      for_ library \lib -> askOracle (BuildComponent package lib)
      for_ testSuites \t -> askOracle (BuildComponent package t)
      -- TODO replace below with implementations
      unless (null subLibraries) (throwM . NotImplemented $ "sub-libraries in " <> cabalFile)
      unless (null executables) (throwM . NotImplemented $ "executables in " <> cabalFile)
      unless (null benchmarks) (putWarn ("benchmarks not implemented: " <> cabalFile))
      unless (null foreignLibs) (putWarn ("foreign libraries not implemented: " <> cabalFile))

    let
      buildDependencies :: [Dependency] -> Action ()
      buildDependencies deps = do
        void $ forP deps \(Dependency pkgName _ _) -> unless (pkgName `Set.member` builtinPackages) do
          -- fetch & build dependencies
          version <- Map.lookup pkgName packageVersions
                    & \case
                        Just v -> return v
                        -- TODO local packages
                        Nothing -> throwM (NoPinnedVersion pkgName)
          path <- fetchPackage (pkgName, version)
          cabal <- findCabal path
          buildPackage (BuildPackage cabal) -- TODO library only

    -- TODO need separate oracles per component type (or sum type),
    -- but most of this action should be reusable
    _buildLibrary <- addOracleCache \(BuildComponent packageId library) -> do
      void $ buildDependencies (targetBuildDepends (libBuildInfo library))
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
      void $ buildDependencies (targetBuildDepends (testBuildInfo testSuite))
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

newtype Version = Version Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable)

throwM :: (Exception e, MonadIO m) => e -> m a
throwM = liftIO . throwIO

builtinPackages :: Set.Set PackageName
builtinPackages = Set.fromList . map mkPackageName $
  [ "binary"
  , "bytestring"
  , "Cabal"
  , "containers"
  , "deepseq"
  , "directory"
  , "filepath"
  , "ghc-compact"
  , "ghc-prim"
  , "integer-gmp"
  , "mtl"
  , "parsec"
  , "pretty"
  , "process"
  , "stm"
  , "template-haskell"
  , "terminfo"
  , "text"
  , "time"
  , "transformers"
  , "unix"
  , "xhtml"
  ]
