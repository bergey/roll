{-# LANGUAGE TupleSections #-}
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

import Distribution.Parsec (simpleParsec)
import Distribution.Version (versionNumbers)
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
import Data.List (isSuffixOf, intercalate)
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
    case targets of
      [] -> want ["all"]
      _
        | "clean" `elem` targets -> want ["clean"]
        -- TODO real target syntax
        | otherwise -> want targets

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
           <&> map (\(n, v) ->
                       let
                         name = mkPackageName (T.unpack n)
                         m_version = simpleParsec . drop 3 . T.unpack $ v
                       in fmap (name,) m_version)
           <&> catMaybes
           <&> Map.fromList
        else return mempty

    fetchPackage <- newCache \(PackageIdentifier name version) -> do
      let
        v = intercalate "." . map show . versionNumbers $ version
        package = unPackageName name <> "-" <> v
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

    let needAllComponents cabalFile = do
          PackageDescription{..} <- parseCabal cabalFile
          -- TODO parallel need all components
          for_ library \lib -> askOracle (BuildLibrary package (libName lib))
          for_ testSuites \t -> askOracle (BuildTestSuite package (testName t))
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
          pkgDesc <- parseCabal =<< findCabal =<< fetchPackage (PackageIdentifier pkgName version)
          askOracle (BuildLibrary (package pkgDesc) LMainLibName)

    void $ addOracle \(BuildLibrary pkgId m_name) -> do
      srcDir <- fetchPackage pkgId
      pkgDesc <- parseCabal =<< findCabal srcDir
      spec <- onNothing (throwM (NoComponent pkgId (CLibName m_name))) $
              case m_name of
                LMainLibName -> library pkgDesc
                LSubLibName name -> findComponent subLibraries libName m_name pkgDesc
      let buildInfo = libBuildInfo spec
      void $ buildDependencies (targetBuildDepends buildInfo)
      let -- TODO tmp folders, copy iff build succeeds
        objectDir = ".roll/objects" </> prettyShow pkgId
        hiDir = ".roll/interfaces" </> prettyShow pkgId
      liftIO $ createDirectoryIfMissing True objectDir
      liftIO $ createDirectoryIfMissing True hiDir
      -- TODO how does shake handle Exceptions?
      -- defaultErrorHandler defaultLogAction $ do
      -- TODO need hsSourceDirs
      liftIO $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let packageDynFlags =
              (foldr applyExtension dflags (defaultExtensions buildInfo ++ oldExtensions buildInfo))
              { objectDir = Just objectDir
              , hiDir = Just hiDir
              , outputFile = Nothing
              , importPaths = case hsSourceDirs buildInfo of
                  [] -> [srcDir] -- package dir is root of module path
                  srcs -> map (srcDir </>) srcs
              }
        setSessionDynFlags packageDynFlags
        setTargets =<< for (exposedModules spec) \m -> guessTarget (prettyShow m) Nothing
        void $ load LoadAllTargets
      return ()

    void $ addOracle \(BuildTestSuite pkgId name) -> do
      srcDir <- fetchPackage pkgId
      pkgDesc <- parseCabal =<< findCabal srcDir
      spec <- findComponent testSuites testName name pkgDesc
        & onNothing (throwM (NoComponent pkgId (CTestName name)))
      void $ buildDependencies (targetBuildDepends (testBuildInfo spec))
      let -- TODO tmp folders, copy iff build succeeds
        objectDir = ".roll/objects" </> prettyShow pkgId
        hiDir = ".roll/interfaces" </> prettyShow pkgId
        binDir = ".roll/bin"
      liftIO $ createDirectoryIfMissing True objectDir
      liftIO $ createDirectoryIfMissing True hiDir
      liftIO $ createDirectoryIfMissing True binDir
      -- TODO need hsSourceDirs
      let e_target = case testInterface spec of
            TestSuiteExeV10 _ path -> Right path
            TestSuiteLibV09 _ moduleName -> Right (prettyShow moduleName)
            unknown -> Left (show unknown)
      case e_target of
        Left unknown -> putWarn ("unsupported test suite type: " <> show unknown)
        Right testMain -> liftIO $ runGhc (Just libdir) $ do
          dflags <- getSessionDynFlags
          -- todo applyExtension (or merge duplicated code)
          setSessionDynFlags dflags
            { objectDir = Just objectDir
            , hiDir = Just hiDir
            , outputFile = Just (unUnqualComponentName (testName spec))
            , importPaths = case hsSourceDirs (testBuildInfo spec) of
                [] -> [srcDir]-- no change
                srcs -> map (srcDir </>) srcs
            }
          setTargets . (:[]) =<< guessTarget testMain Nothing
          void $ load LoadAllTargets
      return ()

    phony "all" do
      -- TODO recursive search?  Targets from project file?
      cabals <- liftIO (listDirectory "." <&> filter (".cabal" `isSuffixOf`))
      need cabals

    phonys \cabalFile ->
      if ".cabal" `isSuffixOf` cabalFile
      then Just (needAllComponents cabalFile)
      else Nothing


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

data BuildLibrary = BuildLibrary PackageIdentifier LibraryName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)
type instance RuleResult BuildLibrary = ()

data BuildExecutable = BuildExecutable PackageIdentifier UnqualComponentName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)
type instance RuleResult BuildExecutable = ()

data BuildTestSuite = BuildTestSuite PackageIdentifier UnqualComponentName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)
type instance RuleResult BuildTestSuite = ()

data BuildBenchmark = BuildBenchmark PackageIdentifier UnqualComponentName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)
type instance RuleResult BuildBenchmark = ()

data BuildForeignLibrary =
  BuildForeignLibrary PackageIdentifier UnqualComponentName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)
type instance RuleResult BuildForeignLibrary = ()

findComponent :: Eq n => (PackageDescription -> [c]) -> (c -> n) -> n ->
  PackageDescription -> Maybe c
findComponent cs cn n pkgDesc = find (\c -> cn c == n) (cs pkgDesc)

onNothing :: Applicative f => f a -> Maybe a -> f a
onNothing act = maybe act pure

throwM :: (Exception e, MonadIO m) => e -> m a
throwM = liftIO . throwIO

builtinPackages :: Set.Set PackageName
builtinPackages = Set.fromList . map mkPackageName $
  [ "array"
  , "base"
  , "binary"
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
