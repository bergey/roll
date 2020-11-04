{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Description: Parse package-name.cabal files and extract what we need to build.

module Roll.Cabal where

import Control.Exception
import Control.Monad.Catch
import Data.Text (Text)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Types.Dependency
import Distribution.Types.UnqualComponentName
import Distribution.Verbosity
import qualified Data.Text as T

data NotImplemented = NotImplemented String
  deriving (Show, Exception)

readPackageDescription :: Verbosity -> FilePath -> IO PackageDescription
readPackageDescription verbosity filepath =
  readGenericPackageDescription verbosity filepath >>= resolveConditionals

resolveConditionals :: MonadThrow m => GenericPackageDescription -> m PackageDescription
resolveConditionals GenericPackageDescription{..} = do
  library <- traverse (resolveCondTree . ("library",)) condLibrary
  subLibraries <- traverse resolveCondTree condSubLibraries
  executables <- traverse resolveCondTree condExecutables
  testSuites <- traverse resolveCondTree condTestSuites
  benchmarks <- traverse resolveCondTree condBenchmarks
  foreignLibs <- traverse resolveCondTree condForeignLibs
  return packageDescription {library, subLibraries, executables, testSuites, benchmarks, foreignLibs}

resolveCondTree :: MonadThrow m => (UnqualComponentName, CondTree ConfVar [Dependency] a) -> m a
resolveCondTree (name, CondNode{..}) = case condTreeComponents of
  [] -> return condTreeData
  _ -> throwM (NotImplemented ("conditional branches in cabal: " <> show name))
