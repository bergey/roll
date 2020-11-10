{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Description: Parse package-name.cabal files and extract what we need to build.

module Roll.Cabal where

import Control.Exception
import Control.Monad.Catch
import Data.Functor ((<&>))
import Data.Text (Text)
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.System
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.UnqualComponentName
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.Verbosity
import qualified Data.Text as T

data NotImplemented = NotImplemented String
  deriving (Show, Exception)

readPackageDescription :: Verbosity -> FilePath -> IO PackageDescription
readPackageDescription verbosity filepath =
  readGenericPackageDescription verbosity filepath <&> resolveConditionals

resolveConditionals :: GenericPackageDescription -> PackageDescription
resolveConditionals GenericPackageDescription{..} =
  let
    resolveComponents :: Semigroup a => (UnqualComponentName, CondTree ConfVar [Dependency] a) -> a
    resolveComponents = snd . resolveCondTree . snd
    library = snd . resolveCondTree <$> condLibrary
    subLibraries = resolveComponents <$> condSubLibraries
    executables = resolveComponents <$> condExecutables
    testSuites = resolveComponents <$> condTestSuites
    benchmarks = resolveComponents <$> condBenchmarks
    foreignLibs = resolveComponents <$> condForeignLibs
  in  packageDescription {library, subLibraries, executables, testSuites, benchmarks, foreignLibs}

resolveCondTree :: Semigroup a => CondTree ConfVar [Dependency] a -> ([Dependency], a)
resolveCondTree = simplifyCondTree confVars

-- TODO load this from the library somehow
-- | The version of ghc package used
ghcVersion :: Version
ghcVersion = mkVersion [8, 10, 1]

confVars :: ConfVar -> Either ConfVar Bool
confVars var = case var of
  OS os -> Right (os == buildOS)
  Arch arch -> Right (arch == buildArch)
  Impl GHC versionRange -> Right (ghcVersion `withinRange` versionRange)
  Impl _ _ -> Right False
  _ -> Left var
