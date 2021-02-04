{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Description: Parse package-name.cabal files and extract what we need to build.

module Roll.Cabal where

import Distribution.Types.ForeignLib (ForeignLib(..))
import Distribution.Types.Benchmark (Benchmark(..))
import Distribution.Types.PackageId
import DynFlags (xopt_set, xopt_unset, DynFlags)
import qualified GHC.LanguageExtensions as GHC
import qualified Language.Haskell.Extension as Cabal
import Distribution.Types.PackageName (PackageName)
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

data CabalException = MissingCabal String | MultipleCabal String
  deriving (Show, Exception)

data NoPinnedVersion = NoPinnedVersion PackageName
  deriving (Show, Exception)

data NoComponent = NoComponent PackageIdentifier ComponentName
  deriving (Show, Exception)

readPackageDescription :: Verbosity -> FilePath -> IO PackageDescription
readPackageDescription verbosity filepath =
  readGenericPackageDescription verbosity filepath <&> resolveConditionals

resolveConditionals :: GenericPackageDescription -> PackageDescription
resolveConditionals GenericPackageDescription{..} =
  let
    flags = mkFlagAssignment [(flagName, flagDefault) | MkFlag{..} <- genPackageFlags ]
    resolveComponents :: Semigroup a => CondTree ConfVar [Dependency] a -> ([Dependency], a)
    resolveComponents = resolveCondTree flags
    library = snd . resolveComponents <$> condLibrary
    subLibraries = [ (snd $ resolveComponents c) { libName = LSubLibName n } | (n, c) <- condSubLibraries ]
    executables = [ (snd $ resolveComponents c) { exeName = n } | (n, c) <- condExecutables ]
    testSuites = condTestSuites <&> \(n, c) ->
      let
        (deps, testSuite) = resolveComponents c
        newBuildInfo = (testBuildInfo testSuite) { targetBuildDepends = deps }
      in testSuite { testName = n, testBuildInfo = newBuildInfo }
    benchmarks = [ (snd $ resolveComponents c) { benchmarkName = n } | (n, c) <- condBenchmarks ]
    foreignLibs = [ (snd $ resolveComponents c) { foreignLibName = n } | (n, c) <- condForeignLibs ]
  in  packageDescription {library, subLibraries, executables, testSuites, benchmarks, foreignLibs}

resolveCondTree :: Semigroup a =>
  FlagAssignment -> CondTree ConfVar [Dependency] a -> ([Dependency], a)
resolveCondTree flags = simplifyCondTree (confVars flags)

-- TODO load this from the library somehow
-- | The version of ghc package used
ghcVersion :: Version
ghcVersion = mkVersion [8, 10, 3]

confVars :: FlagAssignment -> ConfVar -> Either ConfVar Bool
confVars flags var = case var of
  OS os -> Right (os == buildOS)
  Arch arch -> Right (arch == buildArch)
  Impl GHC versionRange -> Right (ghcVersion `withinRange` versionRange)
  Impl _ _ -> Right False
  Flag name -> case lookupFlagAssignment name flags of
    Just val -> Right val
    Nothing -> Left var

-- | update DynFlags by turning a languge extension on or off.  Used to handle
-- default-extensions section of .cabal files.
applyExtension :: Cabal.Extension -> DynFlags -> DynFlags
applyExtension onOff dflags = case onOff of
  Cabal.EnableExtension ext -> apply xopt_set ext
  Cabal.DisableExtension ext -> apply xopt_unset ext
  Cabal.UnknownExtension _ -> dflags -- TODO log warning?
  where apply f e = maybe dflags (f dflags) (toGhcExtension e)

-- | Cabal recognizes some extensions implemented by hugs but not GHC, and an
-- old Generic extension with no clear modern equivalent.  We don't really
-- expect to encounter these in cabal default-extensions, but to keep this
-- function total we return Maybe.
toGhcExtension :: Cabal.KnownExtension -> Maybe GHC.Extension
toGhcExtension = \case
  Cabal.Generics -> Nothing
  Cabal.ExtensibleRecords -> Nothing
  Cabal.RestrictedTypeSynonyms -> Nothing
  Cabal.HereDocuments -> Nothing
  Cabal.NewQualifiedOperators -> Nothing
  Cabal.XmlSyntax -> Nothing
  Cabal.RegularPatterns -> Nothing
  -- How does GHC handle these pragmas?
  Cabal.Safe -> Nothing
  Cabal.SafeImports -> Nothing
  Cabal.Trustworthy -> Nothing
  Cabal.Unsafe -> Nothing
  -- only extensions implemented in GHC below this line,
  -- following the order of occurrence in GHC.LanguageExtensions.Type
  Cabal.CPP -> Just GHC.Cpp
  Cabal.OverlappingInstances -> Just GHC.OverlappingInstances
  Cabal.UndecidableInstances -> Just GHC.UndecidableInstances
  Cabal.IncoherentInstances -> Just GHC.IncoherentInstances
  Cabal.UndecidableSuperClasses -> Just GHC.UndecidableSuperClasses
  Cabal.MonomorphismRestriction -> Just GHC.MonomorphismRestriction
  Cabal.MonoPatBinds -> Just GHC.MonoPatBinds
  Cabal.MonoLocalBinds -> Just GHC.MonoLocalBinds
  Cabal.RelaxedPolyRec -> Just GHC.RelaxedPolyRec
  Cabal.ExtendedDefaultRules -> Just GHC.ExtendedDefaultRules
  Cabal.ForeignFunctionInterface -> Just GHC.ForeignFunctionInterface
  Cabal.UnliftedFFITypes -> Just GHC.UnliftedFFITypes
  Cabal.InterruptibleFFI -> Just GHC.InterruptibleFFI
  Cabal.CApiFFI -> Just GHC.CApiFFI
  Cabal.GHCForeignImportPrim -> Just GHC.GHCForeignImportPrim
  Cabal.JavaScriptFFI -> Just GHC.JavaScriptFFI
  Cabal.ParallelArrays -> Just GHC.ParallelArrays
  Cabal.Arrows -> Just GHC.Arrows
  Cabal.TemplateHaskell -> Just GHC.TemplateHaskell
  Cabal.TemplateHaskellQuotes -> Just GHC.TemplateHaskellQuotes
  Cabal.QuasiQuotes -> Just GHC.QuasiQuotes
  Cabal.ImplicitParams -> Just GHC.ImplicitParams
  Cabal.ImplicitPrelude -> Just GHC.ImplicitPrelude
  Cabal.ScopedTypeVariables -> Just GHC.ScopedTypeVariables
  Cabal.PatternSignatures -> Just GHC.ScopedTypeVariables -- old synonym
  Cabal.AllowAmbiguousTypes -> Just GHC.AllowAmbiguousTypes
  Cabal.UnboxedTuples -> Just GHC.UnboxedTuples
  Cabal.UnboxedSums -> Just GHC.UnboxedSums
  Cabal.UnliftedNewtypes -> Just GHC.UnliftedNewtypes
  Cabal.BangPatterns -> Just GHC.BangPatterns
  Cabal.TypeFamilies -> Just GHC.TypeFamilies
  Cabal.TypeFamilyDependencies -> Just GHC.TypeFamilyDependencies
  Cabal.TypeInType -> Just GHC.TypeInType
  Cabal.OverloadedStrings -> Just GHC.OverloadedStrings
  Cabal.OverloadedLists -> Just GHC.OverloadedLists
  Cabal.NumDecimals -> Just GHC.NumDecimals
  Cabal.DisambiguateRecordFields -> Just GHC.DisambiguateRecordFields
  Cabal.RecordWildCards -> Just GHC.RecordWildCards
  Cabal.RecordPuns -> Just GHC.RecordPuns
  -- GHC changed the user-facing name but not the constructor?
  Cabal.NamedFieldPuns -> Just GHC.RecordPuns
  Cabal.ViewPatterns -> Just GHC.ViewPatterns
  Cabal.GADTs -> Just GHC.GADTs
  Cabal.GADTSyntax -> Just GHC.GADTSyntax
  Cabal.NPlusKPatterns -> Just GHC.NPlusKPatterns
  Cabal.DoAndIfThenElse -> Just GHC.DoAndIfThenElse
  Cabal.BlockArguments -> Just GHC.BlockArguments
  Cabal.RebindableSyntax -> Just GHC.RebindableSyntax
  Cabal.ConstraintKinds -> Just GHC.ConstraintKinds
  Cabal.PolyKinds -> Just GHC.PolyKinds
  Cabal.DataKinds -> Just GHC.DataKinds
  Cabal.InstanceSigs -> Just GHC.InstanceSigs
  Cabal.ApplicativeDo -> Just GHC.ApplicativeDo

  Cabal.StandaloneDeriving -> Just GHC.StandaloneDeriving
  Cabal.DeriveDataTypeable -> Just GHC.DeriveDataTypeable
  Cabal.AutoDeriveTypeable -> Just GHC.AutoDeriveTypeable
  Cabal.DeriveFunctor -> Just GHC.DeriveFunctor
  Cabal.DeriveTraversable -> Just GHC.DeriveTraversable
  Cabal.DeriveFoldable -> Just GHC.DeriveFoldable
  Cabal.DeriveGeneric -> Just GHC.DeriveGeneric
  Cabal.DefaultSignatures -> Just GHC.DefaultSignatures
  Cabal.DeriveAnyClass -> Just GHC.DeriveAnyClass
  Cabal.DeriveLift -> Just GHC.DeriveLift
  Cabal.DerivingStrategies -> Just GHC.DerivingStrategies
  Cabal.DerivingVia -> Just GHC.DerivingVia

  Cabal.TypeSynonymInstances -> Just GHC.TypeSynonymInstances
  Cabal.FlexibleContexts -> Just GHC.FlexibleContexts
  Cabal.FlexibleInstances -> Just GHC.FlexibleInstances
  Cabal.ConstrainedClassMethods -> Just GHC.ConstrainedClassMethods
  Cabal.MultiParamTypeClasses -> Just GHC.MultiParamTypeClasses
  Cabal.NullaryTypeClasses -> Just GHC.NullaryTypeClasses
  Cabal.FunctionalDependencies -> Just GHC.FunctionalDependencies
  Cabal.UnicodeSyntax -> Just GHC.UnicodeSyntax
  Cabal.ExistentialQuantification -> Just GHC.ExistentialQuantification
  Cabal.MagicHash -> Just GHC.MagicHash
  Cabal.EmptyDataDecls -> Just GHC.EmptyDataDecls
  Cabal.KindSignatures -> Just GHC.KindSignatures
  Cabal.RoleAnnotations -> Just GHC.RoleAnnotations
  Cabal.ParallelListComp -> Just GHC.ParallelListComp
  Cabal.TransformListComp -> Just GHC.TransformListComp
  Cabal.MonadComprehensions -> Just GHC.MonadComprehensions
  Cabal.GeneralizedNewtypeDeriving -> Just GHC.GeneralizedNewtypeDeriving
  Cabal.GeneralisedNewtypeDeriving -> Just GHC.GeneralizedNewtypeDeriving
  Cabal.DoRec -> Just GHC.RecursiveDo
  Cabal.RecursiveDo -> Just GHC.RecursiveDo
  Cabal.PostfixOperators -> Just GHC.PostfixOperators
  Cabal.TupleSections -> Just GHC.TupleSections
  Cabal.PatternGuards -> Just GHC.PatternGuards
  Cabal.LiberalTypeSynonyms -> Just GHC.LiberalTypeSynonyms
  Cabal.RankNTypes -> Just GHC.RankNTypes
  -- deprecated synonyms
  Cabal.Rank2Types -> Just GHC.RankNTypes
  Cabal.PolymorphicComponents -> Just GHC.RankNTypes
  Cabal.ImpredicativeTypes -> Just GHC.ImpredicativeTypes
  Cabal.TypeOperators -> Just GHC.TypeOperators
  Cabal.ExplicitNamespaces -> Just GHC.ExplicitNamespaces
  Cabal.PackageImports -> Just GHC.PackageImports
  Cabal.ExplicitForAll -> Just GHC.ExplicitForAll
  -- These 3 are not supported in Cabal
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/11359
  -- Cabal.AlternativeLayoutRule -> Just GHC.AlternativeLayoutRule
  -- Cabal.AlternativeLayoutRuleTransitional -> Just GHC.AlternativeLayoutRuleTransitional
  -- Cabal.RelaxedLayout -> Just GHC.RelaxedLayout
  Cabal.DatatypeContexts -> Just GHC.DatatypeContexts
  Cabal.NondecreasingIndentation -> Just GHC.NondecreasingIndentation
  Cabal.TraditionalRecordSyntax -> Just GHC.TraditionalRecordSyntax
  Cabal.LambdaCase -> Just GHC.LambdaCase
  Cabal.MultiWayIf -> Just GHC.MultiWayIf
  Cabal.BinaryLiterals -> Just GHC.BinaryLiterals
  Cabal.NegativeLiterals -> Just GHC.NegativeLiterals
  Cabal.HexFloatLiterals -> Just GHC.HexFloatLiterals
  Cabal.DuplicateRecordFields -> Just GHC.DuplicateRecordFields
  Cabal.OverloadedLabels -> Just GHC.OverloadedLabels
  Cabal.EmptyCase -> Just GHC.EmptyCase
  Cabal.PatternSynonyms -> Just GHC.PatternSynonyms
  Cabal.PartialTypeSignatures -> Just GHC.PartialTypeSignatures
  Cabal.NamedWildCards -> Just GHC.NamedWildCards
  Cabal.StaticPointers -> Just GHC.StaticPointers
  Cabal.TypeApplications -> Just GHC.TypeApplications
  Cabal.Strict -> Just GHC.Strict
  Cabal.StrictData -> Just GHC.StrictData
  Cabal.MonadFailDesugaring -> Just GHC.MonadFailDesugaring
  Cabal.EmptyDataDeriving -> Just GHC.EmptyDataDeriving
  Cabal.NumericUnderscores -> Just GHC.NumericUnderscores
  Cabal.QuantifiedConstraints -> Just GHC.QuantifiedConstraints
  Cabal.StarIsType -> Just GHC.StarIsType
  Cabal.ImportQualifiedPost -> Just GHC.ImportQualifiedPost
  Cabal.CUSKs -> Just GHC.CUSKs
  Cabal.StandaloneKindSignatures -> Just GHC.StandaloneKindSignatures
