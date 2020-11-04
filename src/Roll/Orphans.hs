{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
module Roll.Orphans where

import Data.Hashable
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.IncludeRenaming
import Distribution.Types.LegacyExeDependency
import Distribution.Types.LibraryVisibility
import Distribution.Types.Mixin
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Types.PkgconfigVersion
import Distribution.Types.PkgconfigVersionRange
import Distribution.Types.UnqualComponentName
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.Utils.ShortText
import Language.Haskell.Extension
import qualified Data.Set as Set

deriving instance Hashable BuildInfo
deriving instance Hashable Dependency
deriving instance Hashable ExeDependency
deriving instance Hashable Extension
deriving instance Hashable IncludeRenaming
deriving instance Hashable KnownExtension
deriving instance Hashable Language
deriving instance Hashable LegacyExeDependency
deriving instance Hashable Library
deriving instance Hashable LibraryName
deriving instance Hashable LibraryVisibility
deriving instance Hashable Mixin
deriving instance Hashable ModuleReexport
deriving instance Hashable ModuleRenaming
deriving instance Hashable PackageIdentifier
deriving instance Hashable PackageName
deriving instance Hashable PkgconfigDependency
deriving instance Hashable PkgconfigName
deriving instance Hashable PkgconfigVersion
deriving instance Hashable PkgconfigVersionRange
deriving instance Hashable ShortText
deriving instance Hashable UnqualComponentName
deriving instance Hashable Version
deriving instance Hashable VersionRange
deriving instance Hashable a => Hashable (PerCompilerFlavor a)

instance Hashable ModuleName where
  hashWithSalt i = hashWithSalt i . components

instance Hashable a => Hashable (Set.Set a) where
  hashWithSalt i = hashWithSalt i . Set.toList
