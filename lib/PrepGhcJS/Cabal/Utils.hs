{-# LANGUAGE RecordWildCards, QuasiQuotes #-}
module PrepGhcJS.Cabal.Utils where

import Distribution.PackageDescription.Parse
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Verbosity
import Data.Monoid
import qualified Data.Text as T
import Data.Version
import Data.String.QM

getBootDescr fp = getDescr $ fp <> "/" <> fp

showPkg PackageIdentifier{..} =
  (T.pack $ unPackageName pkgName, T.pack $ showVersion pkgVersion)

getDescr fp = do
   gd <- readPackageDescription verbose (fp <> ".cabal")
   return $ package $ packageDescription gd

whipeConstraints fp = do
   gd <- readPackageDescription verbose (fp <> ".cabal")
--    writePackageDescription (fp <> "-new.cabal") gd
   return gd
--    return $ package $ packageDescription gd

fakePackage = [qq|
name:                boot-ng
version:             0.1.0.0
synopsis:            Simple project template from stack
description:         Please see README.md
homepage:            http://github.com/tolysz/boot-ng#readme
license:             PublicDomain
license-file:        LICENSE
author:              Marcin Tolysz
maintainer:          tolysz@gmail.com
copyright:           2016(c) Marcin Tolysz
category:            Web
build-type:          Simple
cabal-version:       >=1.10

executable boot-ng
--  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends: |]
