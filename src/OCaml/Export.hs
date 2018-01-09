{-|
Module      : OCaml.Export
Description : Export everything from one module
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

-}

module OCaml.Export
  ( OCamlPackage
  , NoDependency
  , OCamlModule
  , OCamlSubModule
  , OCamlTypeInFile
  , HaskellTypeName

  , PackageOptions (..)
  , defaultPackageOptions
  , SpecOptions (..)
  , defaultSpecOptions

  , EmbeddedOCamlFiles (..)

  , mkPackage
  , mkFiles
  , mkOCamlTypeMetaData
  
  -- OCaml.BuckleScript.Types
  , OCamlType (..)
  , typeableToOCamlType
  , TypeParameterRef0
  , TypeParameterRef1
  , TypeParameterRef2
  , TypeParameterRef3
  , TypeParameterRef4
  , TypeParameterRef5

  -- servant spec
  , mkOCamlSpecServer
  , MkOCamlSpecAPI
  , mkGoldenFiles

  -- re-export
  , Proxy (..)
  , (:>)
  , (:<|>) (..)
  , Application
  , Server
  , serve

  ) where

import Data.Proxy (Proxy (..))

import OCaml.BuckleScript.Types

import OCaml.BuckleScript.Internal.Module
import OCaml.BuckleScript.Internal.Package
import OCaml.BuckleScript.Internal.Spec

import Servant (Application, Server, serve)
import Servant.API ((:>), (:<|>) (..))
