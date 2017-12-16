module OCaml.BuckleScript
  ( OCamlPackage
  , NoDependency
  , OCamlModule
  , OCamlSubModule
  , OCamlTypeInFile

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
  , TypeParameterRef0
  , TypeParameterRef1
  , TypeParameterRef2
  , TypeParameterRef3
  , TypeParameterRef4
  , TypeParameterRef5

  -- servant spec
  , mkOCamlSpecServer
  , MkOCamlSpecAPI

  -- re-export
  , (:>)
  , (:<|>) (..)
  , Application
  , Server
  , serve

  ) where

import OCaml.BuckleScript.Types

import OCaml.BuckleScript.Internal.Module
import OCaml.BuckleScript.Internal.Package
import OCaml.BuckleScript.Internal.Spec

import Servant (Application, Server, serve)
import Servant.API ((:>), (:<|>) (..))
