{-# LANGUAGE TemplateHaskell #-}

module Shared.Types.Reason.Package
  ( fileMap
  , SharedTypesPackage
  ) where

import qualified Data.Map as Map
import Data.Proxy
import OCaml.Export
import Shared.Types.Reason.Types

fileMap :: Map.Map String EmbeddedOCamlFiles
fileMap = Map.fromList $(mkFiles True False (Proxy :: Proxy SharedTypesPackage))
