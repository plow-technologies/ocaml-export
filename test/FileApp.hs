{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TemplateHaskell #-}

module FileApp where

import qualified Data.Map as Map
import Data.Proxy
import OCaml.Export
import File
import Servant
import Test.Hspec
import Util

$(mkServer "FilePackage" (Proxy :: Proxy FilePackage))

fileMap :: Map.Map String EmbeddedOCamlFiles
fileMap = Map.fromList $(mkFiles True False (Proxy :: Proxy FilePackage))

compareInterfaceFiles = compareFiles "test/interface" "file" True

spec :: Spec
spec = do
  runIO $ mkGoldenFiles
  
  let dir = "test/interface/temp"
  runIO $ mkPackage (Proxy :: Proxy FilePackage) (PackageOptions dir "file" fileMap True $ Just $ SpecOptions "__tests__/file" "golden/file" "http://localhost:8083")

  describe "OCaml Declaration with Interface: Product Types" $ do
    compareInterfaceFiles "File"
