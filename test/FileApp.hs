{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module FileApp where
-- containers
import qualified Data.Map as Map
-- hspec
import Test.Hspec
-- servant-server
import Servant
-- ocaml-export
import OCaml.Export
import File
import Util

$(mkOCamlSpecServer "FilePackage" (Proxy :: Proxy FilePackage))

fileMap :: Map.Map String EmbeddedOCamlFiles
fileMap = Map.fromList $(mkFiles True False (Proxy :: Proxy FilePackage))

compareInterfaceFiles :: FilePath -> SpecWith ()
compareInterfaceFiles = compareFiles "test/interface" "file" True

spec :: Spec
spec = do
  runIO $ mkGoldenFiles (Proxy :: Proxy FilePackage) 10 "test/interface/golden/golden/file"
  
  let dir = "test/interface/temp"
  runIO $ mkPackage (Proxy :: Proxy FilePackage) (PackageOptions dir "file" fileMap True $ Just $ SpecOptions "__tests__/file" "golden/file" "http://localhost:8083")

  describe "OCaml Declaration with Interface: Product Types" $ do
    compareInterfaceFiles "File"
