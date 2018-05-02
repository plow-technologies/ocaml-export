{-# LANGUAGE TemplateHaskell #-}

module ProductApp where

import OCaml.Export
import Product
-- containers
import qualified Data.Map as Map
-- hspec
import Test.Hspec

import Data.FileEmbed
import qualified Data.ByteString

myFile :: Data.ByteString.ByteString
myFile = $(embedFile "test/ocaml/Simple.ml")

-- fileMap :: Map.Map String EmbeddedOCamlFiles
-- fileMap = Map.fromList $(mkFiles True False (Proxy :: Proxy ProductPackage))

fileMap :: [(String,String)]
fileMap = mkFiles True False (Proxy :: Proxy ProductPackage)

fileMap2 :: [(String,String)]
fileMap2 = mkFiles True False (Proxy :: Proxy TTPackage)

fileMap3 :: [(String,String)]
fileMap3 = mkFiles True False (Proxy :: Proxy TTTPackage)


$(mkOCamlSpecServer "ProductPackage" (Proxy :: Proxy ProductPackage))

spec :: Spec
spec = do
  runIO $ do
    print myFile
    print "printing fileMap"
    print fileMap
    print fileMap2
    print fileMap3
  {-
  runIO $ mkGoldenFiles (Proxy :: Proxy ProductPackage) 10 "test/interface/golden/golden/product"
  runGoldenSpec (Proxy :: Proxy ProductPackage) 10 "test/interface/golden/golden/product"

  let dir = "test/interface/temp"

  -- create spec to be tested against servant
  runIO $
    mkPackage
      (Proxy :: Proxy ProductPackage)
      (PackageOptions dir "product" fileMap True $
       Just $
         SpecOptions
           "__tests__/product-servant"
           "golden/product"
           (Just "http://localhost:8081"))

  -- create spec to be tested against files only
  runIO $
    mkPackage
      (Proxy :: Proxy ProductPackage)
      (PackageOptions dir "product" fileMap True $
       Just $
         SpecOptions
           "__tests__/product"
           "golden/product"
           Nothing)
  
  describe "OCaml Declaration with Interface: Product Types" $ do
    compareInterfaceFiles "Person"
    compareInterfaceFiles "Company"
    compareInterfaceFiles "Card"
    compareInterfaceFiles "OneTypeParameter"
    compareInterfaceFiles "TwoTypeParameters"
    compareInterfaceFiles "ThreeTypeParameters"
    compareInterfaceFiles "SubTypeParameter"
    compareInterfaceFiles "UnnamedProduct"
    compareInterfaceFiles "ComplexProduct"
    compareInterfaceFiles "Wrapper"
  -}
