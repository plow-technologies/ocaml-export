module Util where

import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import OCaml.Export
import System.Directory (doesFileExist)
import Test.Hspec

data ADT
  = Product
  | Sum
  | Primitive
  | Complex

adtToPath :: ADT -> FilePath
adtToPath Product = "product"
adtToPath Sum = "sum"
adtToPath Primitive = "primitive"
adtToPath Complex = "complex"

testOCamlType :: ADT -> OCamlFile -> FilePath -> SpecWith ()
testOCamlType adt ocamlFile typeName =
  it typeName $ do
    createOCamlFile testPath ocamlFile
    automated   <- T.readFile (testPath   <> "/" <> typeName <> ".ml")
    handWritten <- T.readFile (goldenPath <> "/" <> typeName <> ".ml")
    automated `shouldBe` handWritten
  where
    adtPath    = adtToPath adt
    testPath   = "test/nointerface/temp/" <> adtPath
    goldenPath = "test/nointerface/golden/" <> adtPath

testOCamlTypeWithInterface :: ADT -> FilePath -> OCamlInterface -> SpecWith ()
testOCamlTypeWithInterface adt typeName ocamlFile =
  it typeName $ do
    createOCamlFileWithInterface testPath specPath typeName ocamlFile
    automated   <- T.readFile (testPath   <> "/" <> typeName <> ".ml")
    handWritten <- T.readFile (goldenPath <> "/" <> typeName <> ".ml")

    automated2   <- T.readFile (testPath   <> "/" <> typeName <> ".mli")
    handWritten2 <- T.readFile (goldenPath <> "/" <> typeName <> ".mli")

    automated `shouldBe` handWritten
    automated2 `shouldBe` handWritten2

    fe <- doesFileExist (specGoldenPath <> "/" <> typeName <> ".ml")
    if fe
      then do
        automated3   <- T.readFile (specPath   <> "/" <> typeName <> ".ml")
        handWritten3 <- T.readFile (specGoldenPath <> "/" <> typeName <> ".ml")
        automated3 `shouldBe` handWritten3
      else pure ()
  where
    adtPath    = adtToPath adt
    testPath   = "test/interface/temp/" <> adtPath
    goldenPath = "test/interface/golden/" <> adtPath
    specPath  = "test/interface/temp/__tests__/" <> adtPath
    specGoldenPath  = "test/interface/golden/__tests__/" <> adtPath

