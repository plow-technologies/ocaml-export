module Util where

import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import OCaml.Export
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
