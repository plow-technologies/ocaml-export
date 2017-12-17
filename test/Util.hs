module Util where

-- base
import Data.Monoid ((<>))
import Data.Time

-- text
import qualified Data.Text.IO as T

-- ocaml-export
import OCaml.Export hiding (Options)


import System.Directory (doesFileExist)

-- filepath
import System.FilePath.Posix ((</>))

-- hspec
import Test.Hspec

-- QuickCheck
import Test.QuickCheck

-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT
import Test.Aeson.Internal.ADT.GoldenSpecs

instance Arbitrary UTCTime where
  arbitrary =
    UTCTime <$> (ModifiedJulianDay <$> (2000 +) <$> arbitrary)
            <*> pure 1.011
--            <*> (fromRational . toRational . (\f -> fromInteger $ round $ f * (10^2) / (10.0^^2)) <$> choose (0:: Double, 86400))

data ADT
  = Options

adtToPath :: ADT -> FilePath
adtToPath Options = "options"


compareFiles :: FilePath -> FilePath -> Bool -> FilePath -> SpecWith ()
compareFiles rootDir categoryDir compareInterfaceAndSpecFiles typeName =
  it typeName $ do
    automated   <- T.readFile (testPath   </> typeName <> ".ml")
    handWritten <- T.readFile (goldenPath </> typeName <> ".ml")
    automated `shouldBe` handWritten
    if compareInterfaceAndSpecFiles
      then do
        automatedI   <- T.readFile (testPath   </> typeName <> ".mli")
        handWrittenI <- T.readFile (goldenPath </> typeName <> ".mli")
        automatedI `shouldBe` handWrittenI
        
        automatedS   <- T.readFile (testSpecPath   </> typeName <> "_spec" <> ".ml")
        handWrittenS <- T.readFile (goldenSpecPath </> typeName <> "_spec" <> ".ml")
        automatedS `shouldBe` handWrittenS
      else pure ()
  where
    testPath   = rootDir </> "temp" </> categoryDir
    goldenPath = rootDir </> "golden" </> categoryDir
    testSpecPath   = rootDir </> "temp" </> "__tests__" </> categoryDir
    goldenSpecPath = rootDir </> "golden" </> "__tests__" </> categoryDir

{-
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

    fe <- doesFileExist (specGoldenPath <> "/" <> typeName <> "_spec.ml")
    if fe
      then do
        automated3   <- T.readFile (specPath <> "/" <> typeName <> "_spec.ml")
        handWritten3 <- T.readFile (specGoldenPath <> "/" <> typeName <> "_spec.ml")
        automated3 `shouldBe` handWritten3
      else pure ()
  where
    adtPath    = adtToPath adt
    testPath   = "test/interface/temp/" <> adtPath
    goldenPath = "test/interface/golden/" <> adtPath
    specPath  = "test/interface/temp/__tests__/" <> adtPath
    specGoldenPath  = "test/interface/golden/__tests__/" <> adtPath
-}
