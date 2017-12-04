{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module File where

import Data.Aeson
import Data.Proxy
import GHC.Generics (Generic)
import OCaml.Export
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Test.Aeson.Internal.ADT.GoldenSpecs


type FilePackage = OCamlModule '["File"] '[]
  :> OCamlTypeInFile Person "test/ocaml/Person"
  :> Automobile
  :> OCamlTypeInFile Business "test/ocaml/Business"


mkGolden :: forall a. (ToADTArbitrary a, ToJSON a) => Proxy a -> IO ()
mkGolden Proxy = mkGoldenFileForType 10 (Proxy :: Proxy a) "test/interface/golden/golden/file"

mkGoldenFiles :: IO ()
mkGoldenFiles = do
  mkGolden (Proxy :: Proxy Person)
  mkGolden (Proxy :: Proxy Automobile)
  mkGolden (Proxy :: Proxy Business)

spec :: Spec
spec = do
  runIO $ mkGoldenFiles
  
  let dir = "test/interface/temp"
  runIO $ mkPackage (Proxy :: Proxy FilePackage) (PackageOptions dir "file" True $ Just $ SpecOptions "__tests__/file" "golden/file" "http://localhost:8083")

data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary

instance ToADTArbitrary Person


data Automobile = Automobile
  { make :: String
  , model :: String
  , year :: Int
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Automobile where
  arbitrary = Automobile <$> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary Automobile


data Business = Business
  { taxId :: String
  , owner :: Person
  , employees :: [Person]
  , companyVehicle :: Maybe Automobile
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Arbitrary Business where
  arbitrary = do
    employeeCount <- choose (0,3)
    Business <$> arbitrary <*> arbitrary <*> vector employeeCount <*> arbitrary

instance ToADTArbitrary Business
