{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module File where

-- base
import Data.Proxy
import GHC.Generics (Generic)

-- aeson
import Data.Aeson

-- hspec
import Test.Hspec

-- hspec-golden-aeson
import Test.Aeson.Internal.ADT.GoldenSpecs

-- QuickCheck
import Test.QuickCheck

-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT

-- ocaml-export
import OCaml.Export hiding (mkGoldenFiles)

type FilePackage = OCamlPackage "" NoDependency :>
  (OCamlModule '["File"]
    :> OCamlTypeInFile Person "test/ocaml/Person"
    :> Automobile
    :> OCamlTypeInFile Business "test/ocaml/Business"
  )


mkGolden :: forall a. (ToADTArbitrary a, ToJSON a) => Proxy a -> IO ()
mkGolden Proxy = mkGoldenFileForType 10 (Proxy :: Proxy a) "test/interface/golden/golden/file"

mkGoldenFiles :: IO ()
mkGoldenFiles = do
  mkGolden (Proxy :: Proxy Person)
  mkGolden (Proxy :: Proxy Automobile)
  mkGolden (Proxy :: Proxy Business)


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
