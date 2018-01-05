{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
import OCaml.Export

type FilePackage = OCamlPackage "" NoDependency :>
  (OCamlModule '["File"]
    :> OCamlTypeInFile Person "test/ocaml/Person"
    :> Automobile
    :> OCamlTypeInFile Business "test/ocaml/Business"
    :> OCamlTypeInFile (Wrapper TypeParameterRef0 TypeParameterRef1) "test/ocaml/Wrapper"
    :> AutoDependingOnManual
  )

data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary

instance ToADTArbitrary Person

instance OCamlType Person where
  toOCamlType _ = typeableToOCamlType (Proxy :: Proxy Person)

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

instance OCamlType Business where
  toOCamlType _ = typeableToOCamlType (Proxy :: Proxy Business)

data Wrapper a b = Wrapper
  { wrapperA :: a
  , wrapperB :: b
  , wrapperC :: String
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Wrapper a b) where
  arbitrary = Wrapper <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, ToADTArbitrary a, Arbitrary b, ToADTArbitrary b) => ToADTArbitrary (Wrapper a b)

instance OCamlType (Wrapper TypeParameterRef0 TypeParameterRef1) where
  toOCamlType _ = typeableToOCamlType (Proxy :: Proxy (Wrapper TypeParameterRef0 TypeParameterRef1))

data AutoDependingOnManual = AutoDependingOnManual
  { abc :: String
  , bbBusiness :: Business
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary AutoDependingOnManual where
  arbitrary = AutoDependingOnManual <$> arbitrary <*> arbitrary

instance ToADTArbitrary AutoDependingOnManual
