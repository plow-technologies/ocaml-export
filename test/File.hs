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
import GHC.Generics -- (Generic)

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
    :> OCamlTypeInFile NonGenericSumTypeInFile "test/ocaml/NonGenericSumTypeInFile"
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

data NonGenericSumTypeInFile
  = NGSumTypeA String Int
  | NGSumTypeB Int Double
  deriving (Show, Eq)

instance ToJSON NonGenericSumTypeInFile where
  toJSON (NGSumTypeA a b) =
    object
      [ "tag" .= ("NGSumTypeA" :: String)
      , "a"   .= a
      , "b"   .= b
      ]

  toJSON (NGSumTypeB a b) =
    object
      [ "tag" .= ("NGSumTypeB" :: String)
      , "a"   .= a
      , "b"   .= b
      ]

instance FromJSON NonGenericSumTypeInFile where
  parseJSON = withObject "NonGenericSumTypeInFile" $ \o -> do
    result <- o .: "tag"
    case result of
      String "NGSumTypeA" ->
        NGSumTypeA <$> o .: "a"
                   <*> o .: "b"
      String "NGSumTypeB" ->
        NGSumTypeB <$> o .: "a"
                   <*> o .: "b"
      _ -> fail "Valid tag not found"

instance ToADTArbitrary NonGenericSumTypeInFile where
  toADTArbitrarySingleton Proxy =
    ADTArbitrarySingleton "File" "NonGenericSumTypeInFile"
      <$> oneof
        [ ConstructorArbitraryPair "NGSumTypeA" <$> (NGSumTypeA <$> arbitrary <*> arbitrary)
        , ConstructorArbitraryPair "NGSumTypeB" <$> (NGSumTypeB <$> arbitrary <*> arbitrary)
        ]

  toADTArbitrary Proxy =
    ADTArbitrary "File" "NonGenericSumTypeInFile"
      <$> sequence
        [ ConstructorArbitraryPair "NGSumTypeA" <$> (NGSumTypeA <$> arbitrary <*> arbitrary)
        , ConstructorArbitraryPair "NGSumTypeB" <$> (NGSumTypeB <$> arbitrary <*> arbitrary)
        ]

instance OCamlType NonGenericSumTypeInFile where
  toOCamlType _ = typeableToOCamlType (Proxy :: Proxy NonGenericSumTypeInFile)

--instance Generic NonGenericSumTypeInFile where
--  from _ = M1 D (MetaData "Tree" "Main" "package-name" False) _ _
