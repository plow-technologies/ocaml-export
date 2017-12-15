{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleInstances #-}


-- for OCaml Module
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TemplateHaskell #-}


module Product where

import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics
import OCaml.Export
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Test.Aeson.Internal.ADT.GoldenSpecs
import Util
import qualified Data.Map as Map

type ProductPackage
  = OCamlPackage "product" NoDependency
    :> (OCamlModule '["SimpleChoice"] :> SimpleChoice
  :<|> OCamlModule '["Person"] :> Person
  :<|> OCamlModule '["Company"] :> Company
  :<|> OCamlModule '["Card"] :> Suit :> Card
  :<|> OCamlModule '["CustomOption"] :> Company2
  :<|> OCamlModule '["OneTypeParameter"] :> OneTypeParameter TypeParameterRef0
  :<|> OCamlModule '["TwoTypeParameters"] :> TwoTypeParameters TypeParameterRef0 TypeParameterRef1
  :<|> OCamlModule '["ThreeTypeParameters"] :> Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2
  :<|> OCamlModule '["SubTypeParameter"] :> SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2)

mkGolden :: forall a. (ToADTArbitrary a, ToJSON a) => Proxy a -> IO ()
mkGolden Proxy = mkGoldenFileForType 10 (Proxy :: Proxy a) "test/interface/golden/golden/product"

mkGoldenFiles :: IO ()
mkGoldenFiles = do
  mkGolden (Proxy :: Proxy SimpleChoice)
  mkGolden (Proxy :: Proxy Person)
  mkGolden (Proxy :: Proxy Company)
  mkGolden (Proxy :: Proxy Suit)
  mkGolden (Proxy :: Proxy Card)
  mkGolden (Proxy :: Proxy Company2)
  mkGolden (Proxy :: Proxy (OneTypeParameter TypeParameterRef0))
  mkGolden (Proxy :: Proxy (TwoTypeParameters TypeParameterRef0 TypeParameterRef1))
  mkGolden (Proxy :: Proxy (Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2))
  mkGolden (Proxy :: Proxy (SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2))
  
compareInterfaceFiles = compareFiles "test/interface" "product" True

compareNoInterfaceFiles = compareFiles "test/nointerface" "product" False


spec :: Spec
spec = do
  runIO $ mkGoldenFiles
  
  let dir = "test/interface/temp"
  runIO $ mkPackage (Proxy :: Proxy ProductPackage) (PackageOptions dir "product" Map.empty True $ Just $ SpecOptions "__tests__/product" "golden/product" "http://localhost:8081")
  
  describe "OCaml Declaration with Interface: Product Types" $ do
    compareInterfaceFiles "Person"
    compareInterfaceFiles "Company"
    compareInterfaceFiles "Card"
    compareInterfaceFiles "OneTypeParameter"
    compareInterfaceFiles "TwoTypeParameters"
    compareInterfaceFiles "ThreeTypeParameters"
    compareInterfaceFiles "SubTypeParameter"

  let dir2 = "test/nointerface/temp"
  runIO $ mkPackage (Proxy :: Proxy ProductPackage) (PackageOptions dir2 "product" Map.empty False Nothing)

  describe "OCaml Declaration without Interface: Product Types" $ do
    compareNoInterfaceFiles "Person"
    compareNoInterfaceFiles "Company"
    compareNoInterfaceFiles "Card"
    compareNoInterfaceFiles "OneTypeParameter"
    compareNoInterfaceFiles "TwoTypeParameters"
    compareNoInterfaceFiles "ThreeTypeParameters"
    compareNoInterfaceFiles "SubTypeParameter"

data SimpleChoice =
  SimpleChoice
    { choice :: Either String Int
    } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary SimpleChoice where
  arbitrary = SimpleChoice <$> arbitrary

instance ToADTArbitrary SimpleChoice

data Person = Person
  { id :: Int
  , name :: Maybe String
  , created :: UTCTime
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary Person

data Company = Company
  { address   :: String
  , employees :: [Person]
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Company where
  arbitrary = do
    k <- choose (1,3)
    v <- vector k
    Company <$> arbitrary <*> pure v

instance ToADTArbitrary Company

data Company2 = Company2
  { address2   :: String
  , boss :: Maybe Person
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Company2 where
  arbitrary = Company2 <$> arbitrary <*> arbitrary

instance ToADTArbitrary Company2

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary Suit where
  arbitrary = elements [Clubs, Diamonds, Hearts, Spades]

instance ToADTArbitrary Suit

data Card =
  Card
    { cardSuit  :: Suit
    , cardValue :: Int
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary

instance ToADTArbitrary Card

data OneTypeParameter a =
  OneTypeParameter
    { otpId :: Int
    , otpFirst :: a
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary (OneTypeParameter TypeParameterRef0) where
  arbitrary = OneTypeParameter <$> arbitrary <*> arbitrary

instance ToADTArbitrary (OneTypeParameter TypeParameterRef0)

data TwoTypeParameters a b =
  TwoTypeParameters
    { ttpId :: Int
    , ttpFirst :: a
    , ttpSecond :: b
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary (TwoTypeParameters TypeParameterRef0 TypeParameterRef1) where
  arbitrary = TwoTypeParameters <$> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary (TwoTypeParameters TypeParameterRef0 TypeParameterRef1)

data Three a b c =
  Three
    { threeId :: Int
    , threeFirst :: a
    , threeSecond :: b
    , threeThird :: c
    , threeString :: String
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary (Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary (Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2)

data SubTypeParameter a b c =
  SubTypeParameter
    { listA :: [a]
    , maybeB :: Maybe b
    , tupleC :: (c,b)
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary (SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2) where
  arbitrary = do
    k <- choose (1,3)
    v <- vector k
    SubTypeParameter <$> pure v <*> arbitrary <*> arbitrary

instance ToADTArbitrary (SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2)
