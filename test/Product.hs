{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- for OCaml Module
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


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

type ProductPackage
  =    OCamlModule '["Person"] '[] :> Person
  :<|> OCamlModule '["Company"] '[] :> Person :> Company
  :<|> OCamlModule '["Card"] '[] :> Suit :> Card
  :<|> OCamlModule '["CustomOption"] '[] :> Person :> Company2
  :<|> OCamlModule '["OneTypeParameter"] '[] :> OneTypeParameter TypeParameterRef0
  :<|> OCamlModule '["TwoTypeParameters"] '[] :> TwoTypeParameters TypeParameterRef0 TypeParameterRef1
  :<|> OCamlModule '["ThreeTypeParameters"] '[] :> Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2
  :<|> OCamlModule '["SubTypeParameter"] '[] :> SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2

mkGoldenFiles :: IO ()
mkGoldenFiles = do
  mkGoldenFileForType 2 (Proxy :: Proxy Person) "test/interface/golden/__tests__/golden/product"
  mkGoldenFileForType 2 (Proxy :: Proxy Company) "test/interface/golden/__tests__/golden/product"
  mkGoldenFileForType 2 (Proxy :: Proxy Suit) "test/interface/golden/__tests__/golden/product"
  mkGoldenFileForType 2 (Proxy :: Proxy Card) "test/interface/golden/__tests__/golden/product"

compareInterfaceFiles = compareFiles "test/interface" "product"

compareNoInterfaceFiles = compareFiles "test/nointerface" "product"


spec :: Spec
spec = do
  let dir = "test/interface/temp"
  runIO $ mkPackage (Proxy :: Proxy Product.ProductPackage) (PackageOptions dir "product" True $ Just $ SpecOptions "__tests__" "test/golden_files" "localhost:8081")
  
  describe "OCaml Declaration with Interface: Product Types" $ do
    compareInterfaceFiles "Person"
    compareInterfaceFiles "Company"
    compareInterfaceFiles "Card"
    compareInterfaceFiles "OneTypeParameter"
    compareInterfaceFiles "TwoTypeParameters"
    compareInterfaceFiles "ThreeTypeParameters"
    compareInterfaceFiles "SubTypeParameter"

  let dir2 = "test/nointerface/temp"
  runIO $ mkPackage (Proxy :: Proxy Product.ProductPackage) (PackageOptions dir2 "product" False Nothing)

  describe "OCaml Declaration with Interface: Product Types" $ do
    compareNoInterfaceFiles "Person"
    compareNoInterfaceFiles "Company"
    compareNoInterfaceFiles "Card"
    compareNoInterfaceFiles "OneTypeParameter"
    compareNoInterfaceFiles "TwoTypeParameters"
    compareNoInterfaceFiles "ThreeTypeParameters"
    compareNoInterfaceFiles "SubTypeParameter"


data Person = Person
  { id :: Int
  , name :: Maybe String
  , created :: UTCTime
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)


--instance Arbitrary UTCTime where
--  arbitrary = posixSecondsToUTCTime . fromIntegral <$> (arbitrary :: Gen Integer)
instance Arbitrary UTCTime where
  arbitrary =
    UTCTime <$> (ModifiedJulianDay <$> (2000 +) <$> arbitrary)
            <*> (fromRational . toRational <$> choose (0:: Double, 86400))


instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary Person

data Company = Company
  { address   :: String
  , employees :: [Person]
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Company where
  arbitrary = do
    --k <- choose (0,1)
    --v <- vector k
    Company <$> arbitrary <*> pure []

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

data TwoTypeParameters a b =
  TwoTypeParameters
    { ttpId :: Int
    , ttpFirst :: a
    , ttpSecond :: b
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

data Three a b c =
  Three
    { threeId :: Int
    , threeFirst :: a
    , threeSecond :: b
    , threeThird :: c
    , threeString :: String
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

data SubTypeParameter a b c =
  SubTypeParameter
    { listA :: [a]
    , maybeB :: Maybe b
    , tupleC :: (c,b)
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)
