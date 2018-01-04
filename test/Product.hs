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

-- base
import Data.Monoid ((<>))
import GHC.Generics
-- text
import Data.Text (Text)
-- time
import Data.Time
import Data.Time.Clock.POSIX
-- containers
import qualified Data.Map as Map

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- hspec
import Test.Hspec
-- QuickCheck
import Test.QuickCheck
-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT
-- hspec-golden-aeson
import Test.Aeson.Internal.ADT.GoldenSpecs
-- ocaml-export
import OCaml.Export hiding (mkGoldenFiles)
import Util

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
  :<|> OCamlModule '["SubTypeParameter"] :> SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2
  :<|> OCamlModule '["UnnamedProduct"] :> UnnamedProduct
  :<|> OCamlModule '["ComplexProduct"] :> OCamlTypeInFile Simple "test/ocaml/Simple" :> ComplexProduct)

compareInterfaceFiles = compareFiles "test/interface" "product" True

compareNoInterfaceFiles = compareFiles "test/nointerface" "product" False

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

data UnnamedProduct = UnnamedProduct String Int
  deriving (Eq, Read, Show, Generic, OCamlType, FromJSON, ToJSON)
  
instance Arbitrary UnnamedProduct where
  arbitrary = UnnamedProduct <$> arbitrary <*> arbitrary

instance ToADTArbitrary UnnamedProduct

data Simple =
  Simple
    { sa :: Int
    , sb :: String
    } deriving (Eq,Show,Generic,FromJSON,ToJSON)

instance OCamlType Simple where
  toOCamlType _ = typeableToOCamlType (Proxy :: Proxy Simple)

instance ToADTArbitrary Simple
instance Arbitrary Simple where
  arbitrary = Simple <$> arbitrary <*> arbitrary

data ComplexProduct =
  ComplexProduct
    { cp0 :: Either Person [Int]
    , cp1 :: [(Int, Either String Double)]
    , cp2 :: [[Int]]
    , cp3 :: Maybe [Int]
    , cp4 :: Either Simple Int
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary ComplexProduct where
  arbitrary = do
    k0 <- choose (1,3)
    v0 <- vector k0

    k1 <- choose (1,3)
    v1 <- vector k1
    
    ComplexProduct <$> arbitrary <*> pure v0 <*> pure v1 <*> arbitrary <*> arbitrary

instance ToADTArbitrary ComplexProduct
