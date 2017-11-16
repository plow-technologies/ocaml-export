{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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


testProduct = testOCamlType Product

testProductInterface = testOCamlTypeWithInterface Product

mkTestOCaml :: OCamlType a => Text -> a -> OCamlInterface
mkTestOCaml modul = mkOCamlInterfaceWithSpec "http://localhost:8081" "__tests__/golden/" modul

mkGoldenFiles :: IO ()
mkGoldenFiles = do
  mkGoldenFileForType 2 (Proxy :: Proxy Person) "test/interface/golden/__tests__/golden/product"
  mkGoldenFileForType 2 (Proxy :: Proxy Company) "test/interface/golden/__tests__/golden/product"
  mkGoldenFileForType 2 (Proxy :: Proxy Card) "test/interface/golden/__tests__/golden/product"


spec :: Spec
spec = do
  describe "OCaml Declaration with Interface: Product Types" $ do
    testProductInterface "Person" (mkTestOCaml "Person" (Proxy :: Proxy Person))
    testProductInterface "Company" (mkTestOCaml "Company" (Proxy :: Proxy Person) <> mkTestOCaml "Company" (Proxy :: Proxy Company))
    testProductInterface "Card" (mkTestOCaml "Card" (Proxy :: Proxy Suit) <> mkTestOCaml "Card" (Proxy :: Proxy Card))

    testProductInterface "CustomOption" (mkOCamlInterface (Proxy :: Proxy Person) <> (mkOCamlInterface (Proxy :: Proxy Company2)))
    testProductInterface "OneTypeParameter" (mkOCamlInterface (Proxy :: Proxy (OneTypeParameter TypeParameterRef0)))
    testProductInterface "TwoTypeParameters" (mkOCamlInterface (Proxy :: Proxy (TwoTypeParameters TypeParameterRef0 TypeParameterRef1)))
    testProductInterface "ThreeTypeParameters" (mkOCamlInterface (Proxy :: Proxy (Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2)))
    testProductInterface "SubTypeParameter" (mkOCamlInterface (Proxy :: Proxy (SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2)))

  describe "Product Types" $ do
    testProduct person "Person"
    testProduct company "Company"
    testProduct card "Card"
    testProduct oneTypeParameter "OneTypeParameter"
    testProduct twoTypeParameters "TwoTypeParameters"
    testProduct three "ThreeTypeParameters"
    testProduct subTypeParameter "SubTypeParameter"

{-
  describe "" $ it "" $ do
    print $ toOCamlSpec (Proxy :: Proxy Person) "http://localhost:8081/person" "../../golden/"
    shouldBe True False
-}
data Person = Person
  { id :: Int
  , name :: Maybe String
  , created :: UTCTime
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)


instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . fromIntegral <$> (arbitrary :: Gen Integer)

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
  } deriving (Show, Eq, Generic, OCamlType)

instance Arbitrary Company2 where
  arbitrary = Company2 <$> arbitrary <*> arbitrary

instance ToADTArbitrary Company2


data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq,Show,Generic,OCamlType,FromJSON, ToJSON)

instance Arbitrary Suit where
  arbitrary = elements [Clubs, Diamonds, Hearts, Spades]

instance ToADTArbitrary Suit

data Card =
  Card
    { cardSuit  :: Suit
    , cardValue :: Int
    } deriving (Eq,Show,Generic,OCamlType, FromJSON, ToJSON)

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary

instance ToADTArbitrary Card

data OneTypeParameter a =
  OneTypeParameter
    { otpId :: Int
    , otpFirst :: a
    } deriving (Eq,Show,Generic,OCamlType)

data TwoTypeParameters a b =
  TwoTypeParameters
    { ttpId :: Int
    , ttpFirst :: a
    , ttpSecond :: b
    } deriving (Eq,Show,Generic,OCamlType)

data Three a b c =
  Three
    { threeId :: Int
    , threeFirst :: a
    , threeSecond :: b
    , threeThird :: c
    , threeString :: String
    } deriving (Eq,Show,Generic,OCamlType)

data SubTypeParameter a b c =
  SubTypeParameter
    { listA :: [a]
    , maybeB :: Maybe b
    , tupleC :: (c,b)
    } deriving (Eq,Show,Generic,OCamlType)

person :: OCamlFile
person =
  OCamlFile
    "Person"
    [ toOCamlTypeSource (Proxy :: Proxy Person)
    , toOCamlEncoderSource (Proxy :: Proxy Person)
    , toOCamlDecoderSource (Proxy :: Proxy Person)
    ]

company :: OCamlFile
company =
  OCamlFile
    "Company"
    [ toOCamlTypeSource (Proxy :: Proxy Person)
    , toOCamlEncoderSource (Proxy :: Proxy Person)
    , toOCamlDecoderSource (Proxy :: Proxy Person)
    , toOCamlTypeSource (Proxy :: Proxy Company)
    , toOCamlEncoderSource (Proxy :: Proxy Company)
    , toOCamlDecoderSource (Proxy :: Proxy Company)
    ]

card :: OCamlFile
card =
  OCamlFile
    "Card"
    [ toOCamlTypeSource (Proxy :: Proxy Suit)
    , toOCamlEncoderSource (Proxy :: Proxy Suit)
    , toOCamlDecoderSource (Proxy :: Proxy Suit)
    , toOCamlTypeSource (Proxy :: Proxy Card)
    , toOCamlEncoderSource (Proxy :: Proxy Card)
    , toOCamlDecoderSource (Proxy :: Proxy Card)
    ]

oneTypeParameter :: OCamlFile
oneTypeParameter =
  OCamlFile
    "OneTypeParameter"
    [ toOCamlTypeSource (Proxy :: Proxy (OneTypeParameter TypeParameterRef0))
    , toOCamlEncoderSource (Proxy :: Proxy (OneTypeParameter TypeParameterRef0))
    , toOCamlDecoderSource (Proxy :: Proxy (OneTypeParameter TypeParameterRef0))
    ]

twoTypeParameters :: OCamlFile
twoTypeParameters =
  OCamlFile
    "TwoTypeParameters"
    [ toOCamlTypeSource (Proxy :: Proxy (TwoTypeParameters TypeParameterRef0 TypeParameterRef1))
    , toOCamlEncoderSource (Proxy :: Proxy (TwoTypeParameters TypeParameterRef0 TypeParameterRef1))
    , toOCamlDecoderSource (Proxy :: Proxy (TwoTypeParameters TypeParameterRef0 TypeParameterRef1))
    ]

three :: OCamlFile
three =
  OCamlFile
    "ThreeTypeParameters"
    [ toOCamlTypeSource (Proxy :: Proxy (Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2))
    , toOCamlEncoderSource (Proxy :: Proxy (Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2))
    , toOCamlDecoderSource (Proxy :: Proxy (Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2))
    ]

subTypeParameter :: OCamlFile
subTypeParameter =
  OCamlFile
    "SubTypeParameter"
    [ toOCamlTypeSource (Proxy :: Proxy (SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2))
    , toOCamlEncoderSource (Proxy :: Proxy (SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2))
    , toOCamlDecoderSource (Proxy :: Proxy (SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2))
    ]
