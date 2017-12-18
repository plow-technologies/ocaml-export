{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Dependency where

-- base
import Data.Monoid ((<>))
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- containers
import Data.Map as Map

-- hspec
import Test.Hspec

-- hspec-aeson-golden
import Test.Aeson.Internal.ADT.GoldenSpecs

-- QuickCheck
import Test.QuickCheck

-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT

-- text
import Data.Text (Text)

-- time
import Data.Time
import Data.Time.Clock.POSIX

-- ocaml-export
import OCaml.Export
import Product (ProductPackage, Person)
import Util

--mkGolden :: forall a. (ToADTArbitrary a, ToJSON a) => Proxy a -> IO ()
--mkGolden Proxy = mkGoldenFileForType 10 (Proxy :: Proxy a) "test/interface/golden/golden/dependency"

--mkGoldenFiles :: IO ()
--mkGoldenFiles = do
--  mkGolden (Proxy :: Proxy Class)

type DependencyPackage
  =    OCamlPackage "dependency" '[ProductPackage] :> (
       OCamlModule '["Class"] :> Class)

type DependencyPackageWithoutProduct
  =    OCamlPackage "dependency" NoDependency :> (
       OCamlModule '["Class"] :> Class)

type SchoolPackage
  =    OCamlPackage "school" '[DependencyPackage] :> (
       OCamlModule '["School"] :> School)
{-
data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary

instance ToADTArbitrary Person
-}
data Class = Class
  { subject   :: String
  , students  :: [Person] -- from another package
  , professor :: Person
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Class where
  arbitrary = do
    k <- choose (1,3)
    v <- vector k
    Class <$> arbitrary <*> pure v <*> arbitrary

instance ToADTArbitrary Class

data School = School
  { schoolName :: String
  , classes :: [Class]
  , administrator :: Person
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary School where
  arbitrary = do
    k <- choose (1,3)
    v <- vector k
    School <$> arbitrary <*> pure v <*> arbitrary

instance ToADTArbitrary School

data A =
  A Int
  deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance ToADTArbitrary A
instance Arbitrary A where
  arbitrary = A <$> arbitrary

data B =
  B String Int
  deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance ToADTArbitrary B
instance Arbitrary B where
  arbitrary = B <$> arbitrary <*> arbitrary

data C =
  C (Int,Int)
  deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance ToADTArbitrary C
instance Arbitrary C where
  arbitrary = C <$> arbitrary

data D =
  D String C
  deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance ToADTArbitrary D
instance Arbitrary D where
  arbitrary = D <$> arbitrary <*> arbitrary

data E =
  E Double
  deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance ToADTArbitrary E
instance Arbitrary E where
  arbitrary = E <$> arbitrary

type SubsPackage = OCamlPackage "subs" NoDependency :>
  (OCamlModule '["AtoE"] :> A :> (OCamlSubModule "One" :> B :> (OCamlSubModule "Two" :> C) :> D) :> E)
--   (OCamlModule '["AtoE"] :> OCamlSubModule '["One"] :> A)

spec :: Spec
spec = do
  runIO $ mkGoldenFiles (Proxy :: Proxy DependencyPackage) 10 "test/interface/golden/golden/dependency"
  runIO $ mkGoldenFiles (Proxy :: Proxy SubsPackage) 10 "test/interface/golden/golden/subs"
  runIO $ mkGoldenFiles (Proxy :: Proxy SchoolPackage) 10 "test/interface/golden/golden/school"
  let dir = "test/interface/temp"
  runIO $ mkPackage (Proxy :: Proxy DependencyPackage) (PackageOptions dir "dependency" Map.empty True Nothing)
  runIO $ mkPackage (Proxy :: Proxy SubsPackage) (PackageOptions dir "subs" Map.empty True Nothing)
  runIO $ mkPackage (Proxy :: Proxy SchoolPackage) (PackageOptions dir "school" Map.empty True Nothing)

  return ()
