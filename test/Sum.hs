{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sum where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import GHC.Generics
import OCaml.Export
import Test.Hspec
import Util
import qualified Data.Map as Map


-- QuickCheck
import Test.QuickCheck hiding (Result, Success)

-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT
import Test.Aeson.Internal.ADT.GoldenSpecs

import Servant
import Servant.API

type SumPackage
  = OCamlPackage "sum" NoDependency :>
       (OCamlModule '["OnOrOff"] '[] :> OnOrOff
  :<|> OCamlModule '["NameOrIdNumber"] '[] :> NameOrIdNumber
  :<|> OCamlModule '["SumVariant"] '[] :> SumVariant
  :<|> OCamlModule '["WithTuple"] '[] :> WithTuple
  :<|> OCamlModule '["SumWithRecord"] '[] :> SumWithRecord
  :<|> OCamlModule '["Result"] '[] :> Result TypeParameterRef0 TypeParameterRef1
  :<|> OCamlModule '["NewType"] '[] :> NewType)

-- server1 :: Server (MkOCamlSpecAPI SumPackage)
-- server1 = (pure :<|> (pure :<|> pure))

compareInterfaceFiles = compareFiles "test/interface" "sum" True

compareNoInterfaceFiles = compareFiles "test/nointerface" "sum" False

mkGolden :: forall a. (ToADTArbitrary a, ToJSON a) => Proxy a -> IO ()
mkGolden Proxy = mkGoldenFileForType 10 (Proxy :: Proxy a) "test/interface/golden/golden/sum"

mkGoldenFiles :: IO ()
mkGoldenFiles = do
  mkGolden (Proxy :: Proxy OnOrOff)
  mkGolden (Proxy :: Proxy NameOrIdNumber)
  mkGolden (Proxy :: Proxy SumVariant)
  mkGolden (Proxy :: Proxy WithTuple)
  mkGolden (Proxy :: Proxy SumWithRecord)
  mkGolden (Proxy :: Proxy (Result TypeParameterRef0 TypeParameterRef1))
  mkGolden (Proxy :: Proxy NewType) 

spec :: Spec
spec = do
  runIO mkGoldenFiles
  let dir = "test/interface/temp"
  runIO $ mkPackage (Proxy :: Proxy SumPackage) (PackageOptions dir "sum" Map.empty True $ Just $ SpecOptions "__tests__/sum" "golden/sum" "http://localhost:8082")

  describe "OCaml Declaration with Interface: Sum Types" $ do
    compareInterfaceFiles "OnOrOff"
    compareInterfaceFiles "NameOrIdNumber"
    compareInterfaceFiles "SumVariant"
    compareInterfaceFiles "WithTuple"
    compareInterfaceFiles "SumWithRecord"
    compareInterfaceFiles "Result"
    compareInterfaceFiles "NewType"
{-
  let dir2 = "test/nointerface/temp"
  runIO $ mkPackage (Proxy :: Proxy SumPackage) (PackageOptions dir2 "sum" Map.empty False Nothing)

  describe "Sum Types" $ do
    compareNoInterfaceFiles "OnOrOff"
    compareNoInterfaceFiles "NameOrIdNumber"
    compareNoInterfaceFiles "SumVariant"
    compareNoInterfaceFiles "WithTuple"
    compareNoInterfaceFiles "SumWithRecord"
    compareNoInterfaceFiles "Result"
    compareNoInterfaceFiles "NewType"
-}
    
data OnOrOff = On | Off
  deriving (Show,Eq,Generic,OCamlType,ToJSON,FromJSON)

instance Arbitrary OnOrOff where
  arbitrary = elements [On, Off]

instance ToADTArbitrary OnOrOff

data NameOrIdNumber = Name String | IdNumber Int
  deriving (Show, Eq, Generic, OCamlType,ToJSON,FromJSON)

instance Arbitrary NameOrIdNumber where
  arbitrary = oneof [Name <$> arbitrary, IdNumber <$> arbitrary]

instance ToADTArbitrary NameOrIdNumber

data Result a b
  = Success a
  | Error b
  deriving (Show, Eq, Generic, OCamlType, ToJSON, FromJSON)

instance Arbitrary (Result TypeParameterRef0 TypeParameterRef1) where
  arbitrary = oneof [Success <$> arbitrary, Error <$> arbitrary]

instance ToADTArbitrary (Result TypeParameterRef0 TypeParameterRef1)

data SumVariant
  = HasNothing
  | HasSingleInt Int
  | HasSingleTuple (Int,Int)
  | HasMultipleInts Int Int
  | HasMultipleTuples (Int,Int) (Int,Int)
  | HasMixed Int String Double
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

instance Arbitrary SumVariant where
  arbitrary =
    oneof
      [ pure HasNothing
      , HasSingleInt <$> arbitrary
      , HasSingleTuple <$> arbitrary
      , HasMultipleInts <$> arbitrary <*> arbitrary
      , HasMultipleTuples <$> arbitrary <*> arbitrary
      , HasMixed <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance ToADTArbitrary SumVariant


type Tuple
  = (Int,Int)

data WithTuple = WithTuple Tuple
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

instance Arbitrary WithTuple where
  arbitrary = WithTuple <$> arbitrary

instance ToADTArbitrary WithTuple

data SumWithRecord
  = A1 {a1 :: Int}
  | B2 {b2 :: String, b3 :: Int}
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

instance Arbitrary SumWithRecord where
  arbitrary =
    oneof
      [ A1 <$> arbitrary
      , B2 <$> arbitrary <*> arbitrary
      ]

instance ToADTArbitrary SumWithRecord

newtype NewType
  = NewType Int
  deriving (Show,Eq,Generic,OCamlType, ToJSON, FromJSON)

instance Arbitrary NewType where
  arbitrary = NewType <$> arbitrary

instance ToADTArbitrary NewType
