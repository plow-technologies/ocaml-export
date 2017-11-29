{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Sum
  ( spec
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import GHC.Generics
import OCaml.Export
import Test.Hspec
import Util

type SumPackage
  =    OCamlModule '["OnOrOff"] '[] :> OnOrOff
  :<|> OCamlModule '["NameOrIdNumber"] '[] :> NameOrIdNumber
  :<|> OCamlModule '["SumVariant"] '[] :> SumVariant
  :<|> OCamlModule '["WithTuple"] '[] :> WithTuple
  :<|> OCamlModule '["SumWithRecord"] '[] :> SumWithRecord
  :<|> OCamlModule '["Result"] '[] :> Result TypeParameterRef0 TypeParameterRef1
  :<|> OCamlModule '["NewType"] '[] :> NewType

compareInterfaceFiles = compareFiles "test/interface" "sum"

compareNoInterfaceFiles = compareFiles "test/nointerface" "sum"

spec :: Spec
spec = do
  let dir = "test/interface/temp"
  runIO $ mkPackage (Proxy :: Proxy SumPackage) (PackageOptions dir "sum" True $ Just $ SpecOptions "__tests__" "test/golden_files" "localhost:8081")

  
  describe "OCaml Declaration with Interface: Sum Types" $ do
    compareInterfaceFiles "OnOrOff"
    compareInterfaceFiles "NameOrIdNumber"
    compareInterfaceFiles "SumVariant"
    compareInterfaceFiles "WithTuple"
    compareInterfaceFiles "SumWithRecord"
    compareInterfaceFiles "Result"
    compareInterfaceFiles "NewType"

  let dir2 = "test/nointerface/temp"
  runIO $ mkPackage (Proxy :: Proxy SumPackage) (PackageOptions dir2 "sum" False Nothing)

  describe "Sum Types" $ do
    compareNoInterfaceFiles "OnOrOff"
    compareNoInterfaceFiles "NameOrIdNumber"
    compareNoInterfaceFiles "SumVariant"
    compareNoInterfaceFiles "WithTuple"
    compareNoInterfaceFiles "SumWithRecord"
    compareNoInterfaceFiles "Result"
    compareNoInterfaceFiles "NewType"
    
data OnOrOff = On | Off
  deriving (Show,Eq,Generic,OCamlType,ToJSON,FromJSON)

data NameOrIdNumber = Name String | IdNumber Int
  deriving (Show, Eq, Generic, OCamlType,ToJSON,FromJSON)

data Result a b
  = Success a
  | Error b
  deriving (Show, Eq, Generic, OCamlType, ToJSON, FromJSON)

data SumVariant
  = HasNothing
  | HasSingleInt Int
  | HasSingleTuple (Int,Int)
  | HasMultipleInts Int Int
  | HasMultipleTuples (Int,Int) (Int,Int)
  | HasMixed Int String Double
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

type Tuple
  = (Int,Int)

data WithTuple = WithTuple Tuple
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

data SumWithRecord
  = A1 {a1 :: Int}
  | B2 {b2 :: String, b3 :: Int}
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

newtype NewType
  = NewType Int
  deriving (Show,Eq,Generic,OCamlType, ToJSON, FromJSON)

onOrOff :: OCamlFile
onOrOff =
  OCamlFile
    "OnOrOff"
    [ toOCamlTypeSource (Proxy :: Proxy OnOrOff)
    , toOCamlEncoderSource (Proxy :: Proxy OnOrOff)
    , toOCamlDecoderSource (Proxy :: Proxy OnOrOff)
    ]    

nameOrIdNumber :: OCamlFile
nameOrIdNumber =
  OCamlFile
    "NameOrIdNumber"
    [ toOCamlTypeSource (Proxy :: Proxy NameOrIdNumber)
    , toOCamlEncoderSource (Proxy :: Proxy NameOrIdNumber)
    , toOCamlDecoderSource (Proxy :: Proxy NameOrIdNumber)
    ]

sumVariant :: OCamlFile
sumVariant =
  OCamlFile
    "SumVariant"
    [ toOCamlTypeSource (Proxy :: Proxy SumVariant)
    , toOCamlEncoderSource (Proxy :: Proxy SumVariant)
    , toOCamlDecoderSource (Proxy :: Proxy SumVariant)
    ]

tuple :: OCamlFile
tuple =
  OCamlFile
    "Tuple"
    [ toOCamlTypeSource (Proxy :: Proxy Tuple)
    , toOCamlEncoderSource (Proxy :: Proxy Tuple)
    , toOCamlDecoderSource (Proxy :: Proxy Tuple)
    ]

withTuple :: OCamlFile
withTuple =
  OCamlFile
    "WithTuple"
    [ toOCamlTypeSource (Proxy :: Proxy WithTuple)
    , toOCamlEncoderSource (Proxy :: Proxy WithTuple)
    , toOCamlDecoderSource (Proxy :: Proxy WithTuple)
    ]

sumWithRecord :: OCamlFile
sumWithRecord =
  OCamlFile
    "SumWithRecord"
    [ toOCamlTypeSource (Proxy :: Proxy SumWithRecord)
    , toOCamlEncoderSource (Proxy :: Proxy SumWithRecord)
    , toOCamlDecoderSource (Proxy :: Proxy SumWithRecord)
    ]

resultRecord :: OCamlFile
resultRecord =
  OCamlFile
    "Result"
    [ toOCamlTypeSource (Proxy :: Proxy (Result TypeParameterRef0 TypeParameterRef1))
    , toOCamlEncoderSource (Proxy :: Proxy (Result TypeParameterRef0 TypeParameterRef1))
    , toOCamlDecoderSource (Proxy :: Proxy (Result TypeParameterRef0 TypeParameterRef1))
    ]

newTypeRecord :: OCamlFile
newTypeRecord =
  OCamlFile
    "NewType"
    [ toOCamlTypeSource (Proxy :: Proxy NewType)
    , toOCamlEncoderSource (Proxy :: Proxy NewType)
    , toOCamlDecoderSource (Proxy :: Proxy NewType)
    ]
