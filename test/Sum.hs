{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sum (spec) where

import Data.Proxy
import GHC.Generics
import OCaml.Export
import Test.Hspec
import Util

testSum = testOCamlType Sum
testSumInterface = testOCamlTypeWithInterface Sum

spec :: Spec
spec = do
  describe "OCaml Declaration with Interface: Sum Types" $ do
    testSumInterface "OnOrOff" (mkOCamlInterface (Proxy :: Proxy OnOrOff))
    testSumInterface "NameOrIdNumber" (mkOCamlInterface (Proxy :: Proxy NameOrIdNumber))
    testSumInterface "SumVariant" (mkOCamlInterface (Proxy :: Proxy SumVariant))
    testSumInterface "WithTuple" (mkOCamlInterface (Proxy :: Proxy WithTuple))
    testSumInterface "SumWithRecord" (mkOCamlInterface (Proxy :: Proxy SumWithRecord))
    testSumInterface "Result" (mkOCamlInterface (Proxy :: Proxy (Result TypeParameterRef0 TypeParameterRef1)))
    
  describe "Sum Types" $ do
    testSum onOrOff "OnOrOff"
    testSum nameOrIdNumber "NameOrIdNumber"
    testSum sumVariant "SumVariant"
    testSum withTuple "WithTuple"
    testSum sumWithRecord "SumWithRecord"
    testSum resultRecord "Result"
    testSum newTypeRecord "NewType"
    
data OnOrOff = On | Off
  deriving (Show,Eq,Generic, OCamlType)

data NameOrIdNumber = Name String | IdNumber Int
  deriving (Show, Eq, Generic, OCamlType)

data Result a b
  = Success a
  | Error b
  deriving (Show, Eq, Generic, OCamlType)

data SumVariant
  = HasNothing
  | HasSingleInt Int
  | HasSingleTuple (Int,Int)
  | HasMultipleInts Int Int
  | HasMultipleTuples (Int,Int) (Int,Int)
  | HasMixed Int String Double
  deriving (Show,Eq,Generic, OCamlType)  

type Tuple
  = (Int,Int)

data WithTuple = WithTuple Tuple
  deriving (Show,Eq,Generic, OCamlType)

data SumWithRecord
  = A1 {a1 :: Int}
  | B2 {b2 :: String, b3 :: Int}
  deriving (Show,Eq,Generic, OCamlType)

newtype NewType
  = NewType Int
  deriving (Show,Eq,Generic,OCamlType)

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
