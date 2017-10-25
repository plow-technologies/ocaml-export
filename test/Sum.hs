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

spec :: Spec
spec = do
  describe "Sum Types" $ do
    testSum onOrOff "OnOrOff"
    testSum nameOrIdNumber "NameOrIdNumber"
    testSum sumVariant "SumVariant"
    testSum withTuple "WithTuple"
    testSum sumWithRecord "SumWithRecord"
    
data OnOrOff = On | Off
  deriving (Show,Eq,Generic, OCamlType)

data NameOrIdNumber = Name String | IdNumber Int
  deriving (Show,Eq,Generic, OCamlType)

data SumVariant
  = HasNothing
  | HasSingleInt Int
  | HasSingleTuple (Int,Int)
  | HasMultipleInts Int Int
  | HasMultipleTuples (Int,Int) (Int,Int)
  deriving (Show,Eq,Generic, OCamlType)  

type Tuple
  = (Int,Int)

data WithTuple = WithTuple Tuple
  deriving (Show,Eq,Generic, OCamlType)

data SumWithRecord
  = A1 {a1 :: Int}
  | B2 {b2 :: String, b3 :: Int}
  deriving (Show,Eq,Generic, OCamlType)

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
    ]

sumVariant :: OCamlFile
sumVariant =
  OCamlFile
    "SumVariant"
    [ toOCamlTypeSource (Proxy :: Proxy SumVariant)
    , toOCamlEncoderSource (Proxy :: Proxy SumVariant)
    ]

tuple :: OCamlFile
tuple =
  OCamlFile
    "Tuple"
    [ toOCamlTypeSource (Proxy :: Proxy Tuple)
    , toOCamlEncoderSource (Proxy :: Proxy Tuple)
    ]

withTuple :: OCamlFile
withTuple =
  OCamlFile
    "WithTuple"
    [ toOCamlTypeSource (Proxy :: Proxy WithTuple)
    , toOCamlEncoderSource (Proxy :: Proxy WithTuple)
    ]

sumWithRecord :: OCamlFile
sumWithRecord =
  OCamlFile
    "SumWithRecord"
    [ toOCamlTypeSource (Proxy :: Proxy SumWithRecord)
    , toOCamlEncoderSource (Proxy :: Proxy SumWithRecord)
    ]
