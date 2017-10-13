{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Proxy
import GHC.Generics
import OCaml.Export

data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic, OCamlType)

data Company = Company
  { address   :: String
  , employees :: [Person]
  } deriving (Show, Eq, Generic, OCamlType)

data RecWithTuple2 = RecWithTuple2
  { point :: (Int,Int)
  , graph :: Bool
  } deriving (Show, Eq, Generic, OCamlType)

data RecWithTuples = RecWithTuples
  { pointIn3d :: (Float,Float,Float)
  , details   :: (Bool,String,Int,String,Company)
  } deriving (Show, Eq, Generic, OCamlType)

data SumWithTuple = SumWithTuple (Int,Float,Int) String
  deriving (Show, Eq, Generic, OCamlType)

data User = User String String
  deriving (Show, Eq, Generic, OCamlType)  

data Item
  = A Int String
  | B
  | C Double
  deriving (Show, Eq, Generic, OCamlType)

data Triple
  = Triple1
  | Triple2
  | Triple3
  deriving (Show, Eq, Generic, OCamlType)

data Five
  = Five1 Int
  | Five2 Int
  | Five3 Int
  | Five4 Int
  | Five5 Int
  deriving (Show, Eq, Generic, OCamlType)


data EndsInSingle
  = AA Int
  | BB String
  deriving (Show, Eq, Generic, OCamlType)

data EndsInDouble
  = CC Int
  | DD String String
  deriving (Show, Eq, Generic, OCamlType)

{-
data Shape
  = Circle Float Float Float
  | Rectangle Float Float Float Float   
  deriving (Show, Eq, Generic, OCamlType)
-}
data SingletonWithSingle = SingletonWithSingle Double
  deriving (Show, Eq, Generic, OCamlType)

data Point =
  Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Generic, OCamlType)

data Rectangle =
  Rectangle
    { leftPoint :: Point
    , rightPoinit :: Point
    } deriving (Show, Eq, Generic, OCamlType)


spec :: Spec
spec =
  Spec
    ["Types"]
    [ toOCamlTypeSource (Proxy :: Proxy Person)
    , toOCamlEncoderSource (Proxy :: Proxy Person)
    , toOCamlTypeSource (Proxy :: Proxy Company)
    , toOCamlEncoderSource (Proxy :: Proxy Company)
    ]

main :: IO ()
main = do
  print $ toOCamlTypeSource (Proxy :: Proxy Person)
  print $ toOCamlTypeSource (Proxy :: Proxy Company)
  print $ toOCamlTypeSource (Proxy :: Proxy RecWithTuple2)
  print $ toOCamlTypeSource (Proxy :: Proxy RecWithTuples)
  print $ toOCamlTypeSource (Proxy :: Proxy SumWithTuple)
  print $ toOCamlTypeSource (Proxy :: Proxy User)
  print $ toOCamlTypeSource (Proxy :: Proxy Item)
  print $ toOCamlTypeSource (Proxy :: Proxy Triple)
  print $ toOCamlTypeSource (Proxy :: Proxy Five)
  print $ toOCamlTypeSource (Proxy :: Proxy EndsInSingle)
  print $ toOCamlTypeSource (Proxy :: Proxy EndsInDouble)
--  print $ toOCamlTypeSource (Proxy :: Proxy Shape)
  print $ toOCamlTypeSource (Proxy :: Proxy SingletonWithSingle)

  print $ toOCamlEncoderSource (Proxy :: Proxy Person)
  print $ toOCamlEncoderSource (Proxy :: Proxy RecWithTuple2)

  print $ toOCamlEncoderSource (Proxy :: Proxy Rectangle)

  specsToDir [spec] "./"
  return ()

{-
type person = {
  age: int,
  name: string
};

type person = {
  id: int,
  name: option (string)
};


let encodeJson.Decode.{
      start:     json |> field "start" point,
      end_:      json |> field "end" point,
      thickness: json |> optional (field "thickness" int)
    };

let encodePerson (x : person) :json => {
  Json.Encode.(
    object_
      [ ("id", Json.Encode.int x)
      , ("name",Json.Encode.string x)
      ]
  );
};


let encodePerson (x : person) => 
  Json.Encode.object_
    [ ("id", Json.Encode.int x.id)
    , ("name", (fun xx => Option.default Json.Encode.null (Option.map Json.Encode.string xx)) x.name)
    ];

:set -XDeriveGeneric
import GHC.Generics
import Data.Aeson
data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic)
instance ToJSON Person
instance FromJSON Person
-}
