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
  } deriving (Show, Eq, Generic, ReasonType)

data Company = Company
  { address   :: String
  , employees :: [Person]
  } deriving (Show, Eq, Generic, ReasonType)

data RecWithTuple2 = RecWithTuple2
  { point :: (Int,Int)
  , graph :: Bool
  } deriving (Show, Eq, Generic, ReasonType)

data RecWithTuples = RecWithTuples
  { pointIn3d :: (Float,Float,Float)
  , details   :: (Bool,String,Int,String,Company)
  } deriving (Show, Eq, Generic, ReasonType)

data SumWithTuple = SumWithTuple (Int,Float,Int) String
  deriving (Show, Eq, Generic, ReasonType)

data User = User String String
  deriving (Show, Eq, Generic, ReasonType)  

data Item
  = A Int String
  | B
  | C Double
  deriving (Show, Eq, Generic, ReasonType)

data Triple
  = Triple1
  | Triple2
  | Triple3
  deriving (Show, Eq, Generic, ReasonType)

data Five
  = Five1 Int
  | Five2 Int
  | Five3 Int
  | Five4 Int
  | Five5 Int
  deriving (Show, Eq, Generic, ReasonType)


data EndsInSingle
  = AA Int
  | BB String
  deriving (Show, Eq, Generic, ReasonType)

data EndsInDouble
  = CC Int
  | DD String String
  deriving (Show, Eq, Generic, ReasonType)

{-
data Shape
  = Circle Float Float Float
  | Rectangle Float Float Float Float   
  deriving (Show, Eq, Generic, ReasonType)
-}
data SingletonWithSingle = SingletonWithSingle Double
  deriving (Show, Eq, Generic, ReasonType)

data Point =
  Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Generic, ReasonType)

data Rectangle =
  Rectangle
    { leftPoint :: Point
    , rightPoinit :: Point
    } deriving (Show, Eq, Generic, ReasonType)


spec :: Spec
spec =
  Spec
    ["Types"]
    [ toReasonTypeSource (Proxy :: Proxy Person)
    , toReasonEncoderSource (Proxy :: Proxy Person)
    , toReasonTypeSource (Proxy :: Proxy Company)
    , toReasonEncoderSource (Proxy :: Proxy Company)
    ]

main :: IO ()
main = do
  print $ toReasonTypeSource (Proxy :: Proxy Person)
  print $ toReasonTypeSource (Proxy :: Proxy Company)
  print $ toReasonTypeSource (Proxy :: Proxy RecWithTuple2)
  print $ toReasonTypeSource (Proxy :: Proxy RecWithTuples)
  print $ toReasonTypeSource (Proxy :: Proxy SumWithTuple)
  print $ toReasonTypeSource (Proxy :: Proxy User)
  print $ toReasonTypeSource (Proxy :: Proxy Item)
  print $ toReasonTypeSource (Proxy :: Proxy Triple)
  print $ toReasonTypeSource (Proxy :: Proxy Five)
  print $ toReasonTypeSource (Proxy :: Proxy EndsInSingle)
  print $ toReasonTypeSource (Proxy :: Proxy EndsInDouble)
--  print $ toReasonTypeSource (Proxy :: Proxy Shape)
  print $ toReasonTypeSource (Proxy :: Proxy SingletonWithSingle)

  print $ toReasonEncoderSource (Proxy :: Proxy Person)
  print $ toReasonEncoderSource (Proxy :: Proxy RecWithTuple2)

  print $ toReasonEncoderSource (Proxy :: Proxy Rectangle)

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
