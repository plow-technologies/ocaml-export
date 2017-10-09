{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Proxy
import GHC.Generics
import Reason.Export

data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic, ReasonType)

data Company = Company
  { address   :: String
  , employees :: [Person]
  } deriving (Show, Eq, Generic, ReasonType)

data Item
  = A Int String
  | B
  | C Double
  deriving (Show, Eq, Generic, ReasonType)

main :: IO ()
main = do
  print $ toReasonTypeSource (Proxy :: Proxy Person)
  print $ toReasonTypeSource (Proxy :: Proxy Company)
  print $ toReasonTypeSource (Proxy :: Proxy Item)
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
-}
