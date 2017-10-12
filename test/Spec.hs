{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
import           Data.Proxy
import qualified Data.Text.IO as T
import           GHC.Generics
import qualified OCaml.Export as OC
import           Test.Hspec

{-
Haskell Algebraic Data Types

data : product/record, sum
newtype : compiler optimized type wrapper
type : type synonym

single sum no parameter : top level string
multiple sum, no parameter : top level string
single sum, one or more parameters : top level array
one or more sums, one or more parameters: tag string, contents top level or array

when the sum type has no parameters, it is always top level
data OnOrOff = On | Off
when there is at least one parameter it uses the tag system
data OnOrOff = On Int | Off
-}

data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic, OC.ReasonType)

data Company = Company
  { address   :: String
  , employees :: [Person]
  } deriving (Show, Eq, Generic, OC.ReasonType)

data OnOrOff = On | Off
  deriving (Show,Eq,Generic, OC.ReasonType)

data NameOrIdNumber = Name String | IdNumber Int
  deriving (Show,Eq,Generic, OC.ReasonType)

data SumVariant
  = HasNothing
  | HasSingleInt Int
  | HasSingleTuple (Int,Int)
  | HasMultipleInts Int Int
  | HasMultipleTuples (Int,Int) (Int,Int)
  deriving (Show,Eq,Generic, OC.ReasonType)  
  

personSpec :: OC.Spec
personSpec =
  OC.Spec
    ["Person"]
    [ OC.toReasonTypeSource (Proxy :: Proxy Person)
    , OC.toReasonEncoderSource (Proxy :: Proxy Person)
    ]

companySpec :: OC.Spec
companySpec =
  OC.Spec
    ["Company"]
    [ OC.toReasonTypeSource (Proxy :: Proxy Person)
    , OC.toReasonEncoderSource (Proxy :: Proxy Person)
    , OC.toReasonTypeSource (Proxy :: Proxy Company)
    , OC.toReasonEncoderSource (Proxy :: Proxy Company)
    ]

onOrOffSpec :: OC.Spec
onOrOffSpec =
  OC.Spec
    ["OnOrOff"]
    [ OC.toReasonTypeSource (Proxy :: Proxy OnOrOff)
    , OC.toReasonEncoderSource (Proxy :: Proxy OnOrOff)
    ]    

nameOrIdNumberSpec :: OC.Spec
nameOrIdNumberSpec =
  OC.Spec
    ["NameOrIdNumber"]
    [ OC.toReasonTypeSource (Proxy :: Proxy NameOrIdNumber)
    , OC.toReasonEncoderSource (Proxy :: Proxy NameOrIdNumber)
    ]

sumVariantSpec :: OC.Spec
sumVariantSpec =
  OC.Spec
    ["SumVariant"]
    [ OC.toReasonTypeSource (Proxy :: Proxy SumVariant)
    , OC.toReasonEncoderSource (Proxy :: Proxy SumVariant)
    ]

spec :: Spec
spec =
  describe "toReasonTypeSource" $ do
    it "" $ do
      OC.specsToDir [personSpec] "./test/temp/product"
      handWritten <- T.readFile "test/golden/product/Person.ml"
      automated   <- T.readFile "test/temp/product/Person.ml"
      automated `shouldBe` handWritten
    it "" $ do
      OC.specsToDir [companySpec] "./test/temp/product"
      handWritten <- T.readFile "test/golden/product/Company.ml"
      automated   <- T.readFile "test/temp/product/Company.ml"
      automated `shouldBe` handWritten
    it "" $ do
      OC.specsToDir [onOrOffSpec] "./test/temp/sum"
      handWritten <- T.readFile "test/golden/sum/OnOrOff.ml"
      automated   <- T.readFile "test/temp/sum/OnOrOff.ml"
      automated `shouldBe` handWritten      
    it "" $ do
      OC.specsToDir [nameOrIdNumberSpec] "./test/temp/sum"
      handWritten <- T.readFile "test/golden/sum/NameOrIdNumber.ml"
      automated   <- T.readFile "test/temp/sum/NameOrIdNumber.ml"
      automated `shouldBe` handWritten      
    it "" $ do
      OC.specsToDir [sumVariantSpec] "./test/temp/sum"
      handWritten <- T.readFile "test/golden/sum/SumVariant.ml"
      automated   <- T.readFile "test/temp/sum/SumVariant.ml"
      automated `shouldBe` handWritten      


main :: IO ()
main = hspec spec
