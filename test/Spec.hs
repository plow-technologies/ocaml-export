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

main :: IO ()
main = hspec spec
