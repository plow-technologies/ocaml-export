{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
import           Data.Proxy
import qualified Data.Text.IO as T
import           GHC.Generics
import qualified OCaml.Export as OC
import           Test.Hspec

data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic, OC.ReasonType)

data Company = Company
  { address   :: String
  , employees :: [Person]
  } deriving (Show, Eq, Generic, OC.ReasonType)

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
      

main :: IO ()
main = hspec spec
