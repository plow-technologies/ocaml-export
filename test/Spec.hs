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

personSpec :: OC.Spec
personSpec =
  OC.Spec
    ["Person"]
    [ OC.toReasonTypeSource (Proxy :: Proxy Person)
    , OC.toReasonEncoderSource (Proxy :: Proxy Person)
    ]


spec :: Spec
spec =
  describe "toReasonTypeSource" $
    it "" $ do
      OC.specsToDir [personSpec] "./test/temp/product"
      handWritten <- T.readFile "test/golden/product/Person.ml"
      automated   <- T.readFile "test/temp/product/Person.ml"
      automated `shouldBe` handWritten
      -- d `shouldBe` (OC.toReasonTypeSource (Proxy :: Proxy Person) <> "\n\n" <> OC.toReasonEncoderSource (Proxy :: Proxy Person) <> "\n")
      

main :: IO ()
main = hspec spec
