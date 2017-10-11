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

spec :: Spec
spec =
  describe "toReasonTypeSource" $
    it "" $ do
      d <- T.readFile "test/golden/Record.ml"
      d `shouldBe` (OC.toReasonTypeSource (Proxy :: Proxy Person) <> "\n\n" <> OC.toReasonEncoderSource (Proxy :: Proxy Person) <> "\n")
      

main :: IO ()
main = hspec spec
