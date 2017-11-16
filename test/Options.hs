{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Options where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types hiding (Options)
import Data.Char (toUpper)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics
import OCaml.Export hiding (Options,defaultOptions)
import qualified OCaml.Export as OCaml (Options,defaultOptions)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Test.Aeson.Internal.ADT.GoldenSpecs
import Util


testOptions = testOCamlType Options

testOptionsInterface = testOCamlTypeWithInterface Options

mkTestOCaml :: OCamlType a => Text -> a -> OCamlInterface
mkTestOCaml modul = mkOCamlInterfaceWithSpec "http://localhost:8081" "__tests__/golden/" modul

upperOptions = defaultOptions { fieldLabelModifier = map toUpper }

oo = OCaml.defaultOptions {aesonOptions = upperOptions }

spec :: Spec
spec = do
  describe "OCaml Declaration with Interface: Types with Aeson Options" $ do
    testOptionsInterface "Person" (mkOCamlInterfaceWithOptions oo (Proxy :: Proxy Person))

data Person = Person
  { id :: Int
  , name :: Maybe String
  , created :: UTCTime
  } deriving (Show, Eq, Generic, OCamlType)


instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . fromIntegral <$> (arbitrary :: Gen Integer)

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary Person

instance ToJSON Person where
  toJSON = genericToJSON upperOptions

instance FromJSON Person where
  parseJSON = genericParseJSON upperOptions
