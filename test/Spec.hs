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

type Tuple
  = (Int,Int)

data WithTuple = WithTuple Tuple
  deriving (Show,Eq,Generic, OC.ReasonType)

data SumWithRecord
  = A1 {a1 :: Int}
  | B2 {b2 :: String, b3 :: Int}
  deriving (Show,Eq,Generic, OC.ReasonType)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq,Show,Generic,OC.ReasonType)

data Card =
  Card
    { cardSuit  :: Suit
    , cardValue :: Int
    } deriving (Eq,Show,Generic,OC.ReasonType)

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

tupleSpec :: OC.Spec
tupleSpec =
  OC.Spec
    ["Tuple"]
    [ OC.toReasonTypeSource (Proxy :: Proxy Tuple)
    , OC.toReasonEncoderSource (Proxy :: Proxy Tuple)
    ]

withTupleSpec :: OC.Spec
withTupleSpec =
  OC.Spec
    ["WithTuple"]
    [ OC.toReasonTypeSource (Proxy :: Proxy WithTuple)
    , OC.toReasonEncoderSource (Proxy :: Proxy WithTuple)
    ]

cardSpec :: OC.Spec
cardSpec =
  OC.Spec
    ["Card"]
    [ OC.toReasonTypeSource (Proxy :: Proxy Suit)
    , OC.toReasonEncoderSource (Proxy :: Proxy Suit)
    , OC.toReasonTypeSource (Proxy :: Proxy Card)
    , OC.toReasonEncoderSource (Proxy :: Proxy Card)
    ]

sumWithRecordSpec :: OC.Spec
sumWithRecordSpec =
  OC.Spec
    ["SumWithRecord"]
    [ OC.toReasonTypeSource (Proxy :: Proxy SumWithRecord)
    , OC.toReasonEncoderSource (Proxy :: Proxy SumWithRecord)
    ]


data ADT
  = Product
  | Sum

adtToPath :: ADT -> FilePath
adtToPath Product = "product"
adtToPath Sum = "sum"

testOCamlType :: OC.Spec -> FilePath -> ADT -> SpecWith ()
testOCamlType ocamlSpec typeName adt =
  it typeName $ do
    OC.specsToDir [ocamlSpec] testPath
    automated   <- T.readFile (testPath   <> "/" <> typeName <> ".ml")
    handWritten <- T.readFile (goldenPath <> "/" <> typeName <> ".ml")
    automated `shouldBe` handWritten
  where
    adtPath    = adtToPath adt
    testPath   = "test/temp/" <> adtPath
    goldenPath = "test/golden/" <> adtPath

spec :: Spec
spec =
  describe "toReasonTypeSource" $ do
    testOCamlType personSpec "Person" Product
    testOCamlType companySpec "Company" Product
    testOCamlType onOrOffSpec "OnOrOff" Sum
    testOCamlType nameOrIdNumberSpec "NameOrIdNumber" Sum
    testOCamlType sumVariantSpec "SumVariant" Sum
    testOCamlType withTupleSpec "WithTuple" Sum
    testOCamlType cardSpec "Card" Product
    testOCamlType sumWithRecordSpec "SumWithRecord" Sum
    
main :: IO ()
main = hspec spec
