{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE PolyKinds #-}

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
  } deriving (Show, Eq, Generic, OC.OCamlType)

data Tree a =
  Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq, Generic, OC.OCamlType)

data Holder a = Holder
  { holderId :: Int
  , holderValue :: a
  } deriving (Show, Eq, Generic, OC.OCamlType)

data Holder2 a b = Holder2
  { holder2Id :: Int
  , holder2AValue :: a
  , holder2BValue :: b
  } deriving (Show, Eq, Generic, Read, OC.OCamlType)


data Company = Company
  { address   :: String
  , employees :: [Person]
  } deriving (Show, Eq, Generic, OC.OCamlType)

data OnOrOff = On | Off
  deriving (Show,Eq,Generic, OC.OCamlType)

data NameOrIdNumber = Name String | IdNumber Int
  deriving (Show,Eq,Generic, OC.OCamlType)

data SumVariant
  = HasNothing
  | HasSingleInt Int
  | HasSingleTuple (Int,Int)
  | HasMultipleInts Int Int
  | HasMultipleTuples (Int,Int) (Int,Int)
  deriving (Show,Eq,Generic, OC.OCamlType)  

type Tuple
  = (Int,Int)

data WithTuple = WithTuple Tuple
  deriving (Show,Eq,Generic, OC.OCamlType)

data SumWithRecord
  = A1 {a1 :: Int}
  | B2 {b2 :: String, b3 :: Int}
  deriving (Show,Eq,Generic, OC.OCamlType)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq,Show,Generic,OC.OCamlType)

data Card =
  Card
    { cardSuit  :: Suit
    , cardValue :: Int
    } deriving (Eq,Show,Generic,OC.OCamlType)

personSpec :: OC.Spec
personSpec =
  OC.Spec
    ["Person"]
    [ OC.toOCamlTypeSource (Proxy :: Proxy Person)
    , OC.toOCamlEncoderSource (Proxy :: Proxy Person)
    ]

companySpec :: OC.Spec
companySpec =
  OC.Spec
    ["Company"]
    [ OC.toOCamlTypeSource (Proxy :: Proxy Person)
    , OC.toOCamlEncoderSource (Proxy :: Proxy Person)
    , OC.toOCamlTypeSource (Proxy :: Proxy Company)
    , OC.toOCamlEncoderSource (Proxy :: Proxy Company)
    ]

onOrOffSpec :: OC.Spec
onOrOffSpec =
  OC.Spec
    ["OnOrOff"]
    [ OC.toOCamlTypeSource (Proxy :: Proxy OnOrOff)
    , OC.toOCamlEncoderSource (Proxy :: Proxy OnOrOff)
    ]    

nameOrIdNumberSpec :: OC.Spec
nameOrIdNumberSpec =
  OC.Spec
    ["NameOrIdNumber"]
    [ OC.toOCamlTypeSource (Proxy :: Proxy NameOrIdNumber)
    , OC.toOCamlEncoderSource (Proxy :: Proxy NameOrIdNumber)
    ]

sumVariantSpec :: OC.Spec
sumVariantSpec =
  OC.Spec
    ["SumVariant"]
    [ OC.toOCamlTypeSource (Proxy :: Proxy SumVariant)
    , OC.toOCamlEncoderSource (Proxy :: Proxy SumVariant)
    ]

tupleSpec :: OC.Spec
tupleSpec =
  OC.Spec
    ["Tuple"]
    [ OC.toOCamlTypeSource (Proxy :: Proxy Tuple)
    , OC.toOCamlEncoderSource (Proxy :: Proxy Tuple)
    ]

withTupleSpec :: OC.Spec
withTupleSpec =
  OC.Spec
    ["WithTuple"]
    [ OC.toOCamlTypeSource (Proxy :: Proxy WithTuple)
    , OC.toOCamlEncoderSource (Proxy :: Proxy WithTuple)
    ]

cardSpec :: OC.Spec
cardSpec =
  OC.Spec
    ["Card"]
    [ OC.toOCamlTypeSource (Proxy :: Proxy Suit)
    , OC.toOCamlEncoderSource (Proxy :: Proxy Suit)
    , OC.toOCamlTypeSource (Proxy :: Proxy Card)
    , OC.toOCamlEncoderSource (Proxy :: Proxy Card)
    ]

sumWithRecordSpec :: OC.Spec
sumWithRecordSpec =
  OC.Spec
    ["SumWithRecord"]
    [ OC.toOCamlTypeSource (Proxy :: Proxy SumWithRecord)
    , OC.toOCamlEncoderSource (Proxy :: Proxy SumWithRecord)
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
  describe "toOCamlType" $ do
    it "" $ do
--      print $ OC.toOCamlType (Proxy :: Proxy (Holder OC.TypeParameterRef0))
--      print $ OC.toOCamlType (Proxy :: Proxy (Holder2 OC.TypeParameterRef0 OC.TypeParameterRef1))
--      print $ OC.toOCamlEncoderSource (Proxy :: Proxy (Holder2 OC.TypeParameterRef0 OC.TypeParameterRef1))
      print $ OC.toOCamlTypeSource (Proxy :: Proxy (Holder2 OC.TypeParameterRef0 OC.TypeParameterRef1))
      False `shouldBe` True
  {-
  describe "toOCamlTypeSource" $ do
    testOCamlType personSpec "Person" Product
    testOCamlType companySpec "Company" Product
    testOCamlType onOrOffSpec "OnOrOff" Sum
    testOCamlType nameOrIdNumberSpec "NameOrIdNumber" Sum
    testOCamlType sumVariantSpec "SumVariant" Sum
    testOCamlType withTupleSpec "WithTuple" Sum
    testOCamlType cardSpec "Card" Product
    testOCamlType sumWithRecordSpec "SumWithRecord" Sum
  -}
main :: IO ()
main = hspec spec
