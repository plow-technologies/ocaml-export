{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Product where
-- base
import Data.Typeable
import GHC.Generics
-- time
import Data.Time
-- aeson
import Data.Aeson (FromJSON, ToJSON)
-- hspec
import Test.Hspec (SpecWith)
-- QuickCheck
import Test.QuickCheck
-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT
-- ocaml-export
import OCaml.Export hiding (mkGoldenFiles)
import Util

type TTPackage
  = OCamlTypeInFile Simple "test/ocaml/Simple"

type TTTPackage
--  = (OCamlModule '["File"] :> (OCamlTypeInFile Simple "test/ocaml/Simple"))
  = ((OCamlTypeInFile (Card) "test/ocaml/Card") :> (OCamlTypeInFile (Simple) "test/ocaml/Simple"))

type ProductPackage
  = OCamlPackage "product" NoDependency
    :> (OCamlModule '["SimpleChoice"] :> SimpleChoice
  :<|> OCamlModule '["Person"] :> Person
  :<|> OCamlModule '["Company"] :> Company
  :<|> OCamlModule '["Card"] :> Suit :> Card
  :<|> OCamlModule '["CustomOption"] :> Company2
  :<|> OCamlModule '["OneTypeParameter"] :> OneTypeParameter TypeParameterRef0
  :<|> OCamlModule '["TwoTypeParameters"] :> TwoTypeParameters TypeParameterRef0 TypeParameterRef1
  :<|> OCamlModule '["ThreeTypeParameters"] :> Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2
  :<|> OCamlModule '["SubTypeParameter"] :> SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2
  :<|> OCamlModule '["UnnamedProduct"] :> UnnamedProduct
  :<|> OCamlModule '["ComplexProduct"] :> OCamlTypeInFile Simple "test/ocaml/Simple" :> ComplexProduct
  :<|> OCamlModule '["Wrapper"]
         :> Wrapper TypeParameterRef0
         :> IntWrapped
         :> MaybeWrapped
         :> EitherWrapped
         :> ComplexWrapped
         :> SumWrapped
         :> TupleWrapped
         :> HalfWrapped TypeParameterRef0
         :> PartiallyWrapped TypeParameterRef0 TypeParameterRef1 TypeParameterRef2
         :> ScrambledTypeParameterRefs TypeParameterRef0 TypeParameterRef1 TypeParameterRef2 TypeParameterRef3 TypeParameterRef4 TypeParameterRef5
         :> WrappedWrapper
         :> WrapThree TypeParameterRef0 TypeParameterRef1 TypeParameterRef2
         :> WrapThreeUnfilled TypeParameterRef0 TypeParameterRef1 TypeParameterRef2
         :> WrapThreeFilled
         :> WrapThreePartiallyFilled TypeParameterRef0
--         :> TypeSynonymKey TypeParameterRef0
--         :> NewTypeKey TypeParameterRef0         
       )

compareInterfaceFiles :: FilePath -> SpecWith ()
compareInterfaceFiles = compareFiles "test/interface" "product" True

data SimpleChoice =
  SimpleChoice
    { choice :: Either String Int
    } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary SimpleChoice where
  arbitrary = SimpleChoice <$> arbitrary

instance ToADTArbitrary SimpleChoice

data Person = Person
  { id :: Int
  , name :: Maybe String
  , created :: UTCTime
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary Person

data Company = Company
  { address   :: String
  , employees :: [Person]
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Company where
  arbitrary = do
    k <- choose (1,3)
    v <- vector k
    Company <$> arbitrary <*> pure v

instance ToADTArbitrary Company

data Company2 = Company2
  { address2   :: String
  , boss :: Maybe Person
  } deriving (Show, Eq, Generic, OCamlType, FromJSON, ToJSON)

instance Arbitrary Company2 where
  arbitrary = Company2 <$> arbitrary <*> arbitrary

instance ToADTArbitrary Company2

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary Suit where
  arbitrary = elements [Clubs, Diamonds, Hearts, Spades]

instance ToADTArbitrary Suit

data Card =
  Card
    { cardSuit  :: Suit
    , cardValue :: Int
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary

instance ToADTArbitrary Card

data OneTypeParameter a =
  OneTypeParameter
    { otpId :: Int
    , otpFirst :: a
    } deriving (Eq,Show,Generic,FromJSON,ToJSON)

instance Arbitrary (OneTypeParameter TypeParameterRef0) where
  arbitrary = OneTypeParameter <$> arbitrary <*> arbitrary

instance ToADTArbitrary (OneTypeParameter TypeParameterRef0)

instance (Typeable a, OCamlType a) => (OCamlType (OneTypeParameter a))

data TwoTypeParameters a b =
  TwoTypeParameters
    { ttpId :: Int
    , ttpFirst :: a
    , ttpSecond :: b
    , ttpThird :: (a, b)
    } deriving (Eq,Show,Generic,FromJSON,ToJSON)

instance Arbitrary (TwoTypeParameters TypeParameterRef0 TypeParameterRef1) where
  arbitrary = TwoTypeParameters <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary (TwoTypeParameters TypeParameterRef0 TypeParameterRef1)

instance (Typeable a, OCamlType a, Typeable b, OCamlType b) => (OCamlType (TwoTypeParameters a b))

data Three a b c =
  Three
    { threeId :: Int
    , threeFirst :: a
    , threeSecond :: b
    , threeThird :: c
    , threeString :: String
    } deriving (Eq,Show,Generic,FromJSON,ToJSON)

instance Arbitrary (Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary (Three TypeParameterRef0 TypeParameterRef1 TypeParameterRef2)

instance (Typeable a, OCamlType a, Typeable b, OCamlType b, Typeable c, OCamlType c) => (OCamlType (Three a b c))

data SubTypeParameter a b c =
  SubTypeParameter
    { listA :: [a]
    , maybeB :: Maybe b
    , tupleC :: (c,b)
    } deriving (Eq,Show,Generic,FromJSON,ToJSON)

instance Arbitrary (SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2) where
  arbitrary = do
    k <- choose (1,3)
    v <- vector k
    SubTypeParameter <$> pure v <*> arbitrary <*> arbitrary

instance ToADTArbitrary (SubTypeParameter TypeParameterRef0 TypeParameterRef1 TypeParameterRef2)

instance (Typeable a, OCamlType a, Typeable b, OCamlType b, Typeable c, OCamlType c) => (OCamlType (SubTypeParameter a b c))

data UnnamedProduct = UnnamedProduct String Int
  deriving (Eq, Read, Show, Generic, OCamlType, FromJSON, ToJSON)
  
instance Arbitrary UnnamedProduct where
  arbitrary = UnnamedProduct <$> arbitrary <*> arbitrary

instance ToADTArbitrary UnnamedProduct

data Simple =
  Simple
    { sa :: Int
    , sb :: String
    } deriving (Eq,Show,Generic,FromJSON,ToJSON)

instance OCamlType Simple where
  toOCamlType _ = typeableToOCamlType (Proxy :: Proxy Simple)

instance ToADTArbitrary Simple
instance Arbitrary Simple where
  arbitrary = Simple <$> arbitrary <*> arbitrary

data ComplexProduct =
  ComplexProduct
    { cp0 :: Either Person [Int]
    , cp1 :: [(Int, Either String Double)]
    , cp2 :: [[Int]]
    , cp3 :: Maybe [Int]
    , cp4 :: Either Simple Int
    } deriving (Eq,Show,Generic,OCamlType,FromJSON,ToJSON)

instance Arbitrary ComplexProduct where
  arbitrary = do
    k0 <- choose (1,3)
    v0 <- vector k0

    k1 <- choose (1,3)
    v1 <- vector k1
    
    ComplexProduct <$> arbitrary <*> pure v0 <*> pure v1 <*> arbitrary <*> arbitrary

instance ToADTArbitrary ComplexProduct

data Wrapper a =
  Wrapper
    { wpa :: a
    } deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance (ToADTArbitrary a, Arbitrary a) => ToADTArbitrary (Wrapper a)
instance (Arbitrary a) => Arbitrary (Wrapper a) where
  arbitrary = Wrapper <$> arbitrary
instance (Typeable a, OCamlType a) => (OCamlType (Wrapper a))

data IntWrapped =
  IntWrapped
    { iw :: Wrapper Int
    } deriving (Eq,Show,Generic,OCamlType,ToJSON,FromJSON)

instance ToADTArbitrary IntWrapped
instance Arbitrary IntWrapped where
  arbitrary = IntWrapped <$> arbitrary

data MaybeWrapped =
  MaybeWrapped
    { mw :: Wrapper (Maybe Int)
    } deriving (Eq,Show,Generic,OCamlType,ToJSON,FromJSON)

instance ToADTArbitrary MaybeWrapped
instance Arbitrary MaybeWrapped where
  arbitrary = MaybeWrapped <$> arbitrary

data EitherWrapped =
  EitherWrapped
    { ew :: Wrapper (Either Int Double)
    } deriving (Eq,Show,Generic,OCamlType,ToJSON,FromJSON)

instance ToADTArbitrary EitherWrapped
instance Arbitrary EitherWrapped where
  arbitrary = EitherWrapped <$> arbitrary

data ComplexWrapped =
  ComplexWrapped
    { cw :: Wrapper (Either (Maybe Char) Double)
    } deriving (Eq,Show,Generic,OCamlType,ToJSON,FromJSON)

instance ToADTArbitrary ComplexWrapped
instance Arbitrary ComplexWrapped where
  arbitrary = ComplexWrapped <$> arbitrary

data TupleWrapped =
  TupleWrapped
    { tw :: Wrapper (Int,String,Double)
    } deriving (Eq,Show,Generic,OCamlType,ToJSON,FromJSON)

instance ToADTArbitrary TupleWrapped
instance Arbitrary TupleWrapped where
  arbitrary = TupleWrapped <$> arbitrary

data SumWrapped
  = SW1
  | SW2 (Wrapper Int)
  | SW3 (Wrapper (Maybe String))
  | SW4 (Wrapper (Either Int String))
  deriving (Eq,Show,Generic,OCamlType,ToJSON,FromJSON)

instance ToADTArbitrary SumWrapped
instance Arbitrary SumWrapped where
  arbitrary =
    oneof
      [ pure SW1
      , SW2 <$> arbitrary
      , SW3 <$> arbitrary
      , SW4 <$> arbitrary
      ]

data HalfWrapped a =
  HalfWrapped
    { hw :: Wrapper (Either Int a)
    } deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance Arbitrary (HalfWrapped TypeParameterRef0) where
  arbitrary = HalfWrapped <$> arbitrary

instance ToADTArbitrary (HalfWrapped TypeParameterRef0)

instance (Typeable a, OCamlType a) => (OCamlType (HalfWrapped a))

data PartiallyWrapped a b c =
  PartiallyWrapped
    { pw :: Wrapper (Either Int (String,b,Double,c,a))
    } deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance Arbitrary (PartiallyWrapped TypeParameterRef0 TypeParameterRef1 TypeParameterRef2) where
  arbitrary = PartiallyWrapped <$> arbitrary

instance ToADTArbitrary (PartiallyWrapped TypeParameterRef0 TypeParameterRef1 TypeParameterRef2)

instance (Typeable a, OCamlType a, Typeable b, OCamlType b, Typeable c, OCamlType c) => (OCamlType (PartiallyWrapped a b c))

-- | type parameter declaration and use order are different
data ScrambledTypeParameterRefs a b c d e f =
  ScrambledTypeParameterRefs
    { stprb :: b
    , stprd :: d
    , stpre :: e
    , stpra :: a
    , stprf :: f
    , stprc :: c
    } deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance Arbitrary (ScrambledTypeParameterRefs TypeParameterRef0 TypeParameterRef1 TypeParameterRef2 TypeParameterRef3 TypeParameterRef4 TypeParameterRef5) where
  arbitrary = ScrambledTypeParameterRefs <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToADTArbitrary (ScrambledTypeParameterRefs TypeParameterRef0 TypeParameterRef1 TypeParameterRef2 TypeParameterRef3 TypeParameterRef4 TypeParameterRef5)

instance (Typeable a, OCamlType a, Typeable b, OCamlType b, Typeable c, OCamlType c, Typeable d, OCamlType d, Typeable e, OCamlType e, Typeable f, OCamlType f) => (OCamlType (ScrambledTypeParameterRefs a b c d e f))

data WrappedWrapper =
  WrappedWrapper
--    { ww :: Maybe (Wrapper (Maybe String))
    { ww :: Maybe (Wrapper (Maybe Int))
    } deriving (Eq,Show,Generic,ToJSON,FromJSON,OCamlType)

instance Arbitrary WrappedWrapper where
  arbitrary = WrappedWrapper <$> arbitrary

instance ToADTArbitrary WrappedWrapper

data WrapThree a b c =
  WrapThree
    { wp2a :: a
    , wp2b :: b
    , wp2ab :: (a, b)
    , wp2cb :: (c, b)
    } deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance (ToADTArbitrary a, Arbitrary a, ToADTArbitrary b, Arbitrary b, ToADTArbitrary c, Arbitrary c) => ToADTArbitrary (WrapThree a b c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (WrapThree a b c) where
  arbitrary = WrapThree <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Typeable a, OCamlType a, Typeable b, OCamlType b, Typeable c, OCamlType c) => (OCamlType (WrapThree a b c))

data WrapThreeUnfilled a b c =
  WrapThreeUnfilled
    { zed :: String
    , unfilled :: WrapThree a b c
    } deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance (ToADTArbitrary a, Arbitrary a, ToADTArbitrary b, Arbitrary b, ToADTArbitrary c, Arbitrary c) => ToADTArbitrary (WrapThreeUnfilled a b c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (WrapThreeUnfilled a b c) where
  arbitrary = WrapThreeUnfilled <$> arbitrary <*> arbitrary
instance (Typeable a, OCamlType a, Typeable b, OCamlType b, Typeable c, OCamlType c) => (OCamlType (WrapThreeUnfilled a b c))

data WrapThreeFilled =
  WrapThreeFilled
    { foo :: String
    , filled :: WrapThree Int Double Person
    } deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance ToADTArbitrary WrapThreeFilled
instance Arbitrary WrapThreeFilled where
  arbitrary = WrapThreeFilled <$> arbitrary <*> arbitrary
instance OCamlType WrapThreeFilled

data WrapThreePartiallyFilled a =
  WrapThreePartiallyFilled
    { bar :: String
    , bar2 :: [Int]
    , partiallyFilled :: WrapThree Float a Double
    } deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance (ToADTArbitrary a, Arbitrary a) => ToADTArbitrary (WrapThreePartiallyFilled a)
instance Arbitrary a => Arbitrary (WrapThreePartiallyFilled a) where
  arbitrary = WrapThreePartiallyFilled <$> arbitrary <*> arbitrary <*> arbitrary
instance (Typeable a, OCamlType a) => OCamlType (WrapThreePartiallyFilled a)

-- phantom types

data TypeSynonymKey a = String
  deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance (ToADTArbitrary a, Arbitrary a) => ToADTArbitrary (TypeSynonymKey a)
instance Arbitrary a => Arbitrary (TypeSynonymKey a) where
  arbitrary = arbitrary
instance (Typeable a, OCamlType a) => OCamlType (TypeSynonymKey a)

newtype NewTypeKey a = NewTypeKey String
  deriving (Eq,Show,Generic,ToJSON,FromJSON)

instance (ToADTArbitrary a, Arbitrary a) => ToADTArbitrary (NewTypeKey a)
instance Arbitrary (NewTypeKey a) where
  arbitrary = NewTypeKey <$> arbitrary
instance (Typeable a, OCamlType a) => OCamlType (NewTypeKey a)


{-
λ> toOCamlType (Proxy :: Proxy (Key User))
OCamlDatatype (HaskellTypeMetaData "Key" "Database.Persist.Class.PersistEntity" "persistent-2.6-HdpHylIi1gZ4QjAhgpXd6i") "Key" (OCamlValueConstructor (NamedConstructor "Key" OCamlEmpty))
λ


λ> toOCamlType (Proxy :: Proxy (UserTag))
OCamlDatatype (HaskellTypeMetaData "UserTag" "Onping.Persist.Models.Internal" "onping-types-0.12.0.0-537fBzusEVB7hcaZbAcQql") "UserTag" (OCamlValueConstructor (RecordConstructor "UserTag" (Values (Values (Values (OCamlField "userTagUser" (OCamlRefApp (Key User) (OCamlRef (HaskellTypeMetaData "User" "Plowtech.Authentication.Persist.Models" "plowtech-authentication-types-0.3.0.0-HOAisznLARVIB1OtLfSJK3") "User"))) (OCamlField "userTagOwner" (OCamlRefApp (Key User) (OCamlRef (HaskellTypeMetaData "User" "Plowtech.Authentication.Persist.Models" "plowtech-authentication-types-0.3.0.0-HOAisznLARVIB1OtLfSJK3") "User")))) (Values (OCamlField "userTagGroup" (OCamlRefApp (Key Group) (OCamlRef (HaskellTypeMetaData "Group" "Onping.Persist.Models.Internal" "onping-types-0.12.0.0-537fBzusEVB7hcaZbAcQql") "Group"))) (Values (OCamlField "userTagSuperGroup" (OCamlRefApp (Key Group) (OCamlRef (HaskellTypeMetaData "Group" "Onping.Persist.Models.Internal" "onping-types-0.12.0.0-537fBzusEVB7hcaZbAcQql") "Group"))) (OCamlField "userTagDefaultDash" (OCamlRefApp (Key Dashboard) (OCamlRef (HaskellTypeMetaData "Dashboard" "Onping.Persist.Models.Internal" "onping-types-0.12.0.0-537fBzusEVB7hcaZbAcQql") "Dashboard")))))) (Values (Values (OCamlField "userTagPhone" (OCamlPrimitiveRef (OOption (OCamlPrimitive OInt)))) (OCamlField "userTagName" (OCamlPrimitiveRef (OOption (OCamlPrimitive OString))))) (Values (OCamlField "userTagCallAlert" (OCamlPrimitiveRef OBool)) (Values (OCamlField "userTagEmailAlert" (OCamlPrimitiveRef OBool)) (OCamlField "userTagTextAlert" (OCamlPrimitiveRef OBool))))))))

-}
