{-|
Module      : OCaml.BuckleScript.Types
Description : Tree representation of Haskell datatypes in OCaml
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

OCaml datatype representation of a Haskell datatype. A recursive tree that
can be interpreted to output OCaml code. It is meant to encode a Haskell type
into OCaml and make json seraliazers that match the output from Generic aeson
instances.
-}

{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module OCaml.BuckleScript.Types
  ( OCamlDatatype (..)
  , OCamlPrimitive (..)
  , OCamlConstructor (..)
  , ValueConstructor (..)
  , EnumeratorConstructor (..)
  , OCamlValue (..)
  , OCamlType (..)
  , HaskellTypeMetaData (..)
  , OCamlTypeMetaData (..)

  , typeableToOCamlType
  -- fill type parameters of a proxy when calling toOCamlType
  -- so the kind is *
  -- e.g. `toOCamlType (Proxy :: Proxy (Either TypeParameterRef0 TypeParameterRef1))`
  , TypeParameterRef0
  , TypeParameterRef1
  , TypeParameterRef2
  , TypeParameterRef3
  , TypeParameterRef4
  , TypeParameterRef5

  -- functions for manipulating and querying the data type tree
  , getTypeParameterRefNames
  , getOCamlValues
  , getTypeParameters
  , isTypeParameterRef
  , mkModulePrefix
  , oCamlValueIsFloat

  -- Typeable functions
  , primitiveTyConToOCamlTypeText
  , typeParameterRefTyConToOCamlTypeText
  , tupleTyConToSize
  , typeRepIsString
  , typeRepToHaskellTypeMetaData
  , tyConToHaskellTypeMetaData
  , ocamlDatatypeHasTypeParameter

  , typeParameterToRef
  ) where

-- base
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Time
import Data.Typeable
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Generics
import GHC.TypeLits (symbolVal, KnownSymbol)
import Prelude

import qualified Data.Map as Map

-- aeson
import Data.Aeson (ToJSON, FromJSON)
-- bytestring
import Data.ByteString (ByteString)
-- text
import Data.Text (Text)
import qualified Data.Text as T

-- QuickCheck
import Test.QuickCheck

-- quicheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT

-- | Top level of an OCaml datatype. A data type may be composed of
--   primitives and/or a combination of constructors and primitives.
--   OCamlDatatype is recursive via OCamlConstructor -> ValueConstructor
--   -> OCamlValue -> OCamlPrimitive -> OCamlDatatype.
data OCamlDatatype
  = OCamlDatatype HaskellTypeMetaData Text OCamlConstructor -- ^ The name of a type and its type constructor
  | OCamlPrimitive OCamlPrimitive -- ^ A primitive value
  deriving (Show, Eq)

-- | Store data about the Haskell origin of a type.
data HaskellTypeMetaData =
  HaskellTypeMetaData
    Text -- "TypeName"
    Text -- "Module.Name"
    Text -- "package-name"
    deriving (Show, Eq, Ord)

-- | Store data about the OCaml destination of a type.
data OCamlTypeMetaData =
  OCamlTypeMetaData
    Text -- "typeName"
    [Text] -- ["File","Path"]
    [Text] -- ["Sub","Module"]
    deriving (Show, Eq, Ord)

-- | Smallest unit of computation in OCaml.
data OCamlPrimitive
  = OInt -- ^ int
  | OBool -- ^ bool, boolean
  | OChar -- ^ char, it gets interpreted as a string because OCaml char does not support UTF-8
  | ODate -- ^ Js_date.t
  | OFloat -- ^ float
  | OString -- ^ string
  | OUnit -- ^ ()
  | OList OCamlDatatype -- ^ 'a list, 'a Js_array.t
  | OOption OCamlDatatype -- ^ 'a option
  | OEither OCamlDatatype OCamlDatatype -- ^ 'l 'r Aeson.Compatibility.Either.t
  | OTuple2 OCamlDatatype OCamlDatatype -- ^ (*)
  | OTuple3 OCamlDatatype OCamlDatatype OCamlDatatype -- ^ (**)
  | OTuple4 OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype -- ^ (***)
  | OTuple5 OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype -- ^ (****)
  | OTuple6 OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype -- ^ (*****)
  deriving (Show, Eq)

-- | OCamlConstructor take values to create a new instances of a type.
data OCamlConstructor 
  = OCamlValueConstructor ValueConstructor -- ^ Sum, record (product with named fields) or product without named fields
  | OCamlEnumeratorConstructor [EnumeratorConstructor] -- ^ Sum of enumerations only. If a sum contains enumerators and at least one constructor with a value then it is an OCamlValueConstructor
  | OCamlSumOfRecordConstructor Text ValueConstructor -- ^ Sum that contains at least one record. This construction is unique to Haskell. pIt has special Encoding and Decoding rules in order to output a valid OCaml program. i.e. `data A = A {a :: Int} | B {b :: String}`
  deriving (Show, Eq)

-- | OCamlConstructor of one RecordConstructor is a record type.
--   OCamlConstructor of one NamedConstructor that has one value is a Haskell newtype.
--   OCamlConstructor of one NamedConstructor is a product without field names.
--   OCamlConstructor of multiple NamedConstructors is a sum type.
--   OCamlConstructor of at least one RecordConstructor and any other amount of ValueConstructors greater than one is a OCamlSumWithRecordConstructor.
data ValueConstructor
  = NamedConstructor Text OCamlValue -- ^ Product without named fields
  | RecordConstructor Text OCamlValue -- ^ Product with named fields
  | MultipleConstructors [ValueConstructor] -- ^ Sum type
  deriving (Show, Eq)

-- | Enumerators have no values, only tags.
data EnumeratorConstructor
  = EnumeratorConstructor Text -- ^ Enumerator and its tag
  deriving (Show, Eq)

-- | Expected types of a constructor
data OCamlValue
  = OCamlRef HaskellTypeMetaData Text -- ^ The name of a non-primitive data type
  | OCamlRefApp TypeRep OCamlValue -- ^ A type constructor that has at least one type parameter filled
  | OCamlTypeParameterRef Text -- ^ Type parameters like `a` in `Maybe a`
  | OCamlEmpty -- ^ a place holder for OCaml value. It can represent the end of a list or an Enumerator in a mixed sum
  | OCamlPrimitiveRef OCamlPrimitive -- ^ A primitive OCaml type like `int`, `string`, etc.
  | OCamlField Text OCamlValue -- ^ A field name and its type from a record
  | Values OCamlValue OCamlValue -- ^ Used for multiple types in a sum type

  deriving (Show, Eq)
--  -- ^
------------------------------------------------------------
-- | Create an OCaml type from a Haskell type. Use the Generic
--   definition when possible. It also expects `ToJSON` and `FromJSON`
--   to be derived generically.
class OCamlType a where
  toOCamlType :: a -> OCamlDatatype
  toOCamlType = genericToOCamlDatatype . from
  default toOCamlType :: (Generic a, GenericOCamlDatatype (Rep a)) =>
    a -> OCamlDatatype

------------------------------------------------------------
class GenericOCamlDatatype f where
  genericToOCamlDatatype :: f a -> OCamlDatatype

-- | Capture the Haskell type at the left side declaration `data Maybe a`, `data Person`, etc..
--   Transform the constructor, depending on its values, if necessary.
instance (KnownSymbol typ, KnownSymbol package, KnownSymbol modul, GenericValueConstructor f) => GenericOCamlDatatype (M1 D ('MetaData typ modul package 'False) f) where
  genericToOCamlDatatype datatype =
    OCamlDatatype
      (HaskellTypeMetaData
       (T.pack $ symbolVal (Proxy :: Proxy typ))
       (T.pack $ symbolVal (Proxy :: Proxy modul))
       (T.pack $ symbolVal (Proxy :: Proxy package)))
      (T.pack (datatypeName datatype))
      (transform (OCamlValueConstructor (genericToValueConstructor (unM1 datatype))))
    where
      transform ocamlConstructor =
        if isEnumeration ocamlConstructor
          then transformToEnumeration ocamlConstructor
          else 
            if isSumWithRecord ocamlConstructor
              then transformToSumOfRecord (T.pack (datatypeName datatype)) ocamlConstructor
              else ocamlConstructor

instance (KnownSymbol typ, KnownSymbol package, KnownSymbol modul, GenericValueConstructor f) => GenericOCamlDatatype (M1 D ('MetaData typ modul package 'True) f) where
  genericToOCamlDatatype datatype =
    OCamlDatatype
      (HaskellTypeMetaData
       (T.pack $ symbolVal (Proxy :: Proxy typ))
       (T.pack $ symbolVal (Proxy :: Proxy modul))
       (T.pack $ symbolVal (Proxy :: Proxy package)))
      (T.pack (datatypeName datatype))
      (transform (OCamlValueConstructor (genericToValueConstructor (unM1 datatype))))
    where
      transform ocamlConstructor =
        if isEnumeration ocamlConstructor
          then transformToEnumeration ocamlConstructor
          else 
            if isSumWithRecord ocamlConstructor
              then transformToSumOfRecord (T.pack (datatypeName datatype)) ocamlConstructor
              else ocamlConstructor

------------------------------------------------------------
class GenericValueConstructor f where
  genericToValueConstructor :: f a -> ValueConstructor

-- | Capture the Haskell type at the constructor. `Just` or `Nothing` from
--   `data Maybe a = Just a | Nothing`. 
instance (Constructor c, GenericOCamlValue f) => GenericValueConstructor (C1 c f) where
  genericToValueConstructor constructor =
    if conIsRecord constructor
      then RecordConstructor name (genericToOCamlValue (unM1 constructor))
      else NamedConstructor name (genericToOCamlValue (unM1 constructor))
    where
      name = T.pack $ conName constructor

-- | Capture the Haskell right side at the sum partition `|`.
instance (GenericValueConstructor f, GenericValueConstructor g) =>
         GenericValueConstructor (f :+: g) where
  genericToValueConstructor _ =
    MultipleConstructors
      [ genericToValueConstructor (undefined :: f p)
      , genericToValueConstructor (undefined :: g p)
      ]

------------------------------------------------------------
class GenericOCamlValue f where
  genericToOCamlValue :: f a -> OCamlValue

-- | Capture the constructor field.
instance (Selector s, GenericOCamlValue a) =>
         GenericOCamlValue (S1 s a) where
  genericToOCamlValue selector =
    case selName selector of
      ""   -> genericToOCamlValue (undefined :: a p)
      name -> OCamlField (T.pack name) (genericToOCamlValue (undefined :: a p))

-- | Capture the product comma.
instance (GenericOCamlValue f, GenericOCamlValue g) =>
         GenericOCamlValue (f :*: g) where
  genericToOCamlValue _ =
    Values
      (genericToOCamlValue (undefined :: f p))
      (genericToOCamlValue (undefined :: g p))

-- | Enumerator, constructor with no values.
instance GenericOCamlValue U1 where
  genericToOCamlValue _ = OCamlEmpty

-- | Handle type parameter. There are found in the order of declaration on the right hand side of a type.
--   Reordering may be necessary for TypeParameterRefs.
instance (OCamlType a, Typeable a) => GenericOCamlValue (Rec0 a) where
  genericToOCamlValue _ = typeRepToOCamlValue $ typeRep (Proxy :: Proxy a)

typeRepToOCamlValue :: TypeRep -> OCamlValue
typeRepToOCamlValue t =
  -- check if the type is a primitive
  case Map.lookup hd typeParameterRefTyConToOCamlTypeText of
    Just p -> OCamlTypeParameterRef p
    Nothing ->
      case primitiveTypeRepToOCamlPrimitive t of
        Just primitive -> OCamlPrimitiveRef primitive
        Nothing ->
          -- if it has no typeParams then it mkRef
          if length typeParams == 0
          then mkRef (tyConToHaskellTypeMetaData hd) (T.pack . show $ hd)
          else OCamlRefApp t (mkValues)
  where
    (hd, typeParams) = splitTyConApp t

    typeParameterRefs = (T.append) <$> ["a"] <*> (T.pack . show <$> ([0..5] :: [Int]))

    mkRef haskellTypeMetaData n =
      if n `elem` typeParameterRefs
      then OCamlTypeParameterRef n
      else OCamlRef haskellTypeMetaData n

    mkValues =
      if length typeParams == 0
      then OCamlEmpty
      else
        if length typeParams == 1
        then Values (typeRepToOCamlValue $ head typeParams) OCamlEmpty
        else
          if length typeParams == 2
          then Values (typeRepToOCamlValue $ head typeParams) (typeRepToOCamlValue $ head $ tail typeParams)
          else Values (typeRepToOCamlValue $ head typeParams) (foldl (\b a -> Values b (typeRepToOCamlValue a)) (typeRepToOCamlValue $ head $ tail typeParams) (tail $ tail typeParams))


typeParameterToRef :: Map.Map TypeRep Text
typeParameterToRef = Map.fromList
  [ ( typeRep (Proxy :: Proxy TypeParameterRef0), "a0")
  , ( typeRep (Proxy :: Proxy TypeParameterRef1), "a1")
  , ( typeRep (Proxy :: Proxy TypeParameterRef2), "a2")
  , ( typeRep (Proxy :: Proxy TypeParameterRef3), "a3")
  , ( typeRep (Proxy :: Proxy TypeParameterRef4), "a4")
  , ( typeRep (Proxy :: Proxy TypeParameterRef5), "a5")
  ]

primitiveTypeRepToOCamlPrimitive :: TypeRep -> Maybe OCamlPrimitive
primitiveTypeRepToOCamlPrimitive t =
  mkOCamlPrimitive $ length typeParams
  where
    (hd, typeParams) = splitTyConApp t
    mkOCamlPrimitive l
      | l == 0 = Map.lookup hd zero
      | l == 1 = one hd (typeParams !! 0)
      | l == 2 = two hd (typeParams !! 0) (typeParams !! 1)
      | l == 3 = three hd (typeParams !! 0) (typeParams !! 1) (typeParams !! 2)
      | l == 4 = four hd (typeParams !! 0) (typeParams !! 1) (typeParams !! 2) (typeParams !! 3)
      | l == 5 = five hd (typeParams !! 0) (typeParams !! 1) (typeParams !! 2) (typeParams !! 3) (typeParams !! 4)
      | l == 6 = six hd (typeParams !! 0) (typeParams !! 1) (typeParams !! 2) (typeParams !! 3) (typeParams !! 4) (typeParams !! 5)
      | otherwise = Nothing
    
    zero :: Map.Map TyCon OCamlPrimitive
    zero = Map.fromList
      [ ( typeRepTyCon $ typeRep (Proxy :: Proxy Int), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int8), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int16), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int32), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int64), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Integer), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word8), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word16), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word32), OInt)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word64), OInt)   
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Bool), OBool)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Char), OChar)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy UTCTime), ODate)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Float), OFloat)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Double), OFloat)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy Text), OString)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy ByteString), OString)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy String), OString)
      , ( typeRepTyCon $ typeRep (Proxy :: Proxy ()), OUnit)
      ]

    one :: TyCon -> TypeRep -> Maybe OCamlPrimitive
    one tyCon t0 =
      if tyCon == (typeRepTyCon $ typeRep (Proxy :: Proxy []))
      then Just $ OList $ mkOCamlDatatype t0
      else
        if tyCon == (typeRepTyCon $ typeRep (Proxy :: Proxy Maybe))
        then Just $ OOption $ mkOCamlDatatype t0
        else Nothing

    two :: TyCon -> TypeRep -> TypeRep -> Maybe OCamlPrimitive
    two tyCon t0 t1 =
      if tyCon == (typeRepTyCon $ typeRep (Proxy :: Proxy Either))
      then Just $ OEither (mkOCamlDatatype t0) (mkOCamlDatatype t1)
      else
        if tyCon == (typeRepTyCon $ typeRep (Proxy :: Proxy (,)))
        then Just $ OTuple2 (mkOCamlDatatype t0) (mkOCamlDatatype t1)
        else Nothing

    three :: TyCon -> TypeRep -> TypeRep -> TypeRep -> Maybe OCamlPrimitive
    three tyCon t0 t1 t2 =
      if tyCon == (typeRepTyCon $ typeRep (Proxy :: Proxy (,,)))
      then Just $ OTuple3 (mkOCamlDatatype t0) (mkOCamlDatatype t1) (mkOCamlDatatype t2)
      else Nothing

    four :: TyCon -> TypeRep -> TypeRep -> TypeRep -> TypeRep -> Maybe OCamlPrimitive
    four tyCon t0 t1 t2 t3 =
      if tyCon == (typeRepTyCon $ typeRep (Proxy :: Proxy (,,,)))
      then Just $ OTuple4 (mkOCamlDatatype t0) (mkOCamlDatatype t1) (mkOCamlDatatype t2) (mkOCamlDatatype t3)
      else Nothing

    five :: TyCon -> TypeRep -> TypeRep -> TypeRep -> TypeRep -> TypeRep -> Maybe OCamlPrimitive
    five tyCon t0 t1 t2 t3 t4 =
      if tyCon == (typeRepTyCon $ typeRep (Proxy :: Proxy (,,,,)))
      then Just $ OTuple5 (mkOCamlDatatype t0) (mkOCamlDatatype t1) (mkOCamlDatatype t2) (mkOCamlDatatype t3) (mkOCamlDatatype t4)
      else Nothing

    six :: TyCon -> TypeRep -> TypeRep -> TypeRep -> TypeRep -> TypeRep -> TypeRep -> Maybe OCamlPrimitive
    six tyCon t0 t1 t2 t3 t4 t5 =
      if tyCon == (typeRepTyCon $ typeRep (Proxy :: Proxy (,,,,,)))
      then Just $ OTuple6 (mkOCamlDatatype t0) (mkOCamlDatatype t1) (mkOCamlDatatype t2) (mkOCamlDatatype t3) (mkOCamlDatatype t4) (mkOCamlDatatype t5)
      else Nothing

    typeParameterRefs = (T.append) <$> ["TypeParameterRef"] <*> (T.pack . show <$> ([0..5] :: [Int]))

    mkTypeParameterRef x =
      case x of
        "TypeParameterRef0" -> toOCamlType (Proxy :: Proxy TypeParameterRef0)
        "TypeParameterRef1" -> toOCamlType (Proxy :: Proxy TypeParameterRef1)
        "TypeParameterRef2" -> toOCamlType (Proxy :: Proxy TypeParameterRef2)
        "TypeParameterRef3" -> toOCamlType (Proxy :: Proxy TypeParameterRef3)
        "TypeParameterRef4" -> toOCamlType (Proxy :: Proxy TypeParameterRef4)
        "TypeParameterRef5" -> toOCamlType (Proxy :: Proxy TypeParameterRef5)

    mkOCamlDatatype x =
      case primitiveTypeRepToOCamlPrimitive x of
        Just primitive -> OCamlPrimitive primitive
        Nothing ->
          if aTyConName `elem` typeParameterRefs
          then mkTypeParameterRef (show tyc)
          else
            OCamlDatatype
              (tyConToHaskellTypeMetaData tyc)
              aTyConName
              (OCamlValueConstructor . NamedConstructor aTyConName $ typeRepToOCamlValue x)
      where
        tyc = typeRepTyCon x
        aTyConName = T.pack . show $ tyc

-- OCamlType instances for primitives

instance OCamlType a => OCamlType [a] where
  toOCamlType _ = OCamlPrimitive (OList (toOCamlType (Proxy :: Proxy a)))

instance OCamlType a => OCamlType (Maybe a) where
  toOCamlType _ = OCamlPrimitive (OOption (toOCamlType (Proxy :: Proxy a)))

instance (OCamlType l, OCamlType r) => OCamlType (Either l r) where
  toOCamlType _ = OCamlPrimitive (OEither (toOCamlType (Proxy :: Proxy l)) (toOCamlType (Proxy :: Proxy r)))

instance OCamlType () where
  toOCamlType _ = OCamlPrimitive OUnit

instance OCamlType Text where
  toOCamlType _ = OCamlPrimitive OString

instance OCamlType ByteString where
  toOCamlType _ = OCamlPrimitive OString

instance OCamlType Day where
  toOCamlType _ = OCamlPrimitive ODate

instance OCamlType UTCTime where
  toOCamlType _ = OCamlPrimitive ODate

instance OCamlType Float where
  toOCamlType _ = OCamlPrimitive OFloat

instance OCamlType Double where
  toOCamlType _ = OCamlPrimitive OFloat

instance OCamlType Int8 where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Int16 where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Int32 where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Int64 where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Word where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Word8 where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Word16 where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Word32 where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Word64 where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Int where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Integer where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Char where
  toOCamlType _ = OCamlPrimitive OChar

instance OCamlType Bool where
  toOCamlType _ = OCamlPrimitive OBool

instance (OCamlType a, OCamlType b) => OCamlType (a, b) where
  toOCamlType _ =
    OCamlPrimitive $
    OTuple2 (toOCamlType (Proxy :: Proxy a)) (toOCamlType (Proxy :: Proxy b))

instance (OCamlType a, OCamlType b, OCamlType c) => OCamlType (a, b, c) where
  toOCamlType _ =
    OCamlPrimitive $
    OTuple3 (toOCamlType (Proxy :: Proxy a)) (toOCamlType (Proxy :: Proxy b))
            (toOCamlType (Proxy :: Proxy c))

instance (OCamlType a, OCamlType b, OCamlType c, OCamlType d) => OCamlType (a, b, c, d) where
  toOCamlType _ =
    OCamlPrimitive $
    OTuple4 (toOCamlType (Proxy :: Proxy a)) (toOCamlType (Proxy :: Proxy b))
            (toOCamlType (Proxy :: Proxy c)) (toOCamlType (Proxy :: Proxy d))

instance (OCamlType a, OCamlType b, OCamlType c, OCamlType d, OCamlType e) => OCamlType (a, b, c, d, e) where
  toOCamlType _ =
    OCamlPrimitive $
    OTuple5 (toOCamlType (Proxy :: Proxy a)) (toOCamlType (Proxy :: Proxy b))
            (toOCamlType (Proxy :: Proxy c)) (toOCamlType (Proxy :: Proxy d))
            (toOCamlType (Proxy :: Proxy e))

instance (OCamlType a, OCamlType b, OCamlType c, OCamlType d, OCamlType e, OCamlType f) => OCamlType (a, b, c, d, e, f) where
  toOCamlType _ =
    OCamlPrimitive $
    OTuple6 (toOCamlType (Proxy :: Proxy a)) (toOCamlType (Proxy :: Proxy b))
            (toOCamlType (Proxy :: Proxy c)) (toOCamlType (Proxy :: Proxy d))
            (toOCamlType (Proxy :: Proxy e)) (toOCamlType (Proxy :: Proxy f))

instance (OCamlType a) =>
         OCamlType (Proxy a) where
  toOCamlType _ = toOCamlType (undefined :: a)

{-
-- ToJSON and FromJSON instances are provided for the following types in aeson
-- not currently defined here
-- Map, LocalTime, ZonedTime, IntSet, CTime, Version, Natural
-- TimeOfDay, NominalDiffTime, Day, DiffTime, UUID, DotNetTime
-- Value, Dual, First, Last, IntMap, Tree, Seq, Vector, HashSet, Proxy
-- Const Tagged, Dual, First, Last, tuple up to length of 15
-}

-- | for any type that does not use the same serialization as Generic Aeson
--   and has a manually written OCaml definition, should manually derive OCamlType
--   using this function for convenience.
-- 
--   instance OCamlType X where
--      toOCamlType _ = typeableToOCamlType (Proxy :: Proxy X)
--

typeableToOCamlType :: forall a. Typeable a => Proxy a -> OCamlDatatype
typeableToOCamlType Proxy =
  OCamlDatatype
    (HaskellTypeMetaData aTyConName aTyConModule aTyConPackage)
    aTyConName
    (OCamlValueConstructor . NamedConstructor aTyConName $ OCamlEmpty)
  where
    aTyCon = typeRepTyCon $ typeRep (Proxy :: Proxy a)
    aTyConName = T.pack . tyConName $ aTyCon
    aTyConModule = T.pack . tyConModule $ aTyCon
    aTyConPackage = T.pack . tyConPackage $ aTyCon

-- | Used to fill the type parameters of proxy types. `Proxy :: Proxy (Maybe TypeParameterRef0)`, `Proxy :: Proxy Either TypeParameterRef0 TypeParameterRef1`. JSON representation is as an Int to simplify the automated tests.
newtype TypeParameterRef0 = TypeParameterRef0 Int deriving (Read, Show, Eq, Generic)
instance Arbitrary TypeParameterRef0 where arbitrary = TypeParameterRef0 <$> arbitrary
instance ToADTArbitrary TypeParameterRef0
instance FromJSON TypeParameterRef0
instance ToJSON TypeParameterRef0
instance OCamlType TypeParameterRef0 where
  toOCamlType _ = OCamlDatatype (HaskellTypeMetaData "a0" "OCaml.BuckleScript.Types" "ocaml-export") "a0" $ OCamlValueConstructor $ NamedConstructor "a0" $ OCamlTypeParameterRef "a0"

-- | Second unique TypeParameterRef.
newtype TypeParameterRef1 = TypeParameterRef1 Int deriving (Read, Show, Eq, Generic)
instance Arbitrary TypeParameterRef1 where arbitrary = TypeParameterRef1 <$> arbitrary
instance ToADTArbitrary TypeParameterRef1
instance FromJSON TypeParameterRef1
instance ToJSON TypeParameterRef1
instance OCamlType TypeParameterRef1 where
  toOCamlType _ = OCamlDatatype (HaskellTypeMetaData "a1" "OCaml.BuckleScript.Types" "ocaml-export") "a1" $ OCamlValueConstructor $ NamedConstructor "a1" $ OCamlTypeParameterRef "a1"

-- | Third unique TypeParameterRef.
data TypeParameterRef2 = TypeParameterRef2 Int deriving (Read, Show, Eq, Generic)
instance Arbitrary TypeParameterRef2 where arbitrary = TypeParameterRef2 <$> arbitrary
instance ToADTArbitrary TypeParameterRef2
instance FromJSON TypeParameterRef2
instance ToJSON TypeParameterRef2
instance OCamlType TypeParameterRef2 where
  toOCamlType _ = OCamlDatatype (HaskellTypeMetaData "a2" "OCaml.BuckleScript.Types" "ocaml-export") "a2" $ OCamlValueConstructor $ NamedConstructor "a2" $ OCamlTypeParameterRef "a2"

-- | Fourth unique TypeParameterRef.
data TypeParameterRef3 = TypeParameterRef3 Int deriving (Read, Show, Eq, Generic)
instance Arbitrary TypeParameterRef3 where arbitrary = TypeParameterRef3 <$> arbitrary
instance ToADTArbitrary TypeParameterRef3
instance FromJSON TypeParameterRef3
instance ToJSON TypeParameterRef3
instance OCamlType TypeParameterRef3 where
  toOCamlType _ = OCamlDatatype (HaskellTypeMetaData "a3" "OCaml.BuckleScript.Types" "ocaml-export") "a3" $ OCamlValueConstructor $ NamedConstructor "a3" $ OCamlTypeParameterRef "a3"

-- | Fifth unique TypeParameterRef.
data TypeParameterRef4 = TypeParameterRef4 Int deriving (Read, Show, Eq, Generic)
instance Arbitrary TypeParameterRef4 where arbitrary = TypeParameterRef4 <$> arbitrary
instance ToADTArbitrary TypeParameterRef4
instance FromJSON TypeParameterRef4
instance ToJSON TypeParameterRef4
instance OCamlType TypeParameterRef4 where
  toOCamlType _ = OCamlDatatype (HaskellTypeMetaData "a4" "OCaml.BuckleScript.Types" "ocaml-export") "a4" $ OCamlValueConstructor $ NamedConstructor "a4" $ OCamlTypeParameterRef "a4"

-- | Sixth unique TypeParameterRef.
data TypeParameterRef5 = TypeParameterRef5 Int deriving (Read, Show, Eq, Generic)
instance Arbitrary TypeParameterRef5 where arbitrary = TypeParameterRef5 <$> arbitrary
instance ToADTArbitrary TypeParameterRef5
instance FromJSON TypeParameterRef5
instance ToJSON TypeParameterRef5
instance OCamlType TypeParameterRef5 where
  toOCamlType _ = OCamlDatatype (HaskellTypeMetaData "a5" "OCaml.BuckleScript.Types" "ocaml-export") "a5" $ OCamlValueConstructor $ NamedConstructor "a5" $ OCamlTypeParameterRef "a5"

-- Utility functions

-- | Whether a set of constructors is an enumeration, i.e. whether they lack
--   values. data A = A | B | C would be simple data A = A Int | B | C would not
--   be simple.
isEnumeration :: OCamlConstructor -> Bool
isEnumeration (OCamlValueConstructor (NamedConstructor _ OCamlEmpty)) = True
isEnumeration (OCamlValueConstructor (MultipleConstructors cs)) = all isEnumeration (OCamlValueConstructor <$> cs)
isEnumeration _ = False


-- | Tranform an OCamlConstructor to EnumeratorConstructors 
transformToEnumeration :: OCamlConstructor -> OCamlConstructor
transformToEnumeration (OCamlValueConstructor (NamedConstructor name OCamlEmpty)) =
  OCamlEnumeratorConstructor [EnumeratorConstructor name]

transformToEnumeration (OCamlValueConstructor (MultipleConstructors cs)) =
  -- wrap cs in OCamlValueConstructor so the type matches
  -- getEnumeratorConstructor to make sure it only returns OCamlEnumeratorConstructor
  -- then concat the results and rewrap it in OCamlEnumeratorConstructor
  OCamlEnumeratorConstructor . concat . catMaybes
    $ getEnumeratorConstructor . transformToEnumeration . OCamlValueConstructor
      <$> cs
  where
    getEnumeratorConstructor constructor =
      case constructor of
        (OCamlEnumeratorConstructor c) -> Just c
        _ -> Nothing
        
transformToEnumeration cs = cs

-- | transform a OCamlConstructor to OCamlSumOfRecordConstructor
transformToSumOfRecord :: Text -> OCamlConstructor -> OCamlConstructor
transformToSumOfRecord typeName (OCamlValueConstructor value@(MultipleConstructors _cs)) = OCamlSumOfRecordConstructor typeName value
transformToSumOfRecord _ constructor = constructor   


-- | Haskell allows you to directly declare a sum of records,
-- i.e. data A = A {a :: Int} | B {b :: String}. This does not exist in
-- OCaml so we have to work around it.

isSumWithRecord :: OCamlConstructor -> Bool
isSumWithRecord (OCamlValueConstructor (MultipleConstructors cs)) =
  -- if there is only one constructor than it is not a SumWithRecords.
  -- if there are multiple constructors and at least one is a record constructor
  -- than it is a SumWithRecords
  (\x -> length x > 1 && or x) $ isSumWithRecordsAux . OCamlValueConstructor <$> cs
  where
    isSumWithRecordsAux :: OCamlConstructor -> Bool
    isSumWithRecordsAux (OCamlValueConstructor (RecordConstructor _ _)) = True
    isSumWithRecordsAux _ = False
isSumWithRecord _ = False

-- | Convert OCamlValues to the type parameter names of a data type.
--   `Either a0 a1` -> `["a0","a1"]`
getTypeParameterRefNames :: [OCamlValue] -> [Text]
getTypeParameterRefNames = nub . concat . (fmap match)
  where
    lift (OCamlDatatype _ _ constructor) = getTypeParameters constructor
    lift _ = []

    match value =
      case value of
        (OCamlRefApp typRep _) -> getTypeParameterRefNameForTypeRep typRep
        (OCamlTypeParameterRef name) -> [name]
        (Values v1 v2) -> match v1 ++ match v2
        (OCamlField _ v1) -> match v1
        (OCamlPrimitiveRef (OList v1)) -> lift v1
        (OCamlPrimitiveRef (OOption v1)) -> lift v1
        (OCamlPrimitiveRef (OTuple2 v1 v2)) -> lift v1 ++ lift v2
        (OCamlPrimitiveRef (OTuple3 v1 v2 v3)) -> lift v1 ++ lift v2 ++ lift v3
        (OCamlPrimitiveRef (OTuple4 v1 v2 v3 v4)) -> lift v1 ++ lift v2 ++ lift v3 ++ lift v4
        (OCamlPrimitiveRef (OTuple5 v1 v2 v3 v4 v5)) -> lift v1 ++ lift v2 ++ lift v3 ++ lift v4 ++ lift v5
        (OCamlPrimitiveRef (OTuple6 v1 v2 v3 v4 v5 v6)) -> lift v1 ++ lift v2 ++ lift v3 ++ lift v4 ++ lift v5 ++ lift v6
        _ -> []

-- | getOCamlValues flatten the values from MultipleConstructors into a list and remove ValueConstructor.
getOCamlValues :: ValueConstructor -> [OCamlValue]
getOCamlValues (NamedConstructor     _ value) = [value]
getOCamlValues (RecordConstructor    _ value) = [value]
getOCamlValues (MultipleConstructors cs)      = concat $ getOCamlValues <$> cs

-- | get all of the type parameters from an OCamlConstructor.
getTypeParameters :: OCamlConstructor -> [Text]
getTypeParameters (OCamlValueConstructor vc) = getTypeParameterRefNames . getOCamlValues $ vc
getTypeParameters (OCamlSumOfRecordConstructor _ vc) = getTypeParameterRefNames . getOCamlValues $ vc
getTypeParameters _ = []

-- | Matches all of the TypeParameterRefs (TypeParameterRef0 to TypeParameterRef5).
--   This function is needed to work around the tree structure for special rules for rendering type parameters.
isTypeParameterRef :: OCamlDatatype -> Bool
isTypeParameterRef (OCamlDatatype _ _ (OCamlValueConstructor (NamedConstructor _ (OCamlTypeParameterRef _)))) = True
isTypeParameterRef _ = False

-- | When there is a record that has its type parameters partially filled, it will should have TypeParameterRef0-5 as the
--   unfilled type parameters. This function properly pushes the TypeParameterRef0-5 to the type signature of an OCaml
--   type.
getTypeParameterRefNameForTypeRep :: TypeRep -> [Text]
getTypeParameterRefNameForTypeRep t =
  if length rst == 0
  then typeParamterRefText
  else typeParamterRefText <> concat (getTypeParameterRefNameForTypeRep <$> rst)
  where
  (hd,rst) = splitTyConApp $ t
  typeParamterRefText =
    case Map.lookup hd typeParameterRefTyConToOCamlTypeText of
      Just typeParamterRefText' -> [typeParamterRefText']
      Nothing -> []

-- | Make OCaml module prefix for a value based on the declaration's and parameter's meta data.
mkModulePrefix :: OCamlTypeMetaData -> OCamlTypeMetaData -> Text
mkModulePrefix (OCamlTypeMetaData _ decModules decSubModules) (OCamlTypeMetaData _ parModules parSubModules) =
  if prefix /= "" then prefix <> "." else ""
  where
    (l,r) = zipWithRightRemainder (decModules <> decSubModules) (parModules <> parSubModules)    
    prefix = T.intercalate "." $ (removeMatchingHead l) <> r

-- | Iterate through the beginning of a list, remove values as long as they are equal.
--   When one inequality is found, return the value and its tail.
removeMatchingHead :: Eq a => [(a,a)] -> [a]
removeMatchingHead [] = []
removeMatchingHead (hd:tl) =
  if fst hd == snd hd
  then removeMatchingHead tl
  else [snd hd] <> (snd <$> tl)

-- | Zip two lists. If the right hand side is longer, then return the remaining right side.
zipWithRightRemainder :: [a] -> [b] -> ([(a,b)], [b])
zipWithRightRemainder [] bs = ([], bs)
zipWithRightRemainder _ab [] = ([], [])
zipWithRightRemainder (a:as) (b:bs) = ([(a,b)], []) <> zipWithRightRemainder as bs

-- | BuckleScript has a 'float' type that conflicts when you do 'open Aeson.Decode'
--   float must be appended with 'Aeson.Decode'. 
oCamlValueIsFloat :: OCamlValue -> Bool
oCamlValueIsFloat (OCamlPrimitiveRef OFloat) = True
oCamlValueIsFloat _ = False


-- Typeable related functions
-- when a row is a type with type parameters and those type parameters are filled,
-- we need a way to extract what those type parameters are. This is not possible with
-- Generics, but it can be done with Typeable.

-- | necessary because the TypeRep for 'String' is '([], [Char])', but we want
--   it to be reduced to an OCaml 'string'.
typeRepIsString :: TypeRep -> Bool
typeRepIsString t =
  let (hd, rst) = splitTyConApp t in
  show hd == "[]" && length rst == 1 && ((show $ head rst) == "Char")

primitiveTyConToTypeRep :: Map.Map TyCon TypeRep
primitiveTyConToTypeRep = Map.fromList
  [ ( typeRepTyCon $ typeRep (Proxy :: Proxy []        ), (typeRep (Proxy :: Proxy []        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Maybe     ), (typeRep (Proxy :: Proxy Maybe        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Either    ), (typeRep (Proxy :: Proxy Either        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy ()        ), (typeRep (Proxy :: Proxy ()        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Text      ), (typeRep (Proxy :: Proxy Text        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy ByteString), (typeRep (Proxy :: Proxy ByteString        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Day       ), (typeRep (Proxy :: Proxy Day        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy UTCTime   ), (typeRep (Proxy :: Proxy UTCTime        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Float     ), (typeRep (Proxy :: Proxy Float        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Double    ), (typeRep (Proxy :: Proxy Double        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int8      ), (typeRep (Proxy :: Proxy Int8        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int16     ), (typeRep (Proxy :: Proxy Int16        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int32     ), (typeRep (Proxy :: Proxy Int32        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int64     ), (typeRep (Proxy :: Proxy Int64        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int       ), (typeRep (Proxy :: Proxy Int        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Integer   ), (typeRep (Proxy :: Proxy Integer        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word      ), (typeRep (Proxy :: Proxy Word        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word8     ), (typeRep (Proxy :: Proxy Word8        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word16    ), (typeRep (Proxy :: Proxy Word16        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word32    ), (typeRep (Proxy :: Proxy Word32        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word64    ), (typeRep (Proxy :: Proxy Word64        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Char      ), (typeRep (Proxy :: Proxy Char        )))
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Bool      ), (typeRep (Proxy :: Proxy Bool        )))
  ]


-- | match 'TyCon's (accessible from a TypeRep) to their equivalent OCaml types.
primitiveTyConToOCamlTypeText :: Map.Map TyCon Text
primitiveTyConToOCamlTypeText = Map.fromList
  [ ( typeRepTyCon $ typeRep (Proxy :: Proxy []        ), "list")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Maybe     ), "option")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Either    ), "either")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy ()        ), "unit")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Text      ), "string")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy ByteString), "string")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Day       ), "Js_date.t")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy UTCTime   ), "Js_date.t")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Float     ), "float")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Double    ), "float")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int8      ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int16     ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int32     ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int64     ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Int       ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Integer   ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word      ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word8     ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word16    ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word32    ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Word64    ), "int")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Char      ), "string")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy Bool      ), "boolean")
  ]

tupleTyConToSize :: Map.Map TyCon Int
tupleTyConToSize = Map.fromList
  [ ( typeRepTyCon $ typeRep (Proxy :: Proxy (,)      ), 2)
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy (,,)     ), 3)
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy (,,,)    ), 4)
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy (,,,,)   ), 5)
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy (,,,,,)  ), 6)
  ]

-- | match type parameter reference 'TyCon's (accessible from a TypeRep) to their equivalent OCaml types.
typeParameterRefTyConToOCamlTypeText :: Map.Map TyCon Text
typeParameterRefTyConToOCamlTypeText = Map.fromList
  [ ( typeRepTyCon $ typeRep (Proxy :: Proxy TypeParameterRef0), "a0")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy TypeParameterRef1), "a1")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy TypeParameterRef2), "a2")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy TypeParameterRef3), "a3")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy TypeParameterRef4), "a4")
  , ( typeRepTyCon $ typeRep (Proxy :: Proxy TypeParameterRef5), "a5")
  ]

typeRepToHaskellTypeMetaData :: TypeRep -> HaskellTypeMetaData
typeRepToHaskellTypeMetaData = tyConToHaskellTypeMetaData . typeRepTyCon

tyConToHaskellTypeMetaData :: TyCon -> HaskellTypeMetaData
tyConToHaskellTypeMetaData aTypeCon =
  HaskellTypeMetaData
    (T.pack . tyConName    $ aTypeCon)
    (T.pack . tyConModule  $ aTypeCon)
    (T.pack . tyConPackage $ aTypeCon)

-- isPhantomTypeParameter :: TypeRep -> Bool

ocamlDatatypeHasTypeParameter :: OCamlDatatype -> Int -> Bool
ocamlDatatypeHasTypeParameter ocamlDatatype index = ocamlDatatypeHasTypeParameter' ocamlDatatype
  where
    typeParameter = OCamlTypeParameterRef $ "a" <> (T.pack . show $ index)
    
    ocamlDatatypeHasTypeParameter' :: OCamlDatatype -> Bool
    ocamlDatatypeHasTypeParameter' (OCamlDatatype _ _ ocamlConstructor) = ocamlConstructorHasTypeParameter ocamlConstructor
    ocamlDatatypeHasTypeParameter' (OCamlPrimitive ocamlPrimitive) = ocamlPrimitiveHasTypeParameter ocamlPrimitive

    ocamlPrimitiveHasTypeParameter :: OCamlPrimitive -> Bool
    ocamlPrimitiveHasTypeParameter (OList d0) = ocamlDatatypeHasTypeParameter' d0
    ocamlPrimitiveHasTypeParameter _ = False

    ocamlConstructorHasTypeParameter :: OCamlConstructor -> Bool
    ocamlConstructorHasTypeParameter (OCamlValueConstructor valueConstructor) = valueConstructorHasTypeParameter valueConstructor
    ocamlConstructorHasTypeParameter (OCamlSumOfRecordConstructor _ valueConstructor) = valueConstructorHasTypeParameter valueConstructor
    ocamlConstructorHasTypeParameter _ = False

    valueConstructorHasTypeParameter :: ValueConstructor -> Bool
    valueConstructorHasTypeParameter (NamedConstructor _ ocamlValue) = ocamlValueHasTypeParameter ocamlValue
    valueConstructorHasTypeParameter (RecordConstructor _ ocamlValue) = ocamlValueHasTypeParameter ocamlValue
    valueConstructorHasTypeParameter (MultipleConstructors ocamlValues) = or $ valueConstructorHasTypeParameter <$> ocamlValues

    ocamlValueHasTypeParameter :: OCamlValue -> Bool
    ocamlValueHasTypeParameter (OCamlPrimitiveRef ocamlPrimitive) = ocamlPrimitiveHasTypeParameter ocamlPrimitive
    ocamlValueHasTypeParameter o@(OCamlTypeParameterRef _) = o == typeParameter
    ocamlValueHasTypeParameter (OCamlField _ v) = ocamlValueHasTypeParameter v
    ocamlValueHasTypeParameter (Values v0 v1) = ocamlValueHasTypeParameter v0 || ocamlValueHasTypeParameter v1
    ocamlValueHasTypeParameter _ = False

    
{-
hasTypeParameter''' :: OCamlValue -> OCamlPrimitive -> Bool
hasTypeParameter''' v (OList d0) = hasTypeParameter'' v d0
hasTypeParameter''' v _ = False

hasTypeParameter'' :: OCamlValue -> OCamlDatatype -> Bool
hasTypeParameter'' v (OCamlDatatype _ _ ocamlConstructor) = hasTypeParameter' v ocamlConstructor
hasTypeParameter'' v (OCamlPrimitive ocamlPrimitive) = hasTypeParameter''' v ocamlPrimitive

hasTypeParameter' :: OCamlValue -> OCamlConstructor -> Bool
hasTypeParameter' v (OCamlValueConstructor valueConstructor) = hasTypeParameter v valueConstructor
hasTypeParameter' v (OCamlSumOfRecordConstructor _ valueConstructor) = hasTypeParameter v valueConstructor
hasTypeParameter' _ _ = False

hasTypeParameter :: OCamlValue -> ValueConstructor -> Bool
hasTypeParameter vv (NamedConstructor _ v) = isOCamlValue vv v
hasTypeParameter vv (RecordConstructor _ v) = isOCamlValue vv v
hasTypeParameter vv (MultipleConstructors vs) = or $ hasTypeParameter vv <$> vs

isOCamlValue :: OCamlValue -> OCamlValue -> Bool
isOCamlValue vv (OCamlPrimitiveRef p) = hasTypeParameter''' vv p
isOCamlValue vv o@(OCamlTypeParameterRef _) = vv == o
isOCamlValue vv (OCamlField _ v) = isOCamlValue vv v
isOCamlValue vv (Values v0 v1) = isOCamlValue vv v0 || isOCamlValue vv v1
isOCamlValue _ _ = False
-}
{-
toOCamlType _ = OCamlDatatype (HaskellTypeMetaData "a0" "OCaml.BuckleScript.Types" "ocaml-export") "a0" $ OCamlValueConstructor $ NamedConstructor "a0" $ OCamlTypeParameterRef "a0"




λ> data Key a b = Key { key :: String, things :: [b] } deriving (Eq,Read,Show,Generic)
λ> data Zed b = Zed {ab :: Double, kk :: Key Int b} deriving (Eq,Read,Show,Generic)
λ> instance (OCamlType b, Typeable b) => OCamlType (Zed b)
λ> data Xed = Xed {xx :: Double, kkx :: Key Int String} deriving (Eq,Read,Show,Generic, OCamlType)

λ> toOCamlType (Proxy :: Proxy (Key TypeParameterRef0 TypeParameterRef1))

OCamlDatatype (HaskellTypeMetaData "Key" "Ghci1" "interactive") "Key" (OCamlValueConstructor (RecordConstructor "Key" (Values (OCamlField "key" (OCamlPrimitiveRef (OList (OCamlPrimitive OChar)))) (OCamlField "things" (OCamlPrimitiveRef (OList (OCamlDatatype (HaskellTypeMetaData "a1" "OCaml.BuckleScript.Types" "ocaml-export") "a1" (OCamlValueConstructor (NamedConstructor "a1" (OCamlTypeParameterRef "a1"))))))))))

λ> toOCamlType (Proxy :: Proxy (Zed TypeParameterRef0))

OCamlDatatype (HaskellTypeMetaData "Zed" "Ghci4" "interactive") "Zed" (OCamlValueConstructor (RecordConstructor "Zed" (Values (OCamlField "ab" (OCamlPrimitiveRef OFloat)) (OCamlField "kk" (OCamlRefApp (Key Int TypeParameterRef0) "Key")))))

λ> toOCamlType (Proxy :: Proxy Xed)

OCamlDatatype (HaskellTypeMetaData "Xed" "Ghci7" "interactive") "Xed" (OCamlValueConstructor (RecordConstructor "Xed" (Values (OCamlField "xx" (OCamlPrimitiveRef OFloat)) (OCamlField "kkx" (OCamlRefApp (Key Int [Char]) "Key")))))
-}
