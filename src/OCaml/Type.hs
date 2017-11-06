{-|
Module      : OCaml.Type
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

{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module OCaml.Type
  ( OCamlDatatype (..)
  , OCamlPrimitive (..)
  , OCamlConstructor (..)
  , ValueConstructor (..)
  , EnumeratorConstructor (..)
  , OCamlValue (..)
  , OCamlType (..)

  -- fill type parameters of a proxy when calling toOCamlType
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
  ) where

import           Data.Int     (Int16, Int32, Int64, Int8)
import           Data.IntMap
import           Data.List (nub)
import           Data.Map
import           Data.Maybe (catMaybes)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics
import           Prelude

-- | Top level of an OCaml datatype. A data type may be composed of
--   primitives and/or a combination of constructors and primitives.
--   OCamlDatatype is recursive via OCamlConstructor -> ValueConstructor
--   -> OCamlValue -> OCamlPrimitive -> OCamlDatatype.
data OCamlDatatype
  = OCamlDatatype Text OCamlConstructor -- ^ The name of a type and its type constructor
  | OCamlPrimitive OCamlPrimitive -- ^ A primitive value
  deriving (Show, Eq)

-- | Smallest unit of computation in OCaml.
data OCamlPrimitive
  = OInt -- ^ int
  | OBool -- ^ bool
  | OChar -- ^ char, it gets interpreted as a string because OCaml char does not support UTF-8
  | ODate -- ^ Js_date.t
  | OFloat -- ^ float
  | OString -- ^ string
  | OUnit -- ^ ()
  | OList OCamlDatatype -- ^ 'a list
  | OOption OCamlDatatype -- ^ 'a option
  | ODict OCamlPrimitive OCamlDatatype -- ^ 'a Js_dict.t
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
  | OCamlSumOfRecordConstructor Text ValueConstructor -- ^ Sum that contains at least one record. This construction is unique to Haskell. It has special Encoding and Decoding rules in order to output a valid OCaml program. i.e. `data A = A {a :: Int} | B {b :: String}`
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
  = OCamlRef Text -- ^ The name of a non-primitive data type
  | OCamlTypeParameterRef Text -- ^ Type parameters like `a` in `Maybe a`
  | OCamlEmpty -- ^ a place holder for OCaml value. It can represent the end of a list or an Enumerator in a mixed sum
  | OCamlPrimitiveRef OCamlPrimitive -- ^ A primitive OCaml type like `int`, `string`, etc.
  | OCamlField Text OCamlValue -- ^ A field name and its type from a record
  | Values OCamlValue OCamlValue -- ^ Used for multiple types in a sum type
  deriving (Show, Eq)

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
instance (Datatype d, GenericValueConstructor f) => GenericOCamlDatatype (D1 d f) where
  genericToOCamlDatatype datatype =
    OCamlDatatype
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

-- | handle type parameter
instance OCamlType a => GenericOCamlValue (Rec0 a) where
  genericToOCamlValue _ =
    case toOCamlType (Proxy :: Proxy a) of
      OCamlPrimitive primitive -> OCamlPrimitiveRef primitive
      OCamlDatatype name _     -> mkRef name
    where
      typeParameterRefs = (T.append) <$> ["a"] <*> (T.pack . show <$> ([0..5] :: [Int]))
      mkRef n
        | n `elem` typeParameterRefs = OCamlTypeParameterRef n
        | otherwise = OCamlRef n

-- OCamlType instances for primitives

instance OCamlType a => OCamlType [a] where
  toOCamlType _ = OCamlPrimitive (OList (toOCamlType (Proxy :: Proxy a)))

instance OCamlType a => OCamlType (Maybe a) where
  toOCamlType _ = OCamlPrimitive (OOption (toOCamlType (Proxy :: Proxy a)))

instance OCamlType () where
  toOCamlType _ = OCamlPrimitive OUnit

instance OCamlType Text where
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

instance (HasOCamlComparable k, OCamlType v) =>
         OCamlType (Map k v) where
  toOCamlType _ =
    OCamlPrimitive $
    ODict (toOCamlComparable (undefined :: k)) (toOCamlType (Proxy :: Proxy v))

instance (OCamlType v) =>
         OCamlType (IntMap v) where
  toOCamlType _ = OCamlPrimitive $ ODict OInt (toOCamlType (Proxy :: Proxy v))

class HasOCamlComparable a where
  toOCamlComparable :: a -> OCamlPrimitive

instance HasOCamlComparable String where
  toOCamlComparable _ = OString

instance OCamlType Int where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Integer where
  toOCamlType _ = OCamlPrimitive OInt

instance OCamlType Char where
  toOCamlType _ = OCamlPrimitive OChar

instance OCamlType Bool where
  toOCamlType _ = OCamlPrimitive OBool

{-
ToJSON and FromJSON instances are provided for the following types in aeson
-- not currently defined here
-- Word, LocalTime, ZonedTime, IntSet, CTime, Version, Natural
-- TimeOfDay, UTCTime, NominalDiffTime, Day, DiffTime, UUID, DotNetTime
-- Value, Dual, First, Last, IntMap, Tree, Seq, Vector, HashSet, Proxy
-- Const Tagged, Dual, First, Last, tuple up to length of 15
-}


-- | Used to fill the type parameters of proxy types. `Proxy :: Proxy (Maybe TypeParameterRef0)`, `Proxy :: Proxy Either TypeParameterRef0 TypeParameterRef1`.
data TypeParameterRef0 = TypeParameterRef0 deriving (Show, Eq, Generic)
instance OCamlType TypeParameterRef0 where
  toOCamlType _ = OCamlDatatype "a0" $ OCamlValueConstructor $ NamedConstructor "a0" $ OCamlTypeParameterRef "a0"

data TypeParameterRef1 = TypeParameterRef1 deriving (Show, Eq)
instance OCamlType TypeParameterRef1 where
  toOCamlType _ = OCamlDatatype "a1" $ OCamlValueConstructor $ NamedConstructor "a1" $ OCamlTypeParameterRef "a1"

data TypeParameterRef2 = TypeParameterRef2 deriving (Show, Eq)
instance OCamlType TypeParameterRef2 where
  toOCamlType _ = OCamlDatatype "a2" $ OCamlValueConstructor $ NamedConstructor "a2" $ OCamlTypeParameterRef "a2"

data TypeParameterRef3 = TypeParameterRef3 deriving (Show, Eq)
instance OCamlType TypeParameterRef3 where
  toOCamlType _ = OCamlDatatype "a3" $ OCamlValueConstructor $ NamedConstructor "a3" $ OCamlTypeParameterRef "a3"

data TypeParameterRef4 = TypeParameterRef4 deriving (Show, Eq)
instance OCamlType TypeParameterRef4 where
  toOCamlType _ = OCamlDatatype "a4" $ OCamlValueConstructor $ NamedConstructor "a4" $ OCamlTypeParameterRef "a4"

data TypeParameterRef5 = TypeParameterRef5 deriving (Show, Eq)
instance OCamlType TypeParameterRef5 where
  toOCamlType _ = OCamlDatatype "a5" $ OCamlValueConstructor $ NamedConstructor "a5" $ OCamlTypeParameterRef "a5"

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
    lift (OCamlDatatype _ constructor) = getTypeParameters constructor
    lift _ = []

    match value =
      case value of
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

-- | 
getTypeParameters :: OCamlConstructor -> [Text]
getTypeParameters (OCamlValueConstructor vc) = getTypeParameterRefNames . getOCamlValues $ vc
getTypeParameters (OCamlSumOfRecordConstructor _ vc) = getTypeParameterRefNames . getOCamlValues $ vc
getTypeParameters _ = []

-- | Matches all of the TypeParameterRefs (TypeParameterRef0 to TypeParameterRef5).
--   This function is needed to work around the tree structure for special rules for rendering type parameters.
isTypeParameterRef :: OCamlDatatype -> Bool
isTypeParameterRef (OCamlDatatype _ (OCamlValueConstructor (NamedConstructor _ (OCamlTypeParameterRef _)))) = True
isTypeParameterRef _ = False
