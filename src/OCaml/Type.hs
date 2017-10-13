{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module OCaml.Type where

import           Data.Int     (Int16, Int32, Int64, Int8)
import           Data.IntMap
import           Data.Map
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics
import           Prelude


data OCamlDatatype
  = OCamlDatatype Text OCamlConstructor
  | OCamlPrimitive OCamlPrimitive
  deriving (Show, Eq)

data OCamlPrimitive
  = OInt -- int (Int32 or JS.Int)
  | OBool -- bool
  | OChar -- char, Char doesn't support Unicode or UTF-8, better to use string
  | ODate -- Js.Date
  | OFloat -- Js.Float
  | OString -- string
  | OUnit -- ()
  | OList OCamlDatatype -- list
  | OOption OCamlDatatype -- option (None,Some)
  | ODict OCamlPrimitive OCamlDatatype
  | OTuple2 OCamlDatatype OCamlDatatype -- (,)
  | OTuple3 OCamlDatatype OCamlDatatype OCamlDatatype -- (,,)
  | OTuple4 OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype -- (,,,)
  | OTuple5 OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype -- (,,,,)
  | OTuple6 OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype -- (,,,,,)
  deriving (Show, Eq)

-- ValueConstructor
data OCamlConstructor 
  = OCamlConstructor OCamlConstructorAux
  | EnumeratorConstructors [OCamlEnumerator]
  deriving (Show, Eq)
-- ValueConstructorAux?
data OCamlConstructorAux
  = NamedConstructor Text OCamlValue
  | RecordConstructor Text OCamlValue
  | MultipleConstructors [OCamlConstructor]
  deriving (Show, Eq)

-- set of named members of a set
-- | Special type of constructor. Names but no values.
data OCamlEnumerator
  = OCamlEnumerator Text
  deriving (Show, Eq)

data OCamlValue
  = OCamlRef Text
  | OCamlEmpty
  | OCamlPrimitiveRef OCamlPrimitive
  | Values OCamlValue OCamlValue
  | OCamlField Text OCamlValue
  deriving (Show, Eq)

------------------------------------------------------------
class OCamlType a where
  toOCamlType :: a -> OCamlDatatype
  toOCamlType = genericToOCamlDatatype . from
  default toOCamlType :: (Generic a, GenericOCamlDatatype (Rep a)) =>
    a -> OCamlDatatype

------------------------------------------------------------
class GenericOCamlDatatype f where
  genericToOCamlDatatype :: f a -> OCamlDatatype

instance (Datatype d, GenericOCamlConstructor f) => GenericOCamlDatatype (D1 d f) where
  genericToOCamlDatatype datatype =
    OCamlDatatype
      (T.pack (datatypeName datatype))
      (transform (genericToOCamlConstructor (unM1 datatype)))
    where
      transform ocamlConstructor =
        if isEnumeration ocamlConstructor
          then transformToEnumeration ocamlConstructor
          else ocamlConstructor               
-- ------------------------------------------------------------
class GenericOCamlConstructor f where
  genericToOCamlConstructor :: f a -> OCamlConstructor

instance (Constructor c, GenericOCamlValue f) => GenericOCamlConstructor (C1 c f) where
  genericToOCamlConstructor constructor =
    if conIsRecord constructor
      then OCamlConstructor $ RecordConstructor name (genericToOCamlValue (unM1 constructor))
      else OCamlConstructor $ NamedConstructor name (genericToOCamlValue (unM1 constructor))
    where
      name = T.pack $ conName constructor

instance (GenericOCamlConstructor f, GenericOCamlConstructor g) =>
         GenericOCamlConstructor (f :+: g) where
  genericToOCamlConstructor _ =
    OCamlConstructor $ MultipleConstructors
      [ genericToOCamlConstructor (undefined :: f p)
      , genericToOCamlConstructor (undefined :: g p)
      ]

------------------------------------------------------------
class GenericOCamlValue f where
  genericToOCamlValue :: f a -> OCamlValue

instance (Selector s, GenericOCamlValue a) =>
         GenericOCamlValue (S1 s a) where
  genericToOCamlValue selector =
    case selName selector of
      ""   -> genericToOCamlValue (undefined :: a p)
      name -> OCamlField (T.pack name) (genericToOCamlValue (undefined :: a p))

instance (GenericOCamlValue f, GenericOCamlValue g) =>
         GenericOCamlValue (f :*: g) where
  genericToOCamlValue _ =
    Values
      (genericToOCamlValue (undefined :: f p))
      (genericToOCamlValue (undefined :: g p))

instance GenericOCamlValue U1 where
  genericToOCamlValue _ = OCamlEmpty

instance OCamlType a => GenericOCamlValue (Rec0 a) where
  genericToOCamlValue _ =
    case toOCamlType (Proxy :: Proxy a) of
      OCamlPrimitive primitive -> OCamlPrimitiveRef primitive
      OCamlDatatype name _     -> OCamlRef name

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

instance OCamlType Char where
  toOCamlType _ = OCamlPrimitive OChar

instance OCamlType Bool where
  toOCamlType _ = OCamlPrimitive OBool

-- | Whether a set of constructors is an enumeration, i.e. whether they lack
--   values. data A = A | B | C would be simple data A = A Int | B | C would not
--   be simple.
isEnumeration :: OCamlConstructor -> Bool
isEnumeration (OCamlConstructor (NamedConstructor _ OCamlEmpty)) = True
isEnumeration (OCamlConstructor (MultipleConstructors cs)) = all isEnumeration cs
isEnumeration _ = False

-- | Tranform a complete OCamlConstructor to EnumeratorConstructors 
transformToEnumeration :: OCamlConstructor -> OCamlConstructor
transformToEnumeration (OCamlConstructor (NamedConstructor name OCamlEmpty)) = EnumeratorConstructors [OCamlEnumerator name]
transformToEnumeration (OCamlConstructor (MultipleConstructors cs)) = EnumeratorConstructors $ concat $ (\(EnumeratorConstructors es) -> es) <$> transformToEnumeration <$> cs
transformToEnumeration _ = undefined

-- | Haskell allows you to directly declare a sum of records,
-- i.e. data A = A {a :: Int} | B {b :: String}. This does not exist in
-- OCaml so we have to work around it.

isSumWithRecords :: OCamlConstructor -> Bool
isSumWithRecords (OCamlConstructor (MultipleConstructors cs)) = (\x -> length x > 1 && or x) $ isSumWithRecordsAux <$> cs
isSumWithRecords _ = False

isSumWithRecordsAux :: OCamlConstructor -> Bool
isSumWithRecordsAux (OCamlConstructor (RecordConstructor _ _)) = True
isSumWithRecordsAux _ = False
