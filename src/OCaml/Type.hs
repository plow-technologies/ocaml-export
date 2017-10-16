{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module OCaml.Type where

import           Data.Char    (isLower)
import           Data.Int     (Int16, Int32, Int64, Int8)
import           Data.IntMap
import           Data.Map
import           Data.Maybe (catMaybes)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics
import           Prelude

import Unsafe.Coerce (unsafeCoerce)

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
  | OList OCamlDatatype -- ... list
  | OOption OCamlDatatype -- ... option
  | ODict OCamlPrimitive OCamlDatatype -- Js_dict
  | OTuple2 OCamlDatatype OCamlDatatype -- (*)
  | OTuple3 OCamlDatatype OCamlDatatype OCamlDatatype -- (**)
  | OTuple4 OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype -- (***)
  | OTuple5 OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype -- (****)
  | OTuple6 OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype OCamlDatatype -- (*****)
  | OTypeParameterRef Text
  deriving (Show, Eq)

-- | OCamlConstructor bridges all Haskell type declarations to OCaml type declaractions.
data OCamlConstructor 
  = OCamlValueConstructor ValueConstructor
  | OCamlEnumeratorConstructor [EnumeratorConstructor]
  | OCamlSumOfRecordConstructor ValueConstructor
  deriving (Show, Eq)

-- | Common type declarations found in Haskell and OCaml.
data ValueConstructor
  = NamedConstructor Text OCamlValue
  | RecordConstructor Text OCamlValue
  | MultipleConstructors [ValueConstructor]
  deriving (Show, Eq)

-- | Special type of constructor. Names but no values. Accounts
--   for the way aeson handles Enumerations.   
data EnumeratorConstructor
  = EnumeratorConstructor Text
  deriving (Show, Eq)

data OCamlValue
  = OCamlRef Text
  | OCamlTypeParameterRef Text
  | OCamlEmpty
  | OCamlPrimitiveRef OCamlPrimitive
  | Values OCamlValue OCamlValue
  | OCamlField Text OCamlValue
  deriving (Show, Eq)

data TypeParameterRef0 = TypeParameterRef0 deriving (Show, Eq, Generic)
instance OCamlType TypeParameterRef0 where
  toOCamlType _ = OCamlPrimitive (OTypeParameterRef "TypeParameterRef0")
{-
class OCamlType a where
  toOCamlType :: a -> OCamlDatatype
  toOCamlType = genericToOCamlDatatype . from
  default toOCamlType :: (Generic a, GenericOCamlDatatype (Rep a)) =>
    a -> OCamlDatatype

-}

data TypeParameterRef1 = TypeParameterRef1 deriving (Show, Eq)
data TypeParameterRef2 = TypeParameterRef2 deriving (Show, Eq)

class OCamlTypeParameter (a :: * -> *) where
  toOCamlTypeParameter :: a b -> String
  toOCamlTypeParameter = genericToOCamlTypeParameter . from1
  default toOCamlTypeParameter :: (Generic1 a, GenericOCamlTypeParameter (Rep1 a)) => a b -> String

class GenericOCamlTypeParameter a where
  genericToOCamlTypeParameter :: a b -> String

instance (GenericOCamlTypeParameter f) => GenericOCamlTypeParameter (D1 d f) where
  genericToOCamlTypeParameter d = genericToOCamlTypeParameter (unM1 d)

instance (GenericOCamlTypeParameter f) => GenericOCamlTypeParameter (M1 C k (S1 d f)) where
  genericToOCamlTypeParameter d = genericToOCamlTypeParameter (unM1 d)

instance (GenericOCamlTypeParameter f) => GenericOCamlTypeParameter (S1 d f) where
  genericToOCamlTypeParameter (M1 d) = genericToOCamlTypeParameter d

instance GenericOCamlTypeParameter Par1 where
  genericToOCamlTypeParameter a = unsafeCoerce a
{-
    D1 ('MetaData "Tree" "Main" "package-name" 'False)
      (C1 ('MetaCons "Leaf" 'PrefixI 'False)
         (S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
               Par1)
       :+:
       C1 ('MetaCons "Node" 'PrefixI 'False)
         (S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
               (Rec1 Tree)
          :*:
          S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
               (Rec1 Tree)))


class Functor f where
  fmap :: (a -> b) -> f a -> f b
  default fmap :: (Generic1 f, Functor (Rep1 f)) => (a -> b) -> f a -> f b
  fmap = to1 . fmap . from1

instance Functor Par1 where
  fmap f (Par1 p) = Par1 (f p)
-}

------------------------------------------------------------
class OCamlType a where
  toOCamlType :: a -> OCamlDatatype
  toOCamlType = genericToOCamlDatatype . from
  default toOCamlType :: (Generic a, GenericOCamlDatatype (Rep a)) =>
    a -> OCamlDatatype

------------------------------------------------------------
class GenericOCamlDatatype f where
  genericToOCamlDatatype :: f a -> OCamlDatatype

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
              then transformToSumOfRecord ocamlConstructor
              else ocamlConstructor

--   | OCamlSumOfRecordConstructor ValueConstructor               
------------------------------------------------------------
class GenericValueConstructor f where
  genericToValueConstructor :: f a -> ValueConstructor

instance (Constructor c, GenericOCamlValue f) => GenericValueConstructor (C1 c f) where
  genericToValueConstructor constructor =
    if conIsRecord constructor
      then RecordConstructor name (genericToOCamlValue (unM1 constructor))
      else NamedConstructor name (genericToOCamlValue (unM1 constructor))
    where
      name = T.pack $ conName constructor

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
      OCamlPrimitive primitive -> mkRef primitive
      OCamlDatatype name _     -> OCamlRef name
    where
      mkRef p =
        case p of
          OTypeParameterRef t -> OCamlTypeParameterRef t
          _ -> OCamlPrimitiveRef p
          
{-
      OCamlDatatype name _     -> if isLowercase name then OCamlTypeParameterRef name else OCamlRef name
    where
      isLowercase = isLower . head . T.unpack
-}
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
isEnumeration (OCamlValueConstructor (NamedConstructor _ OCamlEmpty)) = True
isEnumeration (OCamlValueConstructor (MultipleConstructors cs)) = all isEnumeration (OCamlValueConstructor <$> cs)
isEnumeration _ = False


-- | Tranform a complete OCamlConstructor to EnumeratorConstructors 
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
        
transformToEnumeration _ = undefined

transformToSumOfRecord :: OCamlConstructor -> OCamlConstructor
transformToSumOfRecord (OCamlValueConstructor value@(MultipleConstructors _cs)) = OCamlSumOfRecordConstructor value
transformToSumOfRecord constructor = constructor   


-- | Haskell allows you to directly declare a sum of records,
-- i.e. data A = A {a :: Int} | B {b :: String}. This does not exist in
-- OCaml so we have to work around it.

isSumWithRecord :: OCamlConstructor -> Bool
isSumWithRecord (OCamlValueConstructor (MultipleConstructors cs)) =
  -- if there is only one constructor than it is not a SumWithRecords.
  -- if there are multiple constructors and at least one is a record constructor
  -- than it is a SumWithRecords
  (\x -> length x > 1 && or x) $ isSumWithRecordsAux . OCamlValueConstructor <$> cs
isSumWithRecord _ = False

isSumWithRecordsAux :: OCamlConstructor -> Bool
isSumWithRecordsAux (OCamlValueConstructor (RecordConstructor _ _)) = True
isSumWithRecordsAux _ = False
