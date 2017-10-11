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


data ReasonDatatype
  = ReasonDatatype Text ReasonConstructor
  | ReasonPrimitive ReasonPrimitive
  deriving (Show, Eq)

data ReasonPrimitive
  = RInt -- int (Int32 or JS.Int)
  | RBool -- bool
  | RChar -- char, Char doesn't support Unicode or UTF-8, better to use string
  | RDate -- Js.Date
  | RFloat -- Js.Float
  | RString -- string
  | RUnit -- ()
  | RList ReasonDatatype -- list
  | RMaybe ReasonDatatype -- option (None,Some)
  | RDict ReasonPrimitive ReasonDatatype
  | RTuple2 ReasonDatatype ReasonDatatype -- (,)
  | RTuple3 ReasonDatatype ReasonDatatype ReasonDatatype -- (,,)
  | RTuple4 ReasonDatatype ReasonDatatype ReasonDatatype ReasonDatatype -- (,,,)
  | RTuple5 ReasonDatatype ReasonDatatype ReasonDatatype ReasonDatatype ReasonDatatype -- (,,,,)
  | RTuple6 ReasonDatatype ReasonDatatype ReasonDatatype ReasonDatatype ReasonDatatype ReasonDatatype -- (,,,,,)
  deriving (Show, Eq)

data ReasonConstructor
  = NamedConstructor Text ReasonValue
  | RecordConstructor Text ReasonValue
  | MultipleConstructors [ReasonConstructor]
  deriving (Show, Eq)

data ReasonValue
  = ReasonRef Text
  | ReasonEmpty
  | ReasonPrimitiveRef ReasonPrimitive
  | Values ReasonValue ReasonValue
  | ReasonField Text ReasonValue
  deriving (Show, Eq)

------------------------------------------------------------
class ReasonType a where
  toReasonType :: a -> ReasonDatatype
  toReasonType = genericToReasonDatatype . from
  default toReasonType :: (Generic a, GenericReasonDatatype (Rep a)) =>
    a -> ReasonDatatype

------------------------------------------------------------
class GenericReasonDatatype f where
  genericToReasonDatatype :: f a -> ReasonDatatype

instance (Datatype d, GenericReasonConstructor f) => GenericReasonDatatype (D1 d f) where
  genericToReasonDatatype datatype =
    ReasonDatatype
      (T.pack (datatypeName datatype))
      (genericToReasonConstructor (unM1 datatype))

-- ------------------------------------------------------------
class GenericReasonConstructor f where
  genericToReasonConstructor :: f a -> ReasonConstructor

instance (Constructor c, GenericReasonValue f) => GenericReasonConstructor (C1 c f) where
  genericToReasonConstructor constructor =
    if conIsRecord constructor
      then RecordConstructor name (genericToReasonValue (unM1 constructor))
      else NamedConstructor name (genericToReasonValue (unM1 constructor))
    where
      name = T.pack $ conName constructor

instance (GenericReasonConstructor f, GenericReasonConstructor g) =>
         GenericReasonConstructor (f :+: g) where
  genericToReasonConstructor _ =
    MultipleConstructors
      [ genericToReasonConstructor (undefined :: f p)
      , genericToReasonConstructor (undefined :: g p)
      ]

------------------------------------------------------------
class GenericReasonValue f where
  genericToReasonValue :: f a -> ReasonValue

instance (Selector s, GenericReasonValue a) =>
         GenericReasonValue (S1 s a) where
  genericToReasonValue selector =
    case selName selector of
      ""   -> genericToReasonValue (undefined :: a p)
      name -> ReasonField (T.pack name) (genericToReasonValue (undefined :: a p))

instance (GenericReasonValue f, GenericReasonValue g) =>
         GenericReasonValue (f :*: g) where
  genericToReasonValue _ =
    Values
      (genericToReasonValue (undefined :: f p))
      (genericToReasonValue (undefined :: g p))

instance GenericReasonValue U1 where
  genericToReasonValue _ = ReasonEmpty

instance ReasonType a => GenericReasonValue (Rec0 a) where
  genericToReasonValue _ =
    case toReasonType (Proxy :: Proxy a) of
      ReasonPrimitive primitive -> ReasonPrimitiveRef primitive
      ReasonDatatype name _     -> ReasonRef name

instance ReasonType a => ReasonType [a] where
  toReasonType _ = ReasonPrimitive (RList (toReasonType (Proxy :: Proxy a)))

instance ReasonType a => ReasonType (Maybe a) where
  toReasonType _ = ReasonPrimitive (RMaybe (toReasonType (Proxy :: Proxy a)))

instance ReasonType () where
  toReasonType _ = ReasonPrimitive RUnit

instance ReasonType Text where
  toReasonType _ = ReasonPrimitive RString

instance ReasonType Day where
  toReasonType _ = ReasonPrimitive RDate

instance ReasonType UTCTime where
  toReasonType _ = ReasonPrimitive RDate

instance ReasonType Float where
  toReasonType _ = ReasonPrimitive RFloat

instance ReasonType Double where
  toReasonType _ = ReasonPrimitive RFloat

instance ReasonType Int8 where
  toReasonType _ = ReasonPrimitive RInt

instance ReasonType Int16 where
  toReasonType _ = ReasonPrimitive RInt

instance ReasonType Int32 where
  toReasonType _ = ReasonPrimitive RInt

instance ReasonType Int64 where
  toReasonType _ = ReasonPrimitive RInt

instance (ReasonType a, ReasonType b) => ReasonType (a, b) where
  toReasonType _ =
    ReasonPrimitive $
    RTuple2 (toReasonType (Proxy :: Proxy a)) (toReasonType (Proxy :: Proxy b))

instance (ReasonType a, ReasonType b, ReasonType c) => ReasonType (a, b, c) where
  toReasonType _ =
    ReasonPrimitive $
    RTuple3 (toReasonType (Proxy :: Proxy a)) (toReasonType (Proxy :: Proxy b))
            (toReasonType (Proxy :: Proxy c))

instance (ReasonType a, ReasonType b, ReasonType c, ReasonType d) => ReasonType (a, b, c, d) where
  toReasonType _ =
    ReasonPrimitive $
    RTuple4 (toReasonType (Proxy :: Proxy a)) (toReasonType (Proxy :: Proxy b))
            (toReasonType (Proxy :: Proxy c)) (toReasonType (Proxy :: Proxy d))

instance (ReasonType a, ReasonType b, ReasonType c, ReasonType d, ReasonType e) => ReasonType (a, b, c, d, e) where
  toReasonType _ =
    ReasonPrimitive $
    RTuple5 (toReasonType (Proxy :: Proxy a)) (toReasonType (Proxy :: Proxy b))
            (toReasonType (Proxy :: Proxy c)) (toReasonType (Proxy :: Proxy d))
            (toReasonType (Proxy :: Proxy e))

instance (ReasonType a, ReasonType b, ReasonType c, ReasonType d, ReasonType e, ReasonType f) => ReasonType (a, b, c, d, e, f) where
  toReasonType _ =
    ReasonPrimitive $
    RTuple6 (toReasonType (Proxy :: Proxy a)) (toReasonType (Proxy :: Proxy b))
            (toReasonType (Proxy :: Proxy c)) (toReasonType (Proxy :: Proxy d))
            (toReasonType (Proxy :: Proxy e)) (toReasonType (Proxy :: Proxy f))


instance (ReasonType a) =>
         ReasonType (Proxy a) where
  toReasonType _ = toReasonType (undefined :: a)

instance (HasReasonComparable k, ReasonType v) =>
         ReasonType (Map k v) where
  toReasonType _ =
    ReasonPrimitive $
    RDict (toReasonComparable (undefined :: k)) (toReasonType (Proxy :: Proxy v))

instance (ReasonType v) =>
         ReasonType (IntMap v) where
  toReasonType _ = ReasonPrimitive $ RDict RInt (toReasonType (Proxy :: Proxy v))

class HasReasonComparable a where
  toReasonComparable :: a -> ReasonPrimitive

instance HasReasonComparable String where
  toReasonComparable _ = RString

instance ReasonType Int where
  toReasonType _ = ReasonPrimitive RInt

instance ReasonType Char where
  toReasonType _ = ReasonPrimitive RChar

instance ReasonType Bool where
  toReasonType _ = ReasonPrimitive RBool

-- | Whether a set of constructors is an enumeration, i.e. whether they lack
--   values. data A = A | B | C would be simple data A = A Int | B | C would not
--   be simple.
isEnumeration :: ReasonConstructor -> Bool
isEnumeration (NamedConstructor _ ReasonEmpty) = True
isEnumeration (MultipleConstructors cs) = all isEnumeration cs
isEnumeration _ = False
