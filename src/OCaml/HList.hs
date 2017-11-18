{-# LANGUAGE DataKinds, TypeOperators, GADTs, MultiParamTypeClasses, PolyKinds, ScopedTypeVariables, TypeFamilies, RankNTypes, FlexibleInstances #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}

module OCaml.HList
  ( (:>)
  , (:<|>)(..)
  , HasType (..) 
  ) where

-- import Data.Nat
import Data.Proxy

import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Typeable

import OCaml.Record
import OCaml.Type

import Data.Text

data (path :: k) :> a
    deriving (Typeable)
infixr 4 :>

data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)

infixr 3 :<|>

instance (Semigroup a, Semigroup b) => Semigroup (a :<|> b) where
  (a :<|> b) <> (a' :<|> b') = (a <> a') :<|> (b <> b')

instance (Monoid a, Monoid b) => Monoid (a :<|> b) where
  mempty = mempty :<|> mempty
  (a :<|> b) `mappend` (a' :<|> b') = (a `mappend` a') :<|> (b `mappend` b')



{-
class SecretClass a where 
    foo :: a
    bar :: a -> a -> a

class SecretClass a => PublicClass a

instance SecretClass Int where 
    foo = 3
    bar = (+) 
-}

class HasType api where
  mkType :: Proxy api -> Text

-- class HasType a => PublicClass a

instance (HasType a, HasType b) => HasType (a :> b) where
  mkType Proxy = mkType (Proxy :: Proxy a) <> mkType (Proxy :: Proxy b)

instance {-# OVERLAPPABLE #-} OCamlType a => HasType a where
  mkType = toOCamlTypeRef

-- instance HasType a => PublicClass a

-- class API (xs :: [*]) where
--  zed ::
{-
type family ABC (xs :: [*]) where
  InAndOutListWithRouteNamesAPI (a ': '[]) = InAndOutListWithRouteNames a b
  InAndOutListWithRouteNamesAPI (a ': as)  = (InAndOutListWithRouteNames a b) :<|> InAndOutListWithRouteNamesAPI as bs
-}

{-
type family (HasType a) :: Text where
  F Char  = 'True
  F a     = 'False
-}

--type family (HasType a) :: Text where
--  HasType (a :> b)  = toOCamlTypeRef a <> HasType b
--  HasType a     = toOCamlTypeRef a

{-
class HasType api where
--  type TypeT api :: *

  mkType :: Proxy api -> Text

instance (HasType a, HasType b) => HasType (a :> b) where

--  type TypeT (a :> b) = String

  mkType Proxy = mkType (Proxy :: Proxy a) <> mkType (Proxy :: Proxy b)
   -- where pa = Proxy :: Proxy a
     --     pb = Proxy :: Proxy b

-- instance (OCamlType Char) => HasType Char where
instance {-# OVERLAPPING #-} OCamlType a => HasType a where
  mkType = toOCamlTypeRef
-}


{-
instance HasType Char where
  mkType a = toOCamlTypeRef a

instance HasType Int where
  mkType a = toOCamlTypeRef a
-}

{-
instance (KnownSymbol sym, FromHttpApiData a, HasServer api context)
      => HasServer (Header sym a :> api) context where

  type ServerT (Header sym a :> api) m =
    Maybe a -> ServerT api m

hoistServerWithContext _ pc nt s = h


tyConName

class HasServer api context where
  type ServerT api (m :: * -> *) :: *

  route ::
       Proxy api
    -> Context context
    -> Delayed env (Server api)
    -> Router env

  hoistServerWithContext
      :: Proxy api
      -> Proxy context
      -> (forall x. m x -> n x)
      -> ServerT api m
      -> ServerT api n

instance (HasServer a context, HasServer b context) => HasServer (a :<|> b) context where

  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy context server = choice (route pa context ((\ (a :<|> _) -> a) <$> server))
                                      (route pb context ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

  -- | This is better than 'enter', as it's tailor made for 'HasServer'.
  hoistServerWithContext _ pc nt (a :<|> b) =
    hoistServerWithContext (Proxy :: Proxy a) pc nt a :<|>
    hoistServerWithContext (Proxy :: Proxy b) pc nt b
-}

data HList as where
  HNil  :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

infixr 6 `HCons`

{-
class HNth as n where
  hnth :: HList as -> Proxy n -> as :!: n
  
instance HNth as n => HNth (b ': as) ('NS n) where
  hnth (HCons _ as) _ = hnth as (Proxy :: Proxy n)

instance HNth (a ': as) 'NZ where
  hnth (HCons a _) _ = a
-}
type family Map f as where
  Map f '[] = '[]
  Map f (a ': as) = f a ': Map f as

class HMap (as :: [k]) (f :: k -> *) (g :: k -> *) where
  hmap :: Proxy as -> (forall (i :: k). f i -> g i) -> HList (Map f as) -> HList (Map g as)
instance HMap '[] f g where
  hmap _ _ HNil = HNil
instance HMap as f g => HMap (a ': as) f g where
  hmap _ f (HCons a as) = HCons (f a) (hmap (Proxy :: Proxy as) f as)

{-
class HLookup n as where
  hlookup :: Proxy n -> Proxy as -> HList as -> as :!: n
instance HLookup NZ (a ': as) where
  hlookup _ _ (HCons f _) = f
instance HLookup n as => HLookup (NS n) (a ': as) where
  hlookup _ _ (HCons _ fs) = hlookup (Proxy :: Proxy n) (Proxy :: Proxy as) fs
-}
