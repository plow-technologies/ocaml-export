{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE GADTs              #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Shared.Types
  ( Key(..)
  , IsKey(..)
  , Entity(..)
  , Todo(..)
  , TodoId(..)
  , EntityTodo
  , User(..)
  , UserId(..)
  , Username(..)
  , EntityUser
  , API
  ) where

import Data.Aeson
import Data.Text
import Data.Time
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Servant.API
import Web.HttpApiData

newtype Key = Key Word64 deriving (Eq, Generic, Ord, Read, Show, Typeable)

instance ToJSON Key
instance FromJSON Key
instance FromHttpApiData Key where
  parseUrlPiece = fmap Key . parseUrlPiece

class IsKey a where
  fromKey :: Key -> a
  toKey :: a -> Key

data Entity a b where
  Entity :: IsKey a =>
    { entityKey :: a
    , entityVal :: b
    } -> Entity a b

deriving instance (Read a, IsKey a, Read b) => Read (Entity a b)
deriving instance (Show a, Show b) => Show (Entity a b)
deriving instance (Ord a, Ord b)           => Ord (Entity a b)
deriving instance (Typeable a, Typeable b) => Typeable (Entity a b)

instance (Eq a, Eq b) => Eq (Entity a b) where
  (==) (Entity a b) (Entity a' b') = a == a' && b == b'

instance (ToJSON a, ToJSON b) => ToJSON (Entity a b) where
  toJSON (Entity a b) =
    object
      [ "key"   .= a
      , "value" .= b
      ]

instance (IsKey a, FromJSON a, FromJSON b) => FromJSON (Entity a b) where
  parseJSON = withObject "Entity" $ \o ->
    Entity
      <$> o .: "key"
      <*> o .: "value"

newtype Username = Username Text deriving (Eq, Read, Show, Generic)

instance ToJSON Username
instance FromJSON Username
instance FromHttpApiData Username where
  parseUrlPiece = fmap Username . parseUrlPiece

newtype UserId = UserId Key deriving (Eq, Read, Show, Generic)

instance ToJSON UserId
instance FromJSON UserId
instance FromHttpApiData UserId where
  parseUrlPiece = fmap UserId . parseUrlPiece

instance IsKey UserId where
  fromKey key = (UserId key)
  toKey (UserId key) = key

data User =
  User
    { username :: Username
    , password :: Text
    } deriving (Eq, Read, Show, Generic)

instance ToJSON User
instance FromJSON User

type EntityUser = Entity UserId User

newtype TodoId = TodoId Key deriving (Eq, Read, Show, Generic)

instance ToJSON TodoId
instance FromJSON TodoId
instance FromHttpApiData TodoId where
  parseUrlPiece = fmap TodoId . parseUrlPiece

instance IsKey TodoId where
  fromKey key = (TodoId key)
  toKey (TodoId key) = key

data Todo =
  Todo
    { description :: Text
    , completed   :: Bool
    , created     :: UTCTime
    , madeBy      :: UserId
    } deriving (Eq, Read, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

type EntityTodo = Entity TodoId Todo


  
type API =
       "todo"  :> Capture "userid" UserId :> ReqBody '[JSON] Todo :> Post '[JSON] EntityTodo
  :<|> "todos" :> Capture "userid" UserId :> Get '[JSON] [EntityTodo]
  :<|> "user"  :> ReqBody '[JSON] User :> Post '[JSON] EntityUser
  :<|> "users" :> Get '[JSON] [EntityUser]
  :<|> Raw
