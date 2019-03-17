{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}

module Shared.Types.Reason.Types where

import Data.Time.Clock.POSIX
import Data.Time
import qualified Data.Text as T
import OCaml.Export
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

import Shared.Types
  (Entity(..), IsKey(..), Key(..), Todo(..), TodoId(..), User(..), UserId(..), Username(..))

-- because of the restriction we put on Entity
-- we have to make this dummy instance
instance IsKey TypeParameterRef0 where
  fromKey (Key k) = TypeParameterRef0 (fromIntegral k)
  toKey (TypeParameterRef0 k) = Key (fromIntegral k)

type SharedTypesPackage =
  OCamlPackage "shared-types" '[] :> 
    (OCamlModule '["SharedTypes"]
    :> OCamlTypeInFile (Entity TypeParameterRef0 TypeParameterRef1) "handwritten/Entity"
    :> Key
    :> TodoId
    :> UserId
    :> Username
    :> Todo
    :> User
    )

instance (IsKey a, Arbitrary a, Arbitrary b) => Arbitrary (Entity a b) where
  arbitrary = Entity <$> arbitrary <*> arbitrary

instance (IsKey a, Arbitrary a, ToADTArbitrary a, Arbitrary b, ToADTArbitrary b) => ToADTArbitrary (Entity a b) where
  toADTArbitrarySingleton Proxy =
    ADTArbitrarySingleton "Shared.Types" "Entity"
      <$> oneof
        [ ConstructorArbitraryPair "Entity" <$> (Entity <$> arbitrary <*> arbitrary)
        ]

  toADTArbitrary Proxy =
    ADTArbitrary "Shared.Types" "Entity"
      <$> sequence
        [ ConstructorArbitraryPair "Entity" <$> (Entity <$> arbitrary <*> arbitrary) ]

instance OCamlType (Entity TypeParameterRef0 TypeParameterRef1) where
  toOCamlType _ = typeableToOCamlType (Proxy :: Proxy (Entity TypeParameterRef0 TypeParameterRef1))

instance Arbitrary Key where
  arbitrary = Key <$> arbitrary
instance ToADTArbitrary Key
instance OCamlType Key

instance Arbitrary TodoId where
  arbitrary = TodoId <$> arbitrary
instance ToADTArbitrary TodoId
instance OCamlType TodoId

instance Arbitrary Todo where
  arbitrary = Todo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance ToADTArbitrary Todo
instance OCamlType Todo

instance Arbitrary UserId where
  arbitrary = UserId <$> arbitrary
instance ToADTArbitrary UserId
instance OCamlType UserId

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary
instance ToADTArbitrary User
instance OCamlType User

instance Arbitrary Username where
  arbitrary = Username <$> arbitrary
instance ToADTArbitrary Username
instance OCamlType Username








instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . fromIntegral <$> (arbitrary :: Gen Integer)
