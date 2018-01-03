{-|
Module      : OCaml.BuckleScript.Internal.Spec
Description : Build OCaml Modules from Haskell Types
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module OCaml.BuckleScript.Internal.Spec
  (
    mkOCamlSpecServer
  , MkOCamlSpecAPI

  , mkGoldenFiles

  -- utility functions
  , OCamlSpecAPI
  , OCamlPackageTypeCount (..)
  , OCamlModuleTypeCount (..)
  ) where

-- base
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import GHC.Generics
import GHC.TypeLits

-- aeson
import Data.Aeson (ToJSON)

-- hspec-aeson-golden
import Test.Aeson.Internal.ADT.GoldenSpecs (mkGoldenFileForType)

-- ocaml-export
import OCaml.Internal.Common hiding ((</>))
import OCaml.BuckleScript.Internal.Module
import OCaml.BuckleScript.Internal.Package

-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT

-- servant
import Servant.API

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (Text)


mkOCamlSpecServer :: forall ocamlPackage. (OCamlPackageTypeCount ocamlPackage) => String -> Proxy ocamlPackage -> Q [Dec]
mkOCamlSpecServer typeName Proxy = do
  let sizes = ocamlPackageTypeCount (Proxy :: Proxy ocamlPackage)
  if (length sizes) < 1 || (not . and $ (> 0) <$> sizes)
    then fail $ "sizes must have at least one element and each element must be greater than zero: " <> show sizes
    else do
      let argss = (\size -> foldr (\l r -> ParensE $ UInfixE l (ConE $ mkName ":<|>") r) (VarE $ mkName "pure") (replicate (size-1) (VarE $ mkName "pure"))) <$> sizes
      let args = foldl (\l r -> UInfixE l (ConE $ mkName ":<|>") r) (head argss) (tail argss)
      return $
        [ SigD serverName (AppT (ConT $ mkName "Server") $ AppT (ConT $ mkName "MkOCamlSpecAPI") (ConT $ apiName))
        , FunD serverName [Clause [] (NormalB args ) [] ]

        , SigD apiProxy (AppT (ConT $ mkName "Proxy") $ AppT (ConT $ mkName "MkOCamlSpecAPI") (ConT $ apiName))
        , FunD apiProxy [Clause [] (NormalB $ ConE $ mkName "Proxy") []]

        , SigD appName (ConT $ mkName "Application")
        , FunD appName [Clause [] (NormalB $ AppE (AppE (VarE $ mkName "serve") (VarE apiProxy)) (VarE serverName)) []]
        ]
   where
     serverName = mkName $ lowercaseFirst typeName ++ "Server"
     apiName = mkName $ uppercaseFirst typeName
     apiProxy = mkName $ lowercaseFirst typeName ++ "API"
     appName = mkName $ lowercaseFirst typeName ++ "App"


-- | Make hspec-aeson-golden golden files for each type in an OCamlPackage
class HasMkGoldenFiles a where
  mkGoldenFiles :: Proxy a -> Int -> FilePath -> IO ()

instance (HasMkGoldenFilesFlag a ~ flag, HasMkGoldenFiles' flag (a :: *)) => HasMkGoldenFiles a where
  mkGoldenFiles = mkGoldenFiles' (Proxy :: Proxy flag)

type family (HasMkGoldenFilesFlag a) :: Nat where
  HasMkGoldenFilesFlag (OCamlPackage a b :> c) = 7
  HasMkGoldenFilesFlag ((OCamlModule a :> b) :<|> c) = 6
  HasMkGoldenFilesFlag (OCamlModule a :> b) = 5
  HasMkGoldenFilesFlag (OCamlSubModule a :> b) = 4
  HasMkGoldenFilesFlag (OCamlTypeInFile a b) = 3
  HasMkGoldenFilesFlag (a :> b) = 2
  HasMkGoldenFilesFlag a = 1

class HasMkGoldenFiles' (flag :: Nat) a where
  mkGoldenFiles' :: Proxy flag -> Proxy a -> Int -> FilePath -> IO ()

instance (HasMkGoldenFiles a) => HasMkGoldenFiles' 7 (OCamlPackage packageName deps :> a) where
  mkGoldenFiles' _ Proxy size fp = mkGoldenFiles (Proxy :: Proxy a) size fp

instance (HasMkGoldenFiles a, HasMkGoldenFiles b) => HasMkGoldenFiles' 6 ((OCamlModule modules :> a) :<|> b) where
  mkGoldenFiles' _ Proxy size fp = do
    mkGoldenFiles (Proxy :: Proxy a) size fp
    mkGoldenFiles (Proxy :: Proxy b) size fp
    
instance (HasMkGoldenFiles a) => HasMkGoldenFiles' 5 (OCamlModule modules :> a) where
  mkGoldenFiles' _ Proxy size fp = mkGoldenFiles (Proxy :: Proxy a) size fp

instance (HasMkGoldenFiles a) => HasMkGoldenFiles' 4 (OCamlSubModule subModule :> a) where
  mkGoldenFiles' _ Proxy size fp = mkGoldenFiles (Proxy :: Proxy a) size fp

instance (ToADTArbitrary a, ToJSON a) => HasMkGoldenFiles' 3 (OCamlTypeInFile a b) where
  mkGoldenFiles' _ Proxy size fp = mkGoldenFileForType size (Proxy :: Proxy a) fp

instance (HasMkGoldenFiles a, HasMkGoldenFiles b) => HasMkGoldenFiles' 2 (a :> b) where
  mkGoldenFiles' _ Proxy size fp = do
    mkGoldenFiles (Proxy :: Proxy a) size fp
    mkGoldenFiles (Proxy :: Proxy b) size fp

instance (ToADTArbitrary a, ToJSON a) => HasMkGoldenFiles' 1 a where
  mkGoldenFiles' _ Proxy size fp = mkGoldenFileForType size (Proxy :: Proxy a) fp





-- | Convert an OCamlPackage into a servant API.
type family MkOCamlSpecAPI a :: * where
  MkOCamlSpecAPI (OCamlPackage a deps :> rest) = MkOCamlSpecAPI rest  
  MkOCamlSpecAPI ((OCamlModule modules :> api) :<|> rest) = MkOCamlSpecAPI' modules '[] api :<|> MkOCamlSpecAPI rest
  MkOCamlSpecAPI (OCamlModule modules :> api) = MkOCamlSpecAPI' modules '[] api

-- | Utility type level function.
type family MkOCamlSpecAPI' modules subModules api :: * where
  MkOCamlSpecAPI' modules subModules ((OCamlSubModule restSubModules) :> a) = MkOCamlSpecAPI' modules (Append subModules '[restSubModules]) a
  MkOCamlSpecAPI' modules subModules (a :> b) = MkOCamlSpecAPI' modules subModules a :<|> MkOCamlSpecAPI' modules subModules b
  MkOCamlSpecAPI' modules subModules (OCamlTypeInFile api _typeFilePath) = OCamlSpecAPI modules subModules api
  MkOCamlSpecAPI' modules subModules api = OCamlSpecAPI modules subModules api

-- | A servant route for a testing an OCaml type's encoder and decoder
type OCamlSpecAPI (modules :: [Symbol]) (subModules :: [Symbol]) typ =
  ConcatSymbols (Insert (TypeName typ) (Append modules subModules)) (ReqBody '[JSON] [typ] :> Post '[JSON] [typ])

class OCamlModuleTypeCount api where
  ocamlModuleTypeCount :: Proxy api -> Int
    
instance (OCamlModuleTypeCountFlag a ~ flag, OCamlModuleTypeCount' flag (a :: *)) => OCamlModuleTypeCount a where
  ocamlModuleTypeCount = ocamlModuleTypeCount' (Proxy :: Proxy flag)

type family (OCamlModuleTypeCountFlag a) :: Bool where
  OCamlModuleTypeCountFlag (a :> b)           = 'True
  OCamlModuleTypeCountFlag (OCamlModule a)    = 'True
  OCamlModuleTypeCountFlag (OCamlSubModule a) = 'True
  OCamlModuleTypeCountFlag a                  = 'False

class OCamlModuleTypeCount' (flag :: Bool) a where
  ocamlModuleTypeCount' :: Proxy flag -> Proxy a -> Int

instance (OCamlModuleTypeCount a, OCamlModuleTypeCount b) => OCamlModuleTypeCount' 'True (a :> b) where
  ocamlModuleTypeCount' _ Proxy = (ocamlModuleTypeCount (Proxy :: Proxy a)) + (ocamlModuleTypeCount (Proxy :: Proxy b))

instance OCamlModuleTypeCount' 'True (OCamlModule modules) where
  ocamlModuleTypeCount' _ Proxy = 0

instance OCamlModuleTypeCount' 'True (OCamlSubModule subModules) where
  ocamlModuleTypeCount' _ Proxy = 0

instance OCamlModuleTypeCount' 'False a where
  ocamlModuleTypeCount' _ Proxy = 1

class OCamlPackageTypeCount modules where                
  ocamlPackageTypeCount :: Proxy modules -> [Int]

instance (OCamlPackageTypeCountFlag a ~ flag, OCamlPackageTypeCount' flag (a :: *)) => OCamlPackageTypeCount a where
  ocamlPackageTypeCount = ocamlPackageTypeCount' (Proxy :: Proxy flag)

type family (OCamlPackageTypeCountFlag a) :: Bool where
  OCamlPackageTypeCountFlag (OCamlPackage a b :> c) = 'True
  OCamlPackageTypeCountFlag (a :<|> b)              = 'True
  OCamlPackageTypeCountFlag a                       = 'False 

class OCamlPackageTypeCount' (flag :: Bool) a where
  ocamlPackageTypeCount' :: Proxy flag -> Proxy a -> [Int]

-- OCamlPackage does not increment
instance (OCamlPackageTypeCount b) => OCamlPackageTypeCount' 'True (OCamlPackage a deps :> b) where
  ocamlPackageTypeCount' _ Proxy = ocamlPackageTypeCount (Proxy :: Proxy b)

-- Choice operator does not increment
instance (OCamlModuleTypeCount a, OCamlPackageTypeCount b) => OCamlPackageTypeCount' 'True (a :<|> b) where
  ocamlPackageTypeCount' _ Proxy = (ocamlModuleTypeCount (Proxy :: Proxy a)) : (ocamlPackageTypeCount (Proxy :: Proxy b))

-- everything else should count as one
instance (OCamlModuleTypeCount a) => OCamlPackageTypeCount' 'False a where
  ocamlPackageTypeCount' _ Proxy = [ocamlModuleTypeCount (Proxy :: Proxy a)]


-- type family TypeName a :: Symbol

-- | Convert a type into a Symbol at the type level.
type family TypeName a :: Symbol where
  -- Types which don't have a Generic instance
  TypeName Bool   = "Bool"
  TypeName Double = "Double"
  TypeName Int = "Int"
  TypeName Integer = "Integer"
  TypeName String = "String"
  TypeName Text = "Text"
  
  -- Generic instances
  TypeName (M1 D ('MetaData name _ _ _) f ()) = name

--  TypeName (Rep a b) = TypeName (Rep1 a ())
  TypeName a = TypeName (Rep a ())
--  TypeName (M1 D ('MetaData name _ _ _) f ()) = name
--  TypeName (S1 _ _ a) = TypeName (Rep a ())
--  TypeName (S1 _ _ a) = TypeName (Rep a ())
  {-
S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
               (Rec0 (Tree a))))
-}
  -- TypeName (Rep a ()) = TypeName (Rep a ())
--  TypeName a = TypeName (Rep a ())
  -- TypeName a = TypeName' a
  
-- open
type family TypeName' a :: Symbol
type instance TypeName' Bool = "Bool"

--type instance TypeName' a = ""
type instance TypeName' Int = "Int"

{-

λ> :t from On
from On
  :: D1
       ('MetaData "OnOrOff" "Ghci2" "interactive" 'False)
       (C1 ('MetaCons "On" 'PrefixI 'False) U1
        :+: C1 ('MetaCons "Off" 'PrefixI 'False) U1)
       x
λ> :t from (T 1)
from (T 1)
  :: D1
       ('MetaData "T" "Ghci1" "interactive" 'False)
       (C1
          ('MetaCons "T" 'PrefixI 'False)
          (S1
             ('MetaSel
                'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
             (Rec0 Int)))
       x


class Print a where
  tprint :: a -> Symbol

instance (TShow a ~ flag, Print' flag a) => Print a where
  tprint = print' (Proxy :: Proxy flag)

class Print' flag a where
  print' :: flag -> a -> Symbol

instance Print' HTrue a where
  print' _ x = tprint (Rep a ())

instance Print' HFalse (M1 D ('MetaData name a b c) f ()) where
  print' _ x = (name :: Symbol)

--type family TShow a :: Symbol
type family TShow a
type instance TShow Bool = HTrue

data HTrue
data HFalse
{-
type instance TShow Bool = "Bool"
type instance TShow Double = "Double"
type instance TShow Int = "Int"
type instance TShow Integer = "Integer"
type instance TShow String = "String"
type instance TShow Text = "Text"
type instance TShow (M1 D ('MetaData name _ _ _) f ()) = name
-}
-- type instance TShow a = TypeName (Rep a ())
--  TypeName a = TypeName (Rep a ())
-}

-- | Append two type level lists.
type family Append xy ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': (Append xs ys)

-- | Get the length of a type level list.
type family Length xs :: Nat where
  Length '[]       = 0
  Length (x ': xs) = 1 + Length xs

-- | Insert type into type level list
type family Insert a xs where
  Insert a '[]       = a ': '[]
  Insert a (x ': xs) = x ': (Insert a xs)

-- | Concat a Symbol the end of a list of Symbols.
type family ConcatSymbols xs rhs :: * where
  ConcatSymbols '[] rhs = rhs
  ConcatSymbols (x ': xs) rhs = x :> ConcatSymbols xs rhs
