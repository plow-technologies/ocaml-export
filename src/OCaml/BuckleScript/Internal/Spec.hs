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

-- ocaml-export
import OCaml.Internal.Common hiding ((</>))
import OCaml.BuckleScript.Internal.Module
import OCaml.BuckleScript.Internal.Package

-- servant
import Servant.API

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (Text)


mkOCamlSpecServer :: forall ocamlPackage. (OCamlPackageTypeCount ocamlPackage, HasOCamlPackage ocamlPackage) => String -> Proxy ocamlPackage -> Q [Dec]
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


-- | Convert a type into a Symbol at the type level.
type family TypeName a :: Symbol where
  -- Types which don't have a Generic instance
  TypeName Double = "Double"
  TypeName Int    = "Int"
  TypeName String = "String"
  TypeName Text   = "Text"

  -- Generic instances
  TypeName (M1 D ('MetaData name _ _ _) f ()) = name
  TypeName a = TypeName (Rep a ())

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
