{-|
Module      : OCaml.BuckleScript.Module
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module OCaml.BuckleScript.Module
  (
  -- re-export from Servant
    (:>)

  -- type classes
  , HasOCamlModule (..)
  , HasOCamlType (..)
  , HasGenericOCamlType (..)
  , HasOCamlTypeInFile (..)

  -- types without constructors
  -- used to calculate types
  , OCamlModule
  , OCamlTypeInFile
  , ConcatSymbols

  -- automatically build a servant OCamlSpec Serve
  , OCamlTypeCount (..)
  , OCamlSpecAPI
  , MkOCamlSpecAPI
  , mkServer
  ) where

-- base
import Data.Char (toUpper, toLower)
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Typeable
import GHC.Generics

-- template-haskell
import Language.Haskell.TH

-- filepath
import System.FilePath.Posix ((</>), (<.>))

-- ocaml-export
import OCaml.Common hiding (lowercaseFirst, uppercaseFirst)
import OCaml.BuckleScript.Decode
import OCaml.BuckleScript.Encode
import OCaml.BuckleScript.Record
import OCaml.BuckleScript.Spec
import OCaml.BuckleScript.Types

-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO     as T

-- type level
-- turn type Symbol into String
import GHC.TypeLits
import GHC.TypeLits.List
import Data.Type.Bool
import Data.Type.Equality

-- servant
import Servant.API

-- wl-pprint
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>), (</>))


-- | an OCamlModule as a Haskell type
--   filePath is dir where to store OCamlModule
--   each symbol in moduleName is expanded into "module Symbol = struct ... end"
data OCamlModule (filePath :: [Symbol]) (moduleName :: [Symbol])
  deriving Typeable


--data OCamlModz (filePath :: [Symbol]) (moduleName :: OCamlSubModule)
--  deriving Typeable

--data OCamlModz' (name :: Symbol) (modz :: OCamlSubModule)
--  deriving Typeable

--data OCamlSubModule c
--  = OCamlSubModule (name :: Symbol) (modz :: OCamlSubModule (Proxy :: Proxy c))
--  | OCamlSubModuleOCamlType c
  

-- represent a handwritten OCaml type, encoder and decoder as a Haskell type
data OCamlTypeInFile (a :: Symbol) (filePath :: Symbol)
  deriving Typeable

-- Type with definition only
-- Type with definition and interface
-- either with Spec and Golden File
-- Type with Manual Definintion (with and without interface)
-- 
-- data OCamlPackage list of modules and specs


class HasOCamlModule a where
  mkModule :: Proxy a -> FilePath -> IO ()
  mkModuleWithSpec :: Proxy a -> FilePath -> FilePath -> FilePath -> String -> IO ()
  
instance (KnownSymbols filePath, KnownSymbols moduleName, HasOCamlType api) => HasOCamlModule ((OCamlModule filePath moduleName) :> api) where
  mkModule Proxy rootdir = do
    if (length $ symbolsVal (Proxy :: Proxy filePath)) == 0
      then fail "OCamlModule filePath needs at least one file name"
      else do
        typF <- localModuleDoc <$> mkType (Proxy :: Proxy api)
        intF <- localModuleDoc <$> mkInterface (Proxy :: Proxy api)
        T.writeFile (fp <.> "ml")  typF
        T.writeFile (fp <.> "mli") intF
        
    where
      fp = rootdir </> (foldl (</>) "" $ symbolsVal (Proxy :: Proxy filePath))
      localModuleDoc body = pprinter $ foldr (\l r -> "module" <+> l <+> "= struct" <$$> indent 2 r <$$> "end") (stext body) (stext . T.pack <$> symbolsVal (Proxy :: Proxy moduleName))
      
  mkModuleWithSpec p rootdir specdir goldendir url = do
    mkModule p rootdir
    specF <- mkSpec (Proxy :: Proxy api) (T.pack localModule) (T.pack url) (T.pack goldendir)
    let specBody = if specF /= "" then ("let () =\n" <> specF) else ""
    T.writeFile (fp <.> "ml") specBody
    
    where
      fp = rootdir </> specdir </> (foldl (</>) "" $ symbolsVal (Proxy :: Proxy filePath))
      localModule = (foldl (<.>) "" $ symbolsVal (Proxy :: Proxy moduleName))

-- module Hello = struct
-- end

-- | Combine `HasGenericOCamlType` and `HasOCamlTypeInFile`
class HasOCamlType api where
  mkType :: Proxy api -> IO Text
  mkInterface :: Proxy api -> IO Text
  mkSpec :: Proxy api -> Text -> Text -> Text -> IO Text

instance (HasOCamlType a, HasOCamlType b) => HasOCamlType (a :> b) where
  mkType Proxy = (<>) <$> (mkType (Proxy :: Proxy a)) <*> (mkType (Proxy :: Proxy b))
  mkInterface Proxy = (<>) <$> (mkInterface (Proxy :: Proxy a)) <*> (mkInterface (Proxy :: Proxy b))
  mkSpec Proxy modul url goldendir = (<>) <$> (mkSpec (Proxy :: Proxy a) modul url goldendir) <*> (mkSpec (Proxy :: Proxy b) modul url goldendir)

instance {-# OVERLAPPABLE #-} (HasOCamlTypeInFile (OCamlTypeInFile a b)) => HasOCamlType (OCamlTypeInFile a b) where
  mkType a = readType a
  mkInterface a = readInterface a
  mkSpec _ _ _ _ = pure ""

instance {-# OVERLAPPABLE #-} (HasGenericOCamlType a) => HasOCamlType a where
  mkType a = pure $ mkGType a
  mkInterface a = pure $ mkGInterface a
  mkSpec a modul url goldendir = pure $ mkGSpec a modul url goldendir


-- | Read OCaml type declarations and interfaces from `ml` and `mli` files
class HasOCamlTypeInFile api where
  readType :: Proxy api -> IO Text
  readInterface :: Proxy api -> IO Text

instance (HasOCamlTypeInFile a, HasOCamlTypeInFile b) => HasOCamlTypeInFile (a :> b) where
  readType Proxy = (<>) <$> (readType (Proxy :: Proxy a)) <*> (readType (Proxy :: Proxy b))
  readInterface Proxy = (<>) <$> (readInterface (Proxy :: Proxy a)) <*> (readInterface (Proxy :: Proxy b))

instance (KnownSymbol a, KnownSymbol b) => HasOCamlTypeInFile (OCamlTypeInFile a b) where
  readType Proxy = T.readFile $ symbolVal (Proxy :: Proxy b) </> symbolVal (Proxy :: Proxy a) <.> "ml"
  readInterface Proxy = T.readFile $ symbolVal (Proxy :: Proxy b) </> symbolVal (Proxy :: Proxy a) <.> "mli"


-- | Produce OCaml files for types that have OCamlType derived via GHC.Generics
class HasGenericOCamlType api where
  mkGType :: Proxy api -> Text
  mkGInterface :: Proxy api -> Text
  mkGSpec :: Proxy api -> Text -> Text -> Text -> Text

instance (HasGenericOCamlType a, HasGenericOCamlType b) => HasGenericOCamlType (a :> b) where
  mkGType Proxy = mkGType (Proxy :: Proxy a) <> mkGType (Proxy :: Proxy b)
  mkGInterface Proxy = mkGInterface (Proxy :: Proxy a) <> mkGInterface (Proxy :: Proxy b)
  mkGSpec Proxy modul url goldendir = (mkGSpec (Proxy :: Proxy a) modul url goldendir) <> (mkGSpec (Proxy :: Proxy b) modul url goldendir)

instance {-# OVERLAPPABLE #-} OCamlType a => HasGenericOCamlType a where
  mkGType a = toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderSource a <> "\n\n" <> toOCamlDecoderSource a <> "\n"
  mkGInterface a = toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderInterface a <> "\n\n" <> toOCamlDecoderInterface a <> "\n"
  mkGSpec a modul url goldendir = toOCamlSpec a modul url goldendir <> "\n"





-- build servant spec server

-- | Symbol for Proxy types
type family TypeName a :: Symbol where
  -- Types which don't have a Generic instance
  TypeName Double = "Double"
  TypeName Int    = "Int"
  TypeName String = "String"
  TypeName Text   = "Text"

  -- Generic instances
  TypeName (M1 D ('MetaData name _ _ _) f ()) = name
  TypeName a = TypeName (Rep a ())

type family TypeNames a :: [Symbol] where
  TypeNames (a ': '[]) = '[TypeName a]
  TypeNames (a ': as) = TypeName a ': TypeNames as



-- | Insert type into type level list
type family Insert a xs where
   Insert a '[]       = (a ': '[])
   Insert a (a ': xs) = (a ': xs)
   Insert a (x ': xs) = x ': (Insert a xs)

{-            
type family Length2 xs where
   Length2 (x :> xs) = 1 + Length xs
   Length2 a       = 1
-}
            
-- |
type InAndOut a = (TypeName a) :> ReqBody '[JSON] [a] :> Post '[JSON] [a]

type family InAndOutAPI a :: * where
  InAndOutAPI (a :> b) = InAndOutAPI a :<|> InAndOutAPI b
  InAndOutAPI a = InAndOut a

-- |
type InAndOut2 (modul :: [Symbol]) typ = ConcatSymbols (Insert (TypeName typ) modul) (ReqBody '[JSON] [typ] :> Post '[JSON] [typ])

type family InAndOut2API modul a :: * where
  InAndOut2API modul (a :> b) = InAndOut2API modul a :<|> InAndOut2API modul b
  InAndOut2API modul a = InAndOut2 modul a  

type family MkInAndOut2API a :: * where
  MkInAndOut2API (OCamlModule a b :> api) = InAndOut2API b api

-- | OCamlSpecAPI is a servant type that repesents and OCamlModule
--   and its OCamlTypes. It automatically creates path names based on the
--   name of the OCamlModule and the name of each OCamlType
--   OCamlModule '[] '["Core"] :> User :> Profile
--   /Core/User
--   /Core/Profile
type OCamlSpecAPI (modul :: [Symbol]) typ = ConcatSymbols (Insert (TypeName typ) modul) (ReqBody '[JSON] [typ] :> Post '[JSON] [typ])

type family MkOCamlSpecAPI' modul a :: * where
  MkOCamlSpecAPI' modul (a :> b) = MkOCamlSpecAPI' modul a :<|> MkOCamlSpecAPI' modul b
  MkOCamlSpecAPI' modul a = OCamlSpecAPI modul a  

type family MkOCamlSpecAPI a :: * where
  MkOCamlSpecAPI (OCamlModule a b :> api) = MkOCamlSpecAPI' b api




-- | Get the number of declared types in an OCaml Module
--   Internal helper function.
type family (Flag a) :: Bool where
  Flag (a :> b)  = 'True -- case 0
  Flag (OCamlModule a b)  = 'True -- case 1
  Flag a     = 'False -- case 2


-- | Exposed type level function
class OCamlTypeCount api where
  ocamlTypeCount :: Proxy api -> Int
    
instance (Flag a ~ flag, OCamlTypeCount' flag (a :: *)) => OCamlTypeCount a where
  ocamlTypeCount = ocamlTypeCount' (Proxy :: Proxy flag)


-- | Internal helper function
class OCamlTypeCount' (flag :: Bool) a where
  ocamlTypeCount' :: Proxy flag -> Proxy a -> Int

-- case 0
instance (OCamlTypeCount a, OCamlTypeCount b) => OCamlTypeCount' 'True (a :> b) where
  ocamlTypeCount' _ Proxy = (ocamlTypeCount (Proxy :: Proxy a)) + (ocamlTypeCount (Proxy :: Proxy b))

-- case 1
-- do not count anything wrapped in OCamlModule
instance OCamlTypeCount' 'True (OCamlModule a b) where
  ocamlTypeCount' _ Proxy = 0

-- case 2
-- everything else should count as one
instance OCamlTypeCount' 'False a where
  ocamlTypeCount' _ Proxy = 1



   
-- natVal
-- | Get the length of a type level list
type family Length xs where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs


type family ConcatSymbols xs rhs :: * where
  ConcatSymbols '[] rhs = rhs            
  ConcatSymbols (x ': xs) rhs = If ((Length xs) == 0) (x :> rhs) (x :> ConcatSymbols xs rhs)


lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (x : xs) = toLower x : xs

uppercaseFirst :: String -> String
uppercaseFirst [] = []
uppercaseFirst (x : xs) = toUpper x : xs
                
mkServer :: forall ocamlTypes. (OCamlTypeCount ocamlTypes, HasOCamlModule ocamlTypes) => String -> Proxy ocamlTypes -> Q [Dec]
mkServer typeName Proxy = do
  let size = ocamlTypeCount (Proxy :: Proxy ocamlTypes)
  if size < 1
    then fail "size must be at least one"
    else do
      let args = foldl (\l r -> UInfixE l (ConE $ mkName ":<|>") r) (VarE $ mkName "pure") (replicate (size-1) (VarE $ mkName "pure"))

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
