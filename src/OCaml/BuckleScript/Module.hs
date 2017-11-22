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

  , InAndOut
  , InAndOutAPI
  , InAndOut2
  , InAndOut2API
  , MkInAndOut2API
  , Length2

  , APILength (..)
  , mkServer
  ) where

-- base
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Typeable

-- template-haskell
import Language.Haskell.TH

-- filepath
import System.FilePath.Posix ((</>), (<.>))

-- ocaml-export
import OCaml.BuckleScript.Types
import OCaml.BuckleScript.Record
import OCaml.BuckleScript.Encode
import OCaml.BuckleScript.Decode
import OCaml.BuckleScript.Spec

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
import GHC.Generics

-- represent an OCamlModule as a Haskell type
data OCamlModule (filePath :: [Symbol]) (moduleName :: [Symbol])
  deriving Typeable

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
  
instance (KnownSymbols a, KnownSymbols b, HasOCamlType api) => HasOCamlModule ((OCamlModule a b) :> api) where
  mkModule Proxy rootdir = do
    if (length $ symbolsVal (Proxy :: Proxy a)) == 0
      then fail "OCamlModule filePath needs at least one file name"
      else do
        typF <- mkType (Proxy :: Proxy api)
        intF <- mkInterface (Proxy :: Proxy api)

        T.writeFile (fp <.> "ml")  typF
        T.writeFile (fp <.> "mli") intF
    where
      fp = rootdir </> (foldl (</>) "" $ symbolsVal (Proxy :: Proxy a))

  mkModuleWithSpec p rootdir specdir goldendir url = do
    mkModule p rootdir
    specF <- mkSpec (Proxy :: Proxy api) (T.pack localModule) (T.pack url) (T.pack goldendir)
    let specBody = if specF /= "" then ("let () =\n" <> specF) else ""
    T.writeFile (fp <.> "ml") specBody
    
    where
      fp = rootdir </> specdir </> (foldl (</>) "" $ symbolsVal (Proxy :: Proxy a))
      localModule = (foldl (<.>) "" $ symbolsVal (Proxy :: Proxy b))

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



-- | Insert type into type level list
type family Insert a xs where
   Insert a '[]       = (a ': '[])
   Insert a (a ': xs) = (a ': xs)
   Insert a (x ': xs) = x ': (Insert a xs)

-- | Get the length of a type level list
type family Length xs where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs

type family Length2 xs where
   --Length2 (OCamlModule a b :> xs) = 0 + Length xs
   Length2 (x :> xs) = 1 + Length xs
   Length2 a       = 1


type family (F a) :: Bool where
  F (a :> b)  = 'True
  F (OCamlModule a b)  = 'True
  F a     = 'False

class APILength api where
  apiLength :: Proxy api -> Int
    
instance (F a ~ flag, APILength' flag (a :: *)) => APILength a where
  apiLength = apiLength' (Proxy :: Proxy flag)

class APILength' (flag :: Bool) a where
  apiLength' :: Proxy flag -> Proxy a -> Int

instance APILength' 'True (OCamlModule a b) where
  apiLength' _ Proxy = 0

instance (APILength a, APILength b) => APILength' 'True (a :> b) where
  apiLength' _ Proxy = (apiLength (Proxy :: Proxy a)) + (apiLength (Proxy :: Proxy b))

instance APILength' 'False a where
  apiLength' _ Proxy = 1


-- | Get the number of types in an OCaml Module
{-
class APILength api where
  apiLength :: Proxy api -> Int
  apiLength Proxy = 1

instance (APILength a, APILength b) => APILength (a :> b) where
  apiLength Proxy = (apiLength (Proxy :: Proxy a)) + (apiLength (Proxy :: Proxy b))

instance APILength a

instance APILength (OCamlModule a b) where
  apiLength Proxy = 0
-}

{-
class APILength api where
  apiLength :: Proxy api -> Int

instance {-# OVERLAPPABLE #-} (APILength a, APILength b) => APILength (a :> b) where
  apiLength Proxy = (apiLength (Proxy :: Proxy a)) + (apiLength (Proxy :: Proxy b))

instance APILength (OCamlModule a b) where
  apiLength Proxy = 0

instance {-# OVERLAPPING #-} (HasOCamlType a) => APILength a where
  apiLength Proxy = 1

-}

{-
class CountRS a where
    countRS :: a -> Int
    countRS _ = 1
    countRSList :: [a] -> Int
    countRSList = countListDefault 

instance CountRS Char where
    countRSList = length . words

instance CountRS a => CountRS [a] where
    countRS = countRSList

instance CountRS Bool

-}

   
 -- natVal            
type family ConcatSymbols xs rhs :: * where
  ConcatSymbols '[] rhs = rhs            
  ConcatSymbols (x ': xs) rhs = If ((Length xs) == 0) (x :> rhs) (x :> ConcatSymbols xs rhs)


mkServer :: forall api. (APILength api, HasOCamlModule api) => String -> String -> Proxy api -> Q [Dec]
mkServer fn apiName Proxy = do
  let size = apiLength (Proxy :: Proxy api)
  if size < 1
    then fail "size must be at least one"
    else do
      let fnName = mkName fn
      let args = foldl (\l r -> UInfixE l (ConE $ mkName ":<|>") r) (VarE $ mkName "pure") (replicate (size-1) (VarE $ mkName "pure"))

      return $
        [ SigD fnName (AppT (ConT $ mkName "Server") $ AppT (ConT $ mkName "InAndOutAPI") (ConT $ mkName apiName))
        , FunD fnName [Clause [] (NormalB args ) [] ]
        ]


{-                
-- mkServer :: (APILength a) => String -> String -> Proxy a -> Q [Dec]
mkServer :: String -> String -> Int -> Q [Dec]
mkServer fn apiName size = do
--mkServer fn apiName Proxy = do
--  let size = apiLength (Proxy :: Proxy a)
  if size < 1
    then fail "size must be at least one"
    else do
      let fnName = mkName fn
      let args = foldl (\l r -> UInfixE l (ConE $ mkName ":<|>") r) (VarE $ mkName "pure") (replicate (size-1) (VarE $ mkName "pure"))

      return $
        [ SigD fnName (AppT (ConT $ mkName "Server") $ AppT (ConT $ mkName "InAndOutAPI") (ConT $ mkName apiName))
        , FunD fnName [Clause [] (NormalB args ) [] ]
        ]
-}
mkAPIProxy :: String -> Q [Dec]
mkAPIProxy moduleName =
  return $
    [ SigD fnName (AppT (ConT $ mkName "Proxy") $ AppT (ConT $ mkName "InAndOutAPI") (ConT $ mkName moduleName))
    , FunD fnName [Clause [] (NormalB $ VarE $ mkName "Proxy") [] ]
    ]
  where
    fnName = mkName $ moduleName ++ "API"

mkApp :: String -> Q [Dec]
mkApp moduleName =
  return $
    [ SigD appName (AppT (ConT $ mkName "Proxy") $ AppT (ConT $ mkName "InAndOutAPI") (ConT $ mkName moduleName))
    , FunD appName [Clause [] (NormalB $ (AppE (VarE $ mkName "serve") $ AppE (VarE apiName) (VarE appName))) []]
    ]
  where
    appName = mkName $ moduleName ++ "API"
    apiName = mkName $ moduleName ++ "App"
    serverName = mkName $ moduleName ++ "Server"



  -- ParensE
