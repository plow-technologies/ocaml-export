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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module OCaml.BuckleScript.Module
  (
  -- options
    PackageOptions (..)
  , defaultPackageOptions
  , SpecOptions (..)
  , defaultSpecOptions

  -- type classes
  , HasOCamlPackage (..)
  , HasOCamlModule (..)
  , HasOCamlType (..)
--  , HasGenericOCamlType (..)
--  , HasOCamlTypeInFile (..)

  -- types without constructors
  -- used to calculate types at the type level
  , OCamlPackage
  , OCamlModule
  , OCamlSubModule
  , OCamlTypeInFile
  , NoDependency

  -- servant functions
  -- automatically build a servant OCamlSpec Serve
  , OCamlPackageTypeCount (..)
  , OCamlModuleTypeCount (..)
  , OCamlSpecAPI
  , MkOCamlSpecAPI
  , mkServer

  , HasOCamlTypeMetaData (..)

  -- load handwritten OCaml files at compile time
  , EmbeddedOCamlFiles (..)
  , HasEmbeddedFile (..)
    -- re-export from Servant
  , (:>)
  , (:<|>)

  ) where

-- base
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Typeable (typeRep, Typeable, typeRepTyCon, tyConName, tyConModule, tyConPackage)
import GHC.Generics
import GHC.TypeLits

-- bytestring
import Data.ByteString (ByteString)

-- containers
import qualified Data.Map.Strict as Map

-- directory
import System.Directory (createDirectoryIfMissing)

-- file-embed
import Data.FileEmbed (embedFile)

-- filepath
import System.FilePath.Posix ((</>), (<.>))

-- ocaml-export
import OCaml.Common hiding ((</>))
import OCaml.BuckleScript.Decode
import OCaml.BuckleScript.Encode
import OCaml.BuckleScript.Record
import OCaml.BuckleScript.Spec
import OCaml.BuckleScript.Types
-- import OCaml.Internal.Type.Util (ConcatSymbols,TypeNames)

-- servant
import Servant.API

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO     as T
import Data.Text.Encoding (decodeUtf8)

-- typelits-witnesses
import GHC.TypeLits.List

-- | Options for creating an OCaml package based on Haskell types.
data PackageOptions
  = PackageOptions
    { packageRootDir :: FilePath -- ^ root directory where all relatives directories will be placed.
    , packageSrcDir :: FilePath -- ^ location to place ml and mli files relative to 'packageRootDir'.
    , packageEmbeddedFiles :: Map.Map String EmbeddedOCamlFiles
    , createInterfaceFile :: Bool -- ^ create an mli file if 'True'.
    , mSpecOptions :: Maybe SpecOptions -- ^ produce OCaml spec file if 'Just'.
    }

-- | Default 'PackageOptions'.
defaultPackageOptions :: PackageOptions
defaultPackageOptions = PackageOptions "ocaml-generic-package" "src" Map.empty True (Just defaultSpecOptions)

-- | Details for OCaml spec.
data SpecOptions
  = SpecOptions
    { specDir :: FilePath -- ^ Directory in which to store the OCaml spec, relative to 'packageRootDir'.
    , goldenDir :: FilePath -- ^ Location of golden JSON files produced by Haskell, relative to 'packageRootDir'.
    , servantURL :: String -- ^ The URL of the automated Servant spec server to run OCaml specs against.
    }

-- | Default 'SpecOptions'.
defaultSpecOptions :: SpecOptions
defaultSpecOptions = SpecOptions "/__tests__" "/__tests__/golden" "localhost:8081"

class HasOCamlTypeMetaData a where
  mkOCamlTypeMetaData :: Proxy a -> Map.Map HaskellTypeMetaData OCamlTypeMetaData -- [OCamlTypeMetaData]

-- | packages
instance (HasOCamlTypeMetaData (OCamlPackage packageName deps), HasOCamlTypeMetaData rest) => HasOCamlTypeMetaData (OCamlPackage packageName deps :<|> rest) where
  mkOCamlTypeMetaData Proxy = mkOCamlTypeMetaData (Proxy :: Proxy (OCamlPackage packageName deps)) <> mkOCamlTypeMetaData (Proxy :: Proxy rest)

-- | build a packages dependencies and its declared modules
instance (HasOCamlTypeMetaData deps, HasOCamlTypeMetaData modules) => HasOCamlTypeMetaData (OCamlPackage packageName deps :> modules) where
  mkOCamlTypeMetaData Proxy = mkOCamlTypeMetaData (Proxy :: Proxy deps) <> mkOCamlTypeMetaData (Proxy :: Proxy modules)

-- | packages
instance (HasOCamlTypeMetaData modul, HasOCamlTypeMetaData rst) => HasOCamlTypeMetaData (modul ': rst) where
  mkOCamlTypeMetaData Proxy = mkOCamlTypeMetaData (Proxy :: Proxy modul) <> mkOCamlTypeMetaData (Proxy :: Proxy rst)

-- | modules
instance (HasOCamlTypeMetaData modul, HasOCamlTypeMetaData rst) => HasOCamlTypeMetaData (modul :<|> rst) where
  mkOCamlTypeMetaData Proxy = mkOCamlTypeMetaData (Proxy :: Proxy modul) <> mkOCamlTypeMetaData (Proxy :: Proxy rst)

-- | single module
instance (KnownSymbols modules, HasOCamlTypeMetaData' api) => HasOCamlTypeMetaData ((OCamlModule modules) :> api) where
  mkOCamlTypeMetaData Proxy = Map.fromList $ mkOCamlTypeMetaData' (T.pack <$> symbolsVal (Proxy :: Proxy modules)) [] (Proxy :: Proxy api)


instance HasOCamlTypeMetaData '[] where
  mkOCamlTypeMetaData Proxy = Map.empty

-- | Need flag to overcome overlapping issues
type family (HasOCamlTypeMetaDataFlag a) :: Nat where
  HasOCamlTypeMetaDataFlag (OCamlSubModule a :> b) = 4
  HasOCamlTypeMetaDataFlag (a :> b) = 3
  HasOCamlTypeMetaDataFlag (OCamlTypeInFile a b) = 2
  HasOCamlTypeMetaDataFlag a = 1

class HasOCamlTypeMetaData' a where
  mkOCamlTypeMetaData' :: [Text] -> [Text] -> Proxy a -> [(HaskellTypeMetaData,OCamlTypeMetaData)]

instance (HasOCamlTypeMetaDataFlag a ~ flag, HasOCamlTypeMetaData'' flag (a :: *)) => HasOCamlTypeMetaData' a where
  mkOCamlTypeMetaData' = mkOCamlTypeMetaData'' (Proxy :: Proxy flag)

class HasOCamlTypeMetaData'' (flag :: Nat) a where
  mkOCamlTypeMetaData'' :: Proxy flag -> [Text] -> [Text] -> Proxy a -> [(HaskellTypeMetaData,OCamlTypeMetaData)]

instance (KnownSymbol subModule, HasOCamlTypeMetaData' b) => HasOCamlTypeMetaData'' 4 (OCamlSubModule subModule :> b) where
  mkOCamlTypeMetaData'' _ modules subModules Proxy =
    (mkOCamlTypeMetaData' modules (subModules ++ [T.pack $ symbolVal (Proxy :: Proxy subModule)]) (Proxy :: Proxy b))

instance (HasOCamlTypeMetaData' a, HasOCamlTypeMetaData' b) => HasOCamlTypeMetaData'' 3 (a :> b) where
  mkOCamlTypeMetaData'' _ modul subModul Proxy = (mkOCamlTypeMetaData' modul subModul (Proxy :: Proxy a)) <> (mkOCamlTypeMetaData' modul subModul (Proxy :: Proxy b))


instance (Typeable a) => HasOCamlTypeMetaData'' 2 (OCamlTypeInFile a b) where
  mkOCamlTypeMetaData'' _ modul subModul Proxy =
    [( HaskellTypeMetaData typeName (T.pack . tyConModule . typeRepTyCon $ aTypeRep) (T.pack . tyConPackage . typeRepTyCon $ aTypeRep)
    ,  OCamlTypeMetaData typeName modul subModul
    )]
    where
      aTypeRep = typeRep (Proxy :: Proxy a)
      typeName = T.pack . tyConName . typeRepTyCon $ aTypeRep

instance (Typeable a) => HasOCamlTypeMetaData'' 1 a where
  mkOCamlTypeMetaData'' _ modul subModul Proxy =
    [( HaskellTypeMetaData typeName (T.pack . tyConModule . typeRepTyCon $ aTypeRep) (T.pack . tyConPackage . typeRepTyCon $ aTypeRep)
    ,  OCamlTypeMetaData typeName modul subModul
    )]
    where
      aTypeRep = typeRep (Proxy :: Proxy a)
      typeName = T.pack . tyConName . typeRepTyCon $ aTypeRep


type NoDependency = '[]

-- | 
data OCamlPackage (packageName :: Symbol) (packageDependencies :: [*])
  deriving Typeable

-- | An OCamlModule as a Haskell type. File level 'modules' is relative to a
--   root directory prvoiided in the 'mkPackage' function. 
data OCamlModule (modules :: [Symbol])
  deriving Typeable

-- | Symobl will be expaneded to 
--   "module SymbolName = struct ... end".
data OCamlSubModule (subModules :: Symbol)
  deriving Typeable

-- | A handwritten OCaml type, encoder and decoder from a file.
data OCamlTypeInFile a (filePath :: Symbol)
  deriving Typeable

-- | Iterate over a list of OCamlModule types that are concated with '(:<|>)',
--   to create a package.
class HasOCamlPackage a where
  mkPackage :: Proxy a -> PackageOptions -> IO ()

instance (HasOCamlTypeMetaData deps, HasOCamlTypeMetaData a, HasOCamlPackage' a) => HasOCamlPackage (OCamlPackage packageName deps :> a) where
  mkPackage Proxy packageOptions = mkPackage' (Proxy :: Proxy a) packageOptions (mkOCamlTypeMetaData (Proxy :: Proxy deps) <> mkOCamlTypeMetaData (Proxy :: Proxy a))

instance {-# OVERLAPPABLE #-} (HasOCamlTypeMetaData a, HasOCamlPackage' a) => HasOCamlPackage a where
  mkPackage Proxy packageOptions = do
    mkPackage' (Proxy :: Proxy a) packageOptions (mkOCamlTypeMetaData (Proxy :: Proxy a))

class HasOCamlPackage' a where
  mkPackage' :: Proxy a -> PackageOptions -> Map.Map HaskellTypeMetaData OCamlTypeMetaData -> IO ()

instance (HasOCamlPackage' modul, HasOCamlPackage' rest) => HasOCamlPackage' (modul :<|> rest) where
  mkPackage' Proxy packageOptions deps = do
    mkPackage' (Proxy :: Proxy modul) packageOptions deps
    mkPackage' (Proxy :: Proxy rest) packageOptions deps

instance {-# OVERLAPPABLE #-} (HasOCamlModule a) => HasOCamlPackage' a where
  mkPackage' Proxy packageOptions = mkModule (Proxy :: Proxy a) packageOptions

-- | Depending on 'PackageOptions' settings, 'mkModule' can
--   - make a declaration file containing encoders and decoders
--   - make an OCaml interface file
--   - make a Spec file that tests the encoders and decoders against a golden file and a servant server
class HasOCamlModule a where
  mkModule :: Proxy a -> PackageOptions -> Map.Map HaskellTypeMetaData OCamlTypeMetaData -> IO ()

instance (KnownSymbols modules, HasOCamlModule' api) => HasOCamlModule ((OCamlModule modules) :> api) where
  mkModule Proxy packageOptions deps = mkModule' (Proxy :: Proxy api) (symbolsVal (Proxy :: Proxy modules)) packageOptions deps

class HasOCamlModule' a where
  mkModule' :: Proxy a -> [String] -> PackageOptions -> Map.Map HaskellTypeMetaData OCamlTypeMetaData -> IO ()

instance (Typeable api, HasOCamlType api) => HasOCamlModule' api where
  mkModule' Proxy modules packageOptions ds = do
    if (length modules) == 0
      then fail "OCamlModule filePath needs at least one file name"
      else do
        createDirectoryIfMissing True (rootDir </> packageSrcDir packageOptions)
        let typF = (<> "\n") . T.intercalate "\n\n" $ mkType (Proxy :: Proxy api) (defaultOptions {dependencies = ds}) (createInterfaceFile packageOptions) (packageEmbeddedFiles packageOptions)
        T.writeFile (fp <.> "ml")  typF

        if createInterfaceFile packageOptions
        then do
          let intF = (<> "\n") . T.intercalate "\n\n" $ mkInterface (Proxy :: Proxy api) (defaultOptions {dependencies = ds}) (packageEmbeddedFiles packageOptions)
          T.writeFile (fp <.> "mli") intF
        else pure ()
        
        case mSpecOptions packageOptions of
          Nothing -> pure ()
          Just specOptions -> do
            let specF = (<> "\n") . T.intercalate "\n\n" $ mkSpec (Proxy :: Proxy api) (defaultOptions {dependencies = ds}) moduls (T.pack $ servantURL specOptions) (T.pack $ goldenDir specOptions) (packageEmbeddedFiles packageOptions)
            let specBody = if specF /= "" then ("let () =\n" <> specF) else ""
            createDirectoryIfMissing True (rootDir </> (specDir specOptions))
            T.writeFile (specFp <> "_spec" <.> "ml") specBody
    
            where
              specFp = rootDir </> (specDir specOptions) </> (foldl (</>) "" modules)
              moduls = T.pack <$> modules
    where
      rootDir = packageRootDir packageOptions
      fp = rootDir </> (packageSrcDir packageOptions) </> (foldl (</>) "" modules)


type family (HasOCamlTypeFlag a) :: Nat where
  HasOCamlTypeFlag (OCamlSubModule a :> b) = 4
  HasOCamlTypeFlag (a :> b) = 3
  HasOCamlTypeFlag (OCamlTypeInFile a b) = 2
  HasOCamlTypeFlag a = 1

class HasOCamlType api where
  mkType :: Proxy api -> Options -> Bool -> Map.Map String EmbeddedOCamlFiles -> [Text]
  mkInterface :: Proxy api -> Options -> Map.Map String EmbeddedOCamlFiles -> [Text]
  mkSpec :: Proxy api -> Options -> [Text] -> Text -> Text -> Map.Map String EmbeddedOCamlFiles -> [Text]

instance (HasOCamlTypeFlag a ~ flag, HasOCamlType' flag (a :: *)) => HasOCamlType a where
  mkType = mkType' (Proxy :: Proxy flag)
  mkInterface = mkInterface' (Proxy :: Proxy flag)
  mkSpec = mkSpec' (Proxy :: Proxy flag)

class HasOCamlType' (flag :: Nat) api where
  mkType' :: Proxy flag -> Proxy api -> Options -> Bool -> Map.Map String EmbeddedOCamlFiles -> [Text]
  mkInterface' :: Proxy flag -> Proxy api -> Options -> Map.Map String EmbeddedOCamlFiles -> [Text]
  mkSpec' :: Proxy flag -> Proxy api -> Options -> [Text] -> Text -> Text -> Map.Map String EmbeddedOCamlFiles -> [Text]

instance (KnownSymbol subModule, HasOCamlType b) => HasOCamlType' 4 (OCamlSubModule subModule :> b) where
  mkType' _ Proxy options interface fileMap = ["module " <> (T.pack $ symbolVal (Proxy :: Proxy subModule)) <> " = struct\n"] <> (mkType (Proxy :: Proxy b) options interface fileMap) <> ["\nend"]

  mkInterface' _ Proxy options fileMap = ["module " <> (T.pack $ symbolVal (Proxy :: Proxy subModule)) <> " : sig\n"] <> (mkInterface (Proxy :: Proxy b) options fileMap) <> ["\nend"]

  mkSpec' _ Proxy options modules url goldendir fileMap = mkSpec (Proxy :: Proxy b) options modules url goldendir fileMap

instance (HasOCamlType a, HasOCamlType b) => HasOCamlType' 3 (a :> b) where
  mkType' _ Proxy options interface fileMap = (mkType (Proxy :: Proxy a) options interface fileMap) <> (mkType (Proxy :: Proxy b) options interface fileMap)
  mkInterface' _ Proxy options fileMap = (mkInterface (Proxy :: Proxy a) options fileMap) <> (mkInterface (Proxy :: Proxy b) options fileMap)
  mkSpec' _ Proxy options modules url goldendir fileMap = (mkSpec (Proxy :: Proxy a) options modules url goldendir fileMap) <> (mkSpec (Proxy :: Proxy b) options modules url goldendir fileMap)


instance (Typeable a, KnownSymbol b) => HasOCamlType' 2 (OCamlTypeInFile a b) where
  mkType' _ Proxy _options _ fileMap = do
    let typeName = tyConName . typeRepTyCon $ typeRep (Proxy :: Proxy a)
    case eocDeclaration <$> Map.lookup typeName fileMap of
      Just v -> [decodeUtf8 v]
      _ -> fail $ "Unable to find the embedded file for " ++ typeName

  mkInterface' _ Proxy _options fileMap = do
    let typeName = tyConName . typeRepTyCon $ typeRep (Proxy :: Proxy a)
    case eocInterface <$> Map.lookup typeName fileMap of
      Just (Just v) -> [decodeUtf8 v]
      _ -> fail $ "Unable to find the embedded file for " ++ typeName

  mkSpec' _ _z _options modules url goldendir _fileMap = 
    [toOCamlSpec2 (T.pack . tyConName . typeRepTyCon $ typeRep (Proxy :: Proxy a)) modules url goldendir]


instance (OCamlType a) => HasOCamlType' 1 a where
  mkType' _ a options interface _ = body
    where
      body = [(toOCamlTypeSourceWith options a)
          , (toOCamlEncoderSourceWith (options {includeOCamlInterface = interface}) a)
          , (toOCamlDecoderSourceWith (options {includeOCamlInterface = interface}) a)]

  mkInterface' _ a options _ = body
    where
      body = [(toOCamlTypeSourceWith options a)
          , (toOCamlEncoderInterfaceWith options a) 
          , (toOCamlDecoderInterfaceWith options a)]

  mkSpec' _ a _options modules url goldendir _ = [toOCamlSpec a modules url goldendir]


-- build servant spec server

-- type level utility functions
            
-- module flag
-- | Get the number of declared types in an OCaml Module
--   Internal helper function.
type family (Flag a) :: Bool where
  Flag (a :> b)  = 'True -- case 1
--  Flag (OCamlPackage a)  = 'True -- case 2  
  Flag (OCamlModule a)  = 'True -- case 2
  Flag (OCamlSubModule a) = 'True
  Flag a     = 'False -- case 3

-- | Exposed type level function
class OCamlModuleTypeCount api where
  ocamlModuleTypeCount :: Proxy api -> Int
    
instance (Flag a ~ flag, OCamlModuleTypeCount' flag (a :: *)) => OCamlModuleTypeCount a where
  ocamlModuleTypeCount = ocamlModuleTypeCount' (Proxy :: Proxy flag)


-- | Internal helper function
class OCamlModuleTypeCount' (flag :: Bool) a where
  ocamlModuleTypeCount' :: Proxy flag -> Proxy a -> Int

-- case 0
instance (OCamlModuleTypeCount a, OCamlModuleTypeCount b) => OCamlModuleTypeCount' 'True (a :> b) where
  ocamlModuleTypeCount' _ Proxy = (ocamlModuleTypeCount (Proxy :: Proxy a)) + (ocamlModuleTypeCount (Proxy :: Proxy b))

-- case 1
-- do not count anything wrapped in OCamlModule
instance OCamlModuleTypeCount' 'True (OCamlModule modules) where
  ocamlModuleTypeCount' _ Proxy = 0

instance OCamlModuleTypeCount' 'True (OCamlSubModule subModules) where
  ocamlModuleTypeCount' _ Proxy = 0
-- case 2
-- everything else should count as one
instance OCamlModuleTypeCount' 'False a where
  ocamlModuleTypeCount' _ Proxy = 1


-- package flag
type family (FFlag a) :: Bool where
  FFlag (OCamlPackage a deps :> b)  = 'True -- case 0
  FFlag (a :<|> b)  = 'True -- case 0
  FFlag a     = 'False -- case 1


class OCamlPackageTypeCount modules where                
  ocamlPackageTypeCount :: Proxy modules -> [Int]

instance (FFlag a ~ flag, OCamlPackageTypeCount' flag (a :: *)) => OCamlPackageTypeCount a where
  ocamlPackageTypeCount = ocamlPackageTypeCount' (Proxy :: Proxy flag)

class OCamlPackageTypeCount' (flag :: Bool) a where
  ocamlPackageTypeCount' :: Proxy flag -> Proxy a -> [Int]

-- case 0
instance (OCamlPackageTypeCount b) => OCamlPackageTypeCount' 'True (OCamlPackage a deps :> b) where
  ocamlPackageTypeCount' _ Proxy = ocamlPackageTypeCount (Proxy :: Proxy b)

-- case 0
instance (OCamlModuleTypeCount a, OCamlPackageTypeCount b) => OCamlPackageTypeCount' 'True (a :<|> b) where
  ocamlPackageTypeCount' _ Proxy = (ocamlModuleTypeCount (Proxy :: Proxy a)) : (ocamlPackageTypeCount (Proxy :: Proxy b))

-- case 1
-- everything else should count as one
instance (OCamlModuleTypeCount a) => OCamlPackageTypeCount' 'False a where
  ocamlPackageTypeCount' _ Proxy = [ocamlModuleTypeCount (Proxy :: Proxy a)]


-- | OCamlSpecAPI is a servant type that repesents and OCamlModule and its
--   OCamlTypes. It automatically creates path names based on the name of the
--   OCamlModule and the name of each OCamlType.
--   OCamlModule '[] '["Core"] :> User :> Profile
--   /Core/User
--   /Core/Profile
--   OCamlModule '["Source"] '["Core"] :> User :> Profile
--   /Source/Core/User
--   /Source/Core/Profile

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



type OCamlSpecAPI (modules :: [Symbol]) (subModules :: [Symbol]) typ = ConcatSymbols (Insert (TypeName typ) (Append modules subModules)) (ReqBody '[JSON] [typ] :> Post '[JSON] [typ])

-- | abc
type family MkOCamlSpecAPI' modules subModules api :: * where
  MkOCamlSpecAPI' modules subModules ((OCamlSubModule restSubModules) :> a) = MkOCamlSpecAPI' modules (Append subModules '[restSubModules]) a
  MkOCamlSpecAPI' modules subModules (a :> b) = MkOCamlSpecAPI' modules subModules a :<|> MkOCamlSpecAPI' modules subModules b
  MkOCamlSpecAPI' modules subModules (OCamlTypeInFile api _typeFilePath) = OCamlSpecAPI modules subModules api
  MkOCamlSpecAPI' modules subModules api = OCamlSpecAPI modules subModules api
  
-- | ff
type family MkOCamlSpecAPI a :: * where
  MkOCamlSpecAPI (OCamlPackage a deps :> rest) = MkOCamlSpecAPI rest  
  MkOCamlSpecAPI ((OCamlModule modules :> api) :<|> rest) = MkOCamlSpecAPI' modules '[] api :<|> MkOCamlSpecAPI rest
  MkOCamlSpecAPI (OCamlModule modules :> api) = MkOCamlSpecAPI' modules '[] api


-- | 
mkServer :: forall ocamlPackage. (OCamlPackageTypeCount ocamlPackage, HasOCamlPackage ocamlPackage) => String -> Proxy ocamlPackage -> Q [Dec]
mkServer typeName Proxy = do
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


data EmbeddedOCamlFiles =
  EmbeddedOCamlFiles
    { eocDeclaration :: ByteString
    , eocInterface   :: Maybe ByteString
    , eocSpec        :: Maybe ByteString
    }

-- | $(mkFile (Proxy :: Proxy Package))

class HasEmbeddedFile api where
  mkFiles :: Bool -> Bool -> Proxy api -> Q Exp

instance (HasEmbeddedFile' api) => HasEmbeddedFile api where
  mkFiles includeInterface includeSpec Proxy = ListE <$> mkFiles' includeInterface includeSpec (Proxy :: Proxy api)


-- | Use Template Haskell to load OCaml files for an OCaml Module
class HasEmbeddedFile' api where
  mkFiles' :: Bool -> Bool -> Proxy api -> Q [Exp]

instance (HasEmbeddedFile' a, HasEmbeddedFile' b) => HasEmbeddedFile' (a :> b) where
  mkFiles' includeInterface includeSpec Proxy = (<>) <$> mkFiles' includeInterface includeSpec (Proxy :: Proxy a) <*> mkFiles' includeInterface includeSpec (Proxy :: Proxy b)

instance (Typeable a, KnownSymbol b) => HasEmbeddedFile' (OCamlTypeInFile a b) where
  mkFiles' includeInterface includeSpec Proxy = do
    let typeFilePath = symbolVal (Proxy :: Proxy b)
    let typeName = tyConName . typeRepTyCon $ typeRep (Proxy :: Proxy a)
    ml  <- embedFile (typeFilePath <.> "ml")

    mli <- if includeInterface
      then (\f -> AppE (ConE $ mkName "Just") f) <$> embedFile (typeFilePath <.> "mli")
      else pure $ ConE $ mkName "Nothing"

    spec <- if includeSpec
      then (\f -> AppE (ConE $ mkName "Just") f) <$> embedFile (typeFilePath <> "_spec" <.> "ml")
      else pure $ ConE $ mkName "Nothing"

    pure [TupE [LitE $ StringL typeName, AppE (AppE (AppE (ConE $ mkName "EmbeddedOCamlFiles") ml) mli) spec]]

instance {-# OVERLAPPABLE #-} HasEmbeddedFile' a where
  mkFiles' _ _ Proxy = pure []
