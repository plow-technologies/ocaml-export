{-|
Module      : OCaml.BuckleScript.Internal.Package
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

module OCaml.BuckleScript.Internal.Package
  (
  -- define an OCamlPackage with servant style types
    OCamlPackage
  , NoDependency

  
  , PackageOptions (..)
  , defaultPackageOptions
  , SpecOptions (..)
  , defaultSpecOptions

  , HasOCamlPackage (..)
  , HasOCamlModule (..)
  , HasOCamlTypeMetaData (..)

  ) where

-- base
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Data.Typeable (typeRep, Typeable, typeRepTyCon, tyConName, tyConModule, tyConPackage)
import GHC.TypeLits

-- containers
import qualified Data.Map.Strict as Map

-- directory
import System.Directory (createDirectoryIfMissing)

-- filepath
import System.FilePath.Posix ((</>), (<.>))

-- ocaml-export
import OCaml.Common hiding ((</>))
import OCaml.BuckleScript.Internal.Module
import OCaml.BuckleScript.Types

-- servant
import Servant.API ((:>), (:<|>))

-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- typelits-witnesses
import GHC.TypeLits.List



-- ==============================================
-- Types
-- ==============================================

data OCamlPackage (packageName :: Symbol) (packageDependencies :: [*])
  deriving Typeable

type NoDependency = '[]




-- ==============================================
-- Data Types
-- ==============================================

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


-- ==============================================
-- Type Level Functions
-- ==============================================

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





-- | Produce type meta data for an OCamlPackage and its dependencies
class HasOCamlTypeMetaData a where
  mkOCamlTypeMetaData :: Proxy a -> Map.Map HaskellTypeMetaData OCamlTypeMetaData

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

-- | empty list
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
