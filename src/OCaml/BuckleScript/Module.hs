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
  -- options
    PackageOptions (..)
  , defaultPackageOptions
  , SpecOptions (..)
  , defaultSpecOptions

  -- type classes
  , HasOCamlPackage (..)
  , HasOCamlModule (..)
  , HasOCamlType (..)
  , HasGenericOCamlType (..)
  , HasOCamlTypeInFile (..)

  -- types without constructors
  -- used to calculate types at the type level
  , OCamlModule
  , OCamlTypeInFile
  , ConcatSymbols

  -- servant functions
  -- automatically build a servant OCamlSpec Serve
  , OCamlPackageTypeCount (..)
  , OCamlModuleTypeCount (..)
  , OCamlSpecAPI
  , MkOCamlSpecAPI
  , mkServer

    -- re-export from Servant
  , (:>)
  , (:<|>)

  ) where

-- base
import qualified Data.List as L (intercalate)
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Typeable
import GHC.Generics

-- template-haskell
import Language.Haskell.TH

-- directory
import System.Directory (createDirectoryIfMissing)

-- filepath
import System.FilePath.Posix ((</>), (<.>))

-- ocaml-export
import OCaml.Common hiding ((</>))
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
import GHC.TypeLits
import GHC.TypeLits.List
import Data.Type.Bool
import Data.Type.Equality

-- servant
import Servant.API

-- wl-pprint
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>), (</>))


-- | Options for creating an OCaml package based on Haskell types.
data PackageOptions
  = PackageOptions
    { packageRootDir :: FilePath -- ^ root directory where all relatives directories will be placed.
    , packageSrcDir :: FilePath -- ^ location to place ml and mli files relative to 'packageRootDir'.
    , createInterfaceFile :: Bool -- ^ create an mli file if 'True'.
    , mSpecOptions :: Maybe SpecOptions -- ^ produce OCaml spec file if 'Just'.
    }

-- | Default 'PackageOptions'.
defaultPackageOptions :: PackageOptions
defaultPackageOptions = PackageOptions "ocaml-generic-package" "src" True (Just defaultSpecOptions)

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

-- | An OCamlModule as a Haskell type. 'filePath' is relative to a
--   root directory prvoiided in the 'mkPackage' function. Each symbol in
--   moduleName will is expanded into "module SymbolName = struct ... end".
data OCamlModule (filePath :: [Symbol]) (moduleName :: [Symbol])
  deriving Typeable

-- | A handwritten OCaml type, encoder and decoder from a file.
data OCamlTypeInFile (a :: Symbol) (filePath :: Symbol)
  deriving Typeable

-- | Iterate over a list of OCamlModule types that are concated with '(:<|>)',
--   to create a package.
class HasOCamlPackage a where
  mkPackage :: Proxy a -> PackageOptions -> IO ()

instance (HasOCamlPackage modul, HasOCamlPackage rest) => HasOCamlPackage (modul :<|> rest) where
  mkPackage Proxy packageOptions = do
    mkPackage (Proxy :: Proxy modul) packageOptions
    mkPackage (Proxy :: Proxy rest) packageOptions

instance {-# OVERLAPPABLE #-} (HasOCamlModule a) => HasOCamlPackage a where
  mkPackage Proxy packageOptions = mkModule (Proxy :: Proxy a) packageOptions

-- | Depending on 'PackageOptions' settings, 'mkModule' can
--   - make a declaration file containing encoders and decoders
--   - make an OCaml interface file
--   - make a Spec file that tests the encoders and decoders against a golden file and a servant server
class HasOCamlModule a where
  mkModule :: Proxy a -> PackageOptions -> IO ()
  
instance (KnownSymbols filePath, KnownSymbols moduleName, HasOCamlType api) => HasOCamlModule ((OCamlModule filePath moduleName) :> api) where
  mkModule Proxy packageOptions = do
    if (length $ symbolsVal (Proxy :: Proxy filePath)) == 0
      then fail "OCamlModule filePath needs at least one file name"
      else do
        createDirectoryIfMissing True (rootDir </> packageSrcDir packageOptions)
        typF <- localModuleDoc . (<> "\n") . T.intercalate "\n\n" <$> mkType (Proxy :: Proxy api) (createInterfaceFile packageOptions)
        T.writeFile (fp <.> "ml")  typF

        if createInterfaceFile packageOptions
          then do
            intF <- localModuleDoc . (<> "\n") . T.intercalate "\n\n" <$> mkInterface (Proxy :: Proxy api)
            T.writeFile (fp <.> "mli") intF
          else pure ()
        
        case mSpecOptions packageOptions of
          Nothing -> pure ()
          Just specOptions -> do
            specF <- (<> "\n") . T.intercalate "\n\n" <$> mkSpec (Proxy :: Proxy api) (T.pack localModule) (T.pack $ servantURL specOptions) (T.pack $ goldenDir specOptions)
            let specBody = if specF /= "" then ("let () =\n" <> specF) else ""
            createDirectoryIfMissing True (rootDir </> (specDir specOptions))
            T.writeFile (specFp <.> "ml") specBody
    
            where
              specFp = rootDir </> (specDir specOptions) </> (foldl (</>) "" $ symbolsVal (Proxy :: Proxy filePath))
              localModule = (L.intercalate "." $ symbolsVal (Proxy :: Proxy moduleName))
        
    where
      rootDir = packageRootDir packageOptions
      fp = rootDir </> (packageSrcDir packageOptions) </> (foldl (</>) "" $ symbolsVal (Proxy :: Proxy filePath))
      localModuleDoc body = pprinter $ foldr (\l r -> "module" <+> l <+> "= struct" <$$> indent 2 r <$$> "end") (stext body) (stext . T.pack <$> symbolsVal (Proxy :: Proxy moduleName))


-- | Combine `HasGenericOCamlType` and `HasOCamlTypeInFile`
class HasOCamlType api where
  mkType :: Proxy api -> Bool -> IO [Text]
  mkInterface :: Proxy api -> IO [Text]
  mkSpec :: Proxy api -> Text -> Text -> Text -> IO [Text]

instance (HasOCamlType a, HasOCamlType b) => HasOCamlType (a :> b) where
  mkType Proxy interface = (<>) <$> (mkType (Proxy :: Proxy a) interface) <*> (mkType (Proxy :: Proxy b) interface)
  mkInterface Proxy = (<>) <$> (mkInterface (Proxy :: Proxy a)) <*> (mkInterface (Proxy :: Proxy b))
  mkSpec Proxy modul url goldendir = (<>) <$> (mkSpec (Proxy :: Proxy a) modul url goldendir) <*> (mkSpec (Proxy :: Proxy b) modul url goldendir)

instance {-# OVERLAPPABLE #-} (HasOCamlTypeInFile (OCamlTypeInFile a b)) => HasOCamlType (OCamlTypeInFile a b) where
  mkType a _ = (:[]) <$> readType a
  mkInterface a = (:[]) <$> readInterface a
  mkSpec _ _ _ _ = pure []

instance {-# OVERLAPPABLE #-} (HasGenericOCamlType a) => HasOCamlType a where
  mkType a interface = pure $ mkGType a interface
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
  mkGType :: Proxy api -> Bool -> [Text]
  mkGInterface :: Proxy api -> [Text]
  mkGSpec :: Proxy api -> Text -> Text -> Text -> [Text]

instance (HasGenericOCamlType a, HasGenericOCamlType b) => HasGenericOCamlType (a :> b) where
  mkGType Proxy interface = mkGType (Proxy :: Proxy a) interface <> mkGType (Proxy :: Proxy b) interface 
  mkGInterface Proxy = mkGInterface (Proxy :: Proxy a) <> mkGInterface (Proxy :: Proxy b)
  mkGSpec Proxy modul url goldendir = (mkGSpec (Proxy :: Proxy a) modul url goldendir) <> (mkGSpec (Proxy :: Proxy b) modul url goldendir)

-- oOCamlEncoderSourceWith (options {includeOCamlInterface = True})
-- toOCamlDecoderSourceWith (options {includeOCamlInterface = True})
-- defaultOptions {includeOCamlInterface = }

instance {-# OVERLAPPABLE #-} OCamlType a => HasGenericOCamlType a where
  mkGType a interface = [toOCamlTypeSource a, toOCamlEncoderSourceWith (defaultOptions {includeOCamlInterface = interface}) a, toOCamlDecoderSourceWith (defaultOptions {includeOCamlInterface = interface}) a]
  mkGInterface a = [toOCamlTypeSource a, toOCamlEncoderInterface a, toOCamlDecoderInterface a]
  mkGSpec a modul url goldendir = [toOCamlSpec a modul url goldendir]





-- build servant spec server

-- type level utility functions

-- | Get the length of a type level list
type family Length xs where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs

-- | Insert type into type level list
type family Insert a xs where
   Insert a '[]       = (a ': '[])
   Insert a (a ': xs) = (a ': xs)
   Insert a (x ': xs) = x ': (Insert a xs)

-- module flag
-- | Get the number of declared types in an OCaml Module
--   Internal helper function.
type family (Flag a) :: Bool where
  Flag (a :> b)  = 'True -- case 1
  Flag (OCamlModule a b)  = 'True -- case 2
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
instance OCamlModuleTypeCount' 'True (OCamlModule a b) where
  ocamlModuleTypeCount' _ Proxy = 0

-- case 2
-- everything else should count as one
instance OCamlModuleTypeCount' 'False a where
  ocamlModuleTypeCount' _ Proxy = 1


-- package flag
type family (FFlag a) :: Bool where
  FFlag (a :<|> b)  = 'True -- case 0
  FFlag a     = 'False -- case 1


class OCamlPackageTypeCount modules where                
  ocamlPackageTypeCount :: Proxy modules -> [Int]

instance (FFlag a ~ flag, OCamlPackageTypeCount' flag (a :: *)) => OCamlPackageTypeCount a where
  ocamlPackageTypeCount = ocamlPackageTypeCount' (Proxy :: Proxy flag)

class OCamlPackageTypeCount' (flag :: Bool) a where
  ocamlPackageTypeCount' :: Proxy flag -> Proxy a -> [Int]

-- case 0
instance (OCamlModuleTypeCount a, OCamlPackageTypeCount b) => OCamlPackageTypeCount' 'True (a :<|> b) where
  ocamlPackageTypeCount' _ Proxy = (ocamlModuleTypeCount (Proxy :: Proxy a)) : (ocamlPackageTypeCount (Proxy :: Proxy b))

-- case 1
-- everything else should count as one
instance (OCamlModuleTypeCount a) => OCamlPackageTypeCount' 'False a where
  ocamlPackageTypeCount' _ Proxy = [ocamlModuleTypeCount (Proxy :: Proxy a)]



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


-- | OCamlSpecAPI is a servant type that repesents and OCamlModule and its
--   OCamlTypes. It automatically creates path names based on the name of the
--   OCamlModule and the name of each OCamlType.
--   OCamlModule '[] '["Core"] :> User :> Profile
--   /Core/User
--   /Core/Profile

--type OCamlPackageSpecAPI 

{-
type OCamlSpecAPI (modul :: [Symbol]) typ = ConcatSymbols (Insert (TypeName typ) modul) (ReqBody '[JSON] [typ] :> Post '[JSON] [typ])

type family MkOCamlSpecAPI'' modul a :: * where
  MkOCamlSpecAPI'' modul (a :> b) = MkOCamlSpecAPI'' modul a :<|> MkOCamlSpecAPI'' modul b
  MkOCamlSpecAPI'' modul a = OCamlSpecAPI modul a  

type family MkOCamlSpecAPI' a :: * where
  MkOCamlSpecAPI' (OCamlModule a b :> api) = MkOCamlSpecAPI'' b api

type family MkOCamlSpecAPI a :: * where
  MkOCamlSpecAPI ((OCamlModule (a b) :> api) :<|> rest) = MkOCamlSpecAPI'' b api :<|> MkOCamlSpecAPI rest
  MkOCamlSpecAPI (OCamlModule (a b) :> api) = MkOCamlSpecAPI'' b api
-}


type family ConcatSymbols xs rhs :: * where
  ConcatSymbols '[] rhs = rhs            
  ConcatSymbols (x ': xs) rhs = If ((Length xs) == 0) (x :> rhs) (x :> ConcatSymbols xs rhs)

type OCamlSpecAPI (modul :: [Symbol]) typ = ConcatSymbols (Insert (TypeName typ) modul) (ReqBody '[JSON] [typ] :> Post '[JSON] [typ])

type family MkOCamlSpecAPI' modul a :: * where
  MkOCamlSpecAPI' modul (a :> b) = MkOCamlSpecAPI' modul a :<|> MkOCamlSpecAPI' modul b
  MkOCamlSpecAPI' modul a = OCamlSpecAPI modul a  

type family MkOCamlSpecAPI a :: * where
  MkOCamlSpecAPI ((OCamlModule a b :> api) :<|> rest) = MkOCamlSpecAPI' b api :<|> MkOCamlSpecAPI rest
  MkOCamlSpecAPI (OCamlModule a b :> api) = MkOCamlSpecAPI' b api


-- | 
mkServer :: forall ocamlPackage. (OCamlPackageTypeCount ocamlPackage, HasOCamlPackage ocamlPackage) => String -> Proxy ocamlPackage -> Q [Dec]
mkServer typeName Proxy = do
  let sizes = ocamlPackageTypeCount (Proxy :: Proxy ocamlPackage)
  if (length sizes) < 1 || (not . and $ (> 0) <$> sizes)
    then fail $ "sizes must have at least one element and each element must be greater than zero: " <> show sizes
    else do
      let argss = (\size -> foldl (\l r -> ParensE $ UInfixE l (ConE $ mkName ":<|>") r) (VarE $ mkName "pure") (replicate (size-1) (VarE $ mkName "pure"))) <$> sizes
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

