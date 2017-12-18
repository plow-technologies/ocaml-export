{-|
Module      : OCaml.BuckleScript.Internal.Module
Description : Build OCaml Modules from Haskell Types
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module OCaml.BuckleScript.Internal.Module
  (
  -- define an OCamlModule with servant style types
    OCamlModule
  , OCamlSubModule
  , OCamlTypeInFile

  , EmbeddedOCamlFiles (..)

  , HasOCamlType (..)
  
  -- template haskell
  -- load OCamlTypeInFile files at compile time
  , HasEmbeddedFile (..)
  ) where

-- base
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Data.Typeable (typeRep, Typeable, typeRepTyCon, tyConName) -- , tyConModule, tyConPackage)
import GHC.TypeLits (Nat, Symbol, KnownSymbol, symbolVal)

-- bytestring
import Data.ByteString (ByteString)

-- containers
import qualified Data.Map.Strict as Map

-- directory
-- import System.Directory (createDirectoryIfMissing)

-- file-embed
import Data.FileEmbed (embedFile)

-- filepath
import System.FilePath.Posix ((<.>)) -- ((</>), (<.>))

-- ocaml-export
import OCaml.Internal.Common hiding ((</>))
import OCaml.BuckleScript.Decode
import OCaml.BuckleScript.Encode
import OCaml.BuckleScript.Record
import OCaml.BuckleScript.Spec
import OCaml.BuckleScript.Types

-- servant
import Servant.API ((:>))

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8)


-- ==============================================
-- Types
-- ==============================================

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


-- ==============================================
-- Data Types
-- ==============================================

-- | Store OCamlFileInType data.
data EmbeddedOCamlFiles =
  EmbeddedOCamlFiles
    { eocDeclaration :: ByteString
    , eocInterface   :: Maybe ByteString
    , eocSpec        :: Maybe ByteString
    }





-- ==============================================
-- Type Level Functions
-- ==============================================

-- | Convert a Haskell type into OCaml source code.
class HasOCamlType api where
  mkType :: Proxy api -> Options -> Bool -> Map.Map String EmbeddedOCamlFiles -> [Text]
  mkInterface :: Proxy api -> Options -> Map.Map String EmbeddedOCamlFiles -> [Text]
  mkSpec :: Proxy api -> Options -> [Text] -> Text -> Text -> Map.Map String EmbeddedOCamlFiles -> [Text]

instance (HasOCamlTypeFlag a ~ flag, HasOCamlType' flag (a :: *)) => HasOCamlType a where
  mkType = mkType' (Proxy :: Proxy flag)
  mkInterface = mkInterface' (Proxy :: Proxy flag)
  mkSpec = mkSpec' (Proxy :: Proxy flag)

type family (HasOCamlTypeFlag a) :: Nat where
  HasOCamlTypeFlag (OCamlSubModule a :> b) = 4
  HasOCamlTypeFlag (a :> b) = 3
  HasOCamlTypeFlag (OCamlTypeInFile a b) = 2
  HasOCamlTypeFlag a = 1

-- | Helper function to work avoid overlapped instances.
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

instance (Typeable a) => HasOCamlType' 2 (OCamlTypeInFile a b) where
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

  mkSpec' _ Proxy _options modules url goldendir _fileMap = 
    [typeInFileToOCamlSpec (T.pack . tyConName . typeRepTyCon $ typeRep (Proxy :: Proxy a)) modules url goldendir]


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




-- ==============================================
-- Template Haskell
-- ==============================================

-- | Use Template Haskell to load OCaml files for an OCaml Module at compile time.
--   '$(mkFile (Proxy :: Proxy Package))'.
class HasEmbeddedFile api where
  mkFiles :: Bool -> Bool -> Proxy api -> Q Exp

instance (HasEmbeddedFile' api) => HasEmbeddedFile api where
  mkFiles includeInterface includeSpec Proxy = ListE <$> mkFiles' includeInterface includeSpec (Proxy :: Proxy api)

-- | Help function for HasEmbeddedFile.
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
