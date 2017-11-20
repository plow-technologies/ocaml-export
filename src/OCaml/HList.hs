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


module OCaml.HList
  ( (:>)
  , (::>)
  , (:<|>)(..)
  , HasOCamlType (..)
  , HasOCamlModule (..)
  , OCamlModule
  , OCamlManualFile
  ) where

-- base
import Data.Monoid (Monoid (..))
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Typeable

-- filepath
import System.FilePath.Posix ((</>), (<.>))

-- ocaml-export
import OCaml.BuckleScript.Encode
import OCaml.BuckleScript.Decode
import OCaml.Record
import OCaml.Type

-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO     as T

-- type level
import GHC.TypeLits
import GHC.TypeLits.List




data (path :: k) :> a
    deriving (Typeable)
infixr 4 :>

data (path :: k) ::> a
    deriving (Typeable)
infixr 2 ::>

  
data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)

infixr 3 :<|>

instance (Semigroup a, Semigroup b) => Semigroup (a :<|> b) where
  (a :<|> b) <> (a' :<|> b') = (a <> a') :<|> (b <> b')

instance (Monoid a, Monoid b) => Monoid (a :<|> b) where
  mempty = mempty :<|> mempty
  (a :<|> b) `mappend` (a' :<|> b') = (a `mappend` a') :<|> (b `mappend` b')


{-
data QueryParam (sym :: Symbol) a
    deriving Typeable

data ReqBody (contentTypes :: [*]) a
    deriving (Typeable)

data OCamlModule a 
-}
--data OCamlPackage (rootDir :: Symbol)
--  deriving Typeable
-- data OCamlPackage (packageName :: Symbol)


data OCamlModule (filePath :: [Symbol]) (moduleName :: [Symbol])
  deriving Typeable

data OCamlManualFile a (filePath :: Symbol)
  deriving Typeable

-- Type with definition only
-- Type with definition and interface
-- either with Spec and Golden File
-- Type with Manual Definintion (with and without interface)

-- 
-- data OCamlPackage list of modules and specs


class HasOCamlModule a where
  mkModule :: Proxy a -> FilePath -> IO ()

{-
instance (KnownSymbol a, HasOCamlType api) => HasOCamlModule (a ::> api) where
  mkModule Proxy rootdir = do
    print ("mkModule ::>" :: Text)
    T.writeFile (fp <.> "ml")  typF
    T.writeFile (fp <.> "mli") intF
    where
      fp = rootdir </> (symbolVal (Proxy :: Proxy a))
      typF = mkType (Proxy :: Proxy api)
      intF = mkInterface (Proxy :: Proxy api)
-}
instance (KnownSymbols a, KnownSymbols b, HasOCamlType api) => HasOCamlModule ((OCamlModule a b) ::> api) where
  mkModule Proxy rootdir = do
    if (length $ symbolsVal (Proxy :: Proxy a)) == 0
      then fail "OCamlModule filePath needs at least one file name"
      else do
        T.writeFile (fp <.> "ml")  typF
        T.writeFile (fp <.> "mli") intF
    where
      fp = rootdir </> (foldl (</>) "" $ symbolsVal (Proxy :: Proxy a))
      typF = mkType (Proxy :: Proxy api)
      intF = mkInterface (Proxy :: Proxy api)
      
--instance {-# OVERLAPPABLE #-} OCamlType a => HasOCamlModule a where
--  mkModule Proxy _rootdir = do
--    print ("mkModule" :: Text)    

--instance {-# OVERLAPPABLE #-} HasOCamlModule a where
--  mkModule Proxy _rootdir = do
--    print ("mkModule" :: Text)    
    -- pure ()

{-
class HasOCamlModule a api where
  mkModule :: Proxy a -> Proxy api -> Text

instance (KnownSymbol a, HasOCamlType api) => HasOCamlModule a api where
  mkModule Proxy Proxy = (T.pack $ symbolVal (Proxy :: Proxy a) ) <> mkType (Proxy :: Proxy api)

mkOCamlInterfaceWithSpec :: OCamlType a => Text -> Text -> Text -> a -> OCamlInterface
mkOCamlInterfaceWithSpec url goldenDir modul a =
  OCamlInterface
    [toOCamlTypeSource a, toOCamlEncoderSourceWith (defaultOptions {includeOCamlInterface = True}) a, toOCamlDecoderSourceWith (defaultOptions {includeOCamlInterface = True}) a]
    [toOCamlTypeSource a, toOCamlEncoderInterface a, toOCamlDecoderInterface a]
    [toOCamlSpec a modul url goldenDir]



-}
  

class HasOCamlType api where
  mkType :: Proxy api -> Text
  mkInterface :: Proxy api -> Text

instance (KnownSymbols filePath, KnownSymbols moduleName, HasOCamlType api) => HasOCamlType (OCamlModule filePath moduleName ::> api) where
  mkType Proxy = (T.pack $ concat $ symbolsVal (Proxy :: Proxy filePath)) <> mkType (Proxy :: Proxy api)
  mkInterface Proxy = ""
  
--instance (HasOCamlType a, KnownSymbol filePath, HasOCamlType b) => HasOCamlType (OCamlManualFile a filePath :> b) where
--  mkType Proxy = (T.pack $ symbolVal (Proxy :: Proxy filePath)) <> mkType (Proxy :: Proxy b)
--  mkDecoderInterface Proxy = mkDecoderInterface (Proxy :: Proxy b)
--  mkEncoderInterface Proxy = mkEncoderInterface (Proxy :: Proxy b)

  
instance (HasOCamlType a, HasOCamlType b) => HasOCamlType (a :> b) where
  mkType Proxy = mkType (Proxy :: Proxy a) <> mkType (Proxy :: Proxy b)
  mkInterface Proxy = mkInterface (Proxy :: Proxy a) <> mkInterface (Proxy :: Proxy b)
  
instance {-# OVERLAPPABLE #-} OCamlType a => HasOCamlType a where
  mkType a = toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderSource a <> "\n\n" <> toOCamlDecoderSource a <> "\n"
  mkInterface a = toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderInterface a <> "\n\n" <> toOCamlDecoderInterface a <> "\n"
