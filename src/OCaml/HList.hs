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
--  , HasGenericOCamlType (..)
--  , HasOCamlTypeInFile (..)

  , HasOCamlModule (..)
  , OCamlModule
  , OCamlTypeInFile
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
import OCaml.BuckleScript.Spec
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
  
instance (KnownSymbols a, KnownSymbols b, HasOCamlType api) => HasOCamlModule ((OCamlModule a b) ::> api) where
  mkModule Proxy rootdir = do
    if (length $ symbolsVal (Proxy :: Proxy a)) == 0
      then fail "OCamlModule filePath needs at least one file name"
      else do
        typF <- mkkType (Proxy :: Proxy api)
        intF <- mkkInterface (Proxy :: Proxy api)

        T.writeFile (fp <.> "ml")  typF
        T.writeFile (fp <.> "mli") intF
    where
      fp = rootdir </> (foldl (</>) "" $ symbolsVal (Proxy :: Proxy a))

  mkModuleWithSpec p rootdir specdir goldendir url = do
    mkModule p rootdir
    specF <- mkkSpec (Proxy :: Proxy api) (T.pack localModule) (T.pack url) (T.pack goldendir)  
    T.writeFile (fp <.> "ml")  ("let () =\n" <> specF)
    
    where
      fp = rootdir </> specdir </> (foldl (</>) "" $ symbolsVal (Proxy :: Proxy a))
      localModule = (foldl (<.>) "" $ symbolsVal (Proxy :: Proxy b))

-- module Hello = struct
-- end

-- | Combine `HasGenericOCamlType` and `HasOCamlTypeInFile`
class HasOCamlType api where
  mkkType :: Proxy api -> IO Text
  mkkInterface :: Proxy api -> IO Text
  mkkSpec :: Proxy api -> Text -> Text -> Text -> IO Text

instance (HasOCamlType a, HasOCamlType b) => HasOCamlType (a :> b) where
  mkkType Proxy = (<>) <$> (mkkType (Proxy :: Proxy a)) <*> (mkkType (Proxy :: Proxy b))
  mkkInterface Proxy = (<>) <$> (mkkInterface (Proxy :: Proxy a)) <*> (mkkInterface (Proxy :: Proxy b))
  mkkSpec Proxy modul url goldendir = (<>) <$> (mkkSpec (Proxy :: Proxy a) modul url goldendir) <*> (mkkSpec (Proxy :: Proxy b) modul url goldendir)

instance (KnownSymbol a, KnownSymbol b) => HasOCamlType (OCamlTypeInFile a b) where
  mkkType Proxy = T.readFile $ symbolVal (Proxy :: Proxy b) </> symbolVal (Proxy :: Proxy a) <.> "ml"
  mkkInterface Proxy = T.readFile $ symbolVal (Proxy :: Proxy b) </> symbolVal (Proxy :: Proxy a) <.> "mli"
  mkkSpec _ _ _ _ = pure ""

{-
instance {-# OVERLAPPABLE #-} OCamlType a => HasOCamlType a where
  mkkType a = pure $ toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderSource a <> "\n\n" <> toOCamlDecoderSource a <> "\n"
  mkkInterface a = pure $ toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderInterface a <> "\n\n" <> toOCamlDecoderInterface a <> "\n"
  mkkSpec a modul url goldendir = pure $ toOCamlSpec a modul url goldendir <> "\n"
-}


{-
-- | Read OCaml type declarations and interfaces from `ml` and `mli` files
class HasOCamlTypeInFile api where
  readType :: Proxy api -> IO Text
  readInterface :: Proxy api -> IO Text

instance (HasOCamlTypeInFile a, HasOCamlTypeInFile b) => HasOCamlTypeInFile (a :> b) where
  readType Proxy = (<>) <$> (readType (Proxy :: Proxy a)) <*> (readType (Proxy :: Proxy b))
  readInterface Proxy = (<>) <$> (readInterface (Proxy :: Proxy a)) <*> (readInterface (Proxy :: Proxy b))

instance (KnownSymbol b) => HasOCamlTypeInFile (OCamlTypeInFile a b) where
  readType Proxy = T.readFile $ symbolVal (Proxy :: Proxy b)
  readInterface Proxy = T.readFile $ symbolVal (Proxy :: Proxy b)


-- | Produce OCaml files for types that have OCamlType derived via GHC.Generics
class HasGenericOCamlType api where
  mkType :: Proxy api -> Text
  mkInterface :: Proxy api -> Text
  mkSpec :: Proxy api -> Text -> Text -> Text -> Text

instance (HasGenericOCamlType a, HasGenericOCamlType b) => HasGenericOCamlType (a :> b) where
  mkType Proxy = mkType (Proxy :: Proxy a) <> mkType (Proxy :: Proxy b)
  mkInterface Proxy = mkInterface (Proxy :: Proxy a) <> mkInterface (Proxy :: Proxy b)
  mkSpec Proxy modul url goldendir = (mkSpec (Proxy :: Proxy a) modul url goldendir) <> (mkSpec (Proxy :: Proxy b) modul url goldendir)

instance {-# OVERLAPPABLE #-} OCamlType a => HasGenericOCamlType a where
  mkType a = toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderSource a <> "\n\n" <> toOCamlDecoderSource a <> "\n"
  mkInterface a = toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderInterface a <> "\n\n" <> toOCamlDecoderInterface a <> "\n"
  mkSpec a modul url goldendir = toOCamlSpec a modul url goldendir <> "\n"
-}
