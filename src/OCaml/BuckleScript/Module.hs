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
  ( (:>)

  , HasOCamlType (..)
--  , HasGenericOCamlType (..)
--  , HasOCamlTypeInFile (..)

  , HasOCamlModule (..)
  , OCamlModule
  , OCamlTypeInFile

  , ConcatSymbols
  ) where

-- base
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Typeable

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
import qualified Servant.API as S
import Servant.API
import GHC.Generics

{-
data (path :: k) :> a
    deriving (Typeable)
infixr 4 :>
-}

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

instance (KnownSymbol a, KnownSymbol b) => HasOCamlType (OCamlTypeInFile a b) where
  mkType Proxy = T.readFile $ symbolVal (Proxy :: Proxy b) </> symbolVal (Proxy :: Proxy a) <.> "ml"
  mkInterface Proxy = T.readFile $ symbolVal (Proxy :: Proxy b) </> symbolVal (Proxy :: Proxy a) <.> "mli"
  mkSpec _ _ _ _ = pure ""

instance {-# OVERLAPPABLE #-} OCamlType a => HasOCamlType a where
  mkType a = pure $ toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderSource a <> "\n\n" <> toOCamlDecoderSource a <> "\n"
  mkInterface a = pure $ toOCamlTypeSource a <> "\n\n" <> toOCamlEncoderInterface a <> "\n\n" <> toOCamlDecoderInterface a <> "\n"
  mkSpec a modul url goldendir = pure $ toOCamlSpec a modul url goldendir <> "\n"


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

type InAndOut a = (TypeName a) S.:> S.ReqBody '[S.JSON] a S.:> S.Post '[S.JSON] a

type family InAndOutAPI a :: * where
  InAndOutAPI (a :> b) = InAndOutAPI a S.:<|> InAndOutAPI b
  InAndOutAPI a = InAndOut a

type family Intersperse (sep :: *) (xs :: [*]) where
  Intersperse _  '[] = '[]
  Intersperse sep (x ': xs) = x ': PrependToAll sep xs

type family PrependToAll (sep :: *) (as :: [*]) where
  PrependToAll _ '[] = '[]
  PrependToAll sep (x ': xs) = sep ': x ': PrependToAll sep xs

type family Length xs where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs
                   
type family ConcatSymbols xs where
  -- ConcatSymbols (x ': '[]) = x
  ConcatSymbols (x ': xs) = If ((Length xs) == 0) (x S.:> "") (x S.:> ConcatSymbols xs)
  {-
     case ((Length xs) == 0) of
       True -> x
       False -> x :> ConcatSymbols xs

  ConcatSymbols (x ': xs) = x :> "x" -- (ConcatSymbols xs)
ConcatSymbols (x ': xs) =
                case ((Length xs) == 0) of
                  True -> x else
                  False -> x :> ConcatSymbols xs
-}
--  ConcatSymbols (x ': xs) = x :> PrependToAllSymbols xs
 --x :> ConcatSymbols xs
--type family PrependToAllSymbols as :: * where
--  PrependToAllSymbols (x ': xs) = (S.:>) x S.:> PrependToAllSymbols xs

{-
intersperse             :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs
-}

                  
{-
type family UnMaybe a :: * where
  UnMaybe (Maybe a) = a
  UnMaybe a         = a
-}

-- type family InAndOutListWithRouteNamesAPI
-- type instance InAndOutListWithRouteNamesAPI [Int]              = Int         -- OK!
-- type instance InAndOutListWithRouteNamesAPI String             = Char        -- OK!
{-
type family InAndOutListWithRouteNamesAPI (xs :: [*]) (ys :: [Symbol]) where
  InAndOutListWithRouteNamesAPI (a ': '[]) (b ': '[]) = InAndOutListWithRouteNames a b
  InAndOutListWithRouteNamesAPI (a ': as)  (b ': bs)  = (InAndOutListWithRouteNames a b) :<|> InAndOutListWithRouteNamesAPI as bs
-}

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
