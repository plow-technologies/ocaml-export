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



type InAndOut a = (TypeName a) :> ReqBody '[JSON] a :> Post '[JSON] a

type family InAndOutAPI a :: * where
  InAndOutAPI (a :> b) = InAndOutAPI a :<|> InAndOutAPI b
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
                   
type family ConcatSymbols xs rhs :: * where
  ConcatSymbols (x ': xs) rhs = If ((Length xs) == 0) (x :> rhs) (x :> ConcatSymbols xs rhs)


          
-- infix 5 +++
{-                
type family ConcatSymbols xs :: * where
  ConcatSymbols (x ': xs) = If ((Length xs) == 0) (x) (x :> ConcatSymbols xs)

type family (s :: Symbol) +++ (t :: Symbol) :: Symbol where
  "" +++ s  = s
  s +++ "" = s


type family AppendSymbol (m :: Symbol) (n :: Symbol) :: Symbol where
  --AppendSymbol = someSymbolVal ((symbolVal (Proxy :: Proxy m)) ++ (symbolVal (Proxy :: Proxy n)))
  AppendSymbol = appendSymbol m n                  
                
type family AppendSymbols (ss :: [Symbol]) :: Symbol where
  AppendSymbols (s ': '[]) = s
  AppendSymbols (s ': ss) = AppendSymbol s (AppendSymbols ss)
-}
{-


appendSymbol :: (KnownSymbol a, KnownSymbol b) :- KnownSymbol (a ++ b)
appendSymbol = magicSSS (++)

class KnownSymbols (ss :: [Symbol]) where
    symbolsVal  :: p ss -> [String]
    symbolsList :: SymbolList ss

instance KnownSymbols '[] where
    symbolsVal  _ = []
    symbolsList    = ØSL

instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s ': ss) where
    symbolsVal  _ = symbolVal (Proxy :: Proxy s) : symbolsVal (Proxy :: Proxy ss)
    symbolsList   = Proxy :<$ symbolsList



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
