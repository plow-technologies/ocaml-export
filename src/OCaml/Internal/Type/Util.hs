{-|
Module      : OCaml.Internal.Type.Util
Description : Type level functions for internal use
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

Use at your own risk. Contents are subject to change.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module OCaml.Internal.Type.Util
  ( TypeName
  , TypeNames
  , ConcatSymbols
  , Append
  , Insert
  , Length
  ) where

-- base
import GHC.Generics
import GHC.TypeLits

-- servant
import Servant.API ((:>))

-- text
import Data.Text (Text)

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

-- | Convert a list of types into a list of Symbols at the type level.
type family TypeNames a :: [Symbol] where
  TypeNames (a ': '[]) = '[TypeName a]
  TypeNames (a ': as) = TypeName a ': TypeNames as

-- | Concat a Symbol the end of a list of Symbols.
type family ConcatSymbols xs rhs where
  ConcatSymbols '[] rhs = rhs
  ConcatSymbols (x ': xs) rhs = x :> ConcatSymbols xs rhs

-- | Append two type level lists.
type family Append xy ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': (Append xs ys)

-- | Get the length of a type level list.
type family Length xs where
  Length '[]       = 0
  Length (x ': xs) = 1 + Length xs

-- | Insert type into type level list
type family Insert a xs :: [*] where
  Insert a '[]       = a ': '[]
  Insert a (x ': xs) = x ': (Insert a xs)
