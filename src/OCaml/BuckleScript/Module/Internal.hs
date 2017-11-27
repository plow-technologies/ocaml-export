{-|
Module      : OCaml.BuckleScript.Module.Internal
Description : Type level declarations to assist OCaml.BuckleScript.Module
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

module OCaml.BuckleScript.Module.Internal where

import Data.Proxy

import Servant.API ((:>))

-- type level
-- turn type Symbol into String
import GHC.TypeLits
import GHC.TypeLits.List
import Data.Type.Bool
import Data.Type.Equality


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
   Length2 (x :> xs) = 1 + Length xs
   Length2 a       = 1
