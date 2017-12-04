{-# LANGUAGE TemplateHaskell #-}

module FileApp where

import Data.Proxy
import OCaml.Export
import File
import Servant

$(mkServer "FilePackage" (Proxy :: Proxy FilePackage))

