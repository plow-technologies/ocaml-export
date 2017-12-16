{-# LANGUAGE TemplateHaskell #-}

module ProductApp where

import Data.Proxy
import OCaml.BuckleScript
import Product
import Servant

$(mkOCamlSpecServer "ProductPackage" (Proxy :: Proxy ProductPackage))
