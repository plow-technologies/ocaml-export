{-# LANGUAGE TemplateHaskell #-}

module ProductApp where

import Data.Proxy
import OCaml.Export
import Product
import Servant

$(mkServer "ProductPackage" (Proxy :: Proxy ProductPackage))
