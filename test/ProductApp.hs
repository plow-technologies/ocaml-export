{-# LANGUAGE TemplateHaskell #-}

module ProductApp where

import OCaml.Export
import Product

$(mkOCamlSpecServer "ProductPackage" (Proxy :: Proxy ProductPackage))
