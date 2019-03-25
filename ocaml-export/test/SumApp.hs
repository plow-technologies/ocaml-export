{-# LANGUAGE TemplateHaskell #-}

module SumApp where

import OCaml.Export
import Sum

$(mkOCamlSpecServer "SumPackage" (Proxy :: Proxy SumPackage))
