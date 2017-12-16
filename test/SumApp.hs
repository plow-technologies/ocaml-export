{-# LANGUAGE TemplateHaskell #-}

module SumApp where

import Data.Proxy
import OCaml.BuckleScript
import Sum

$(mkOCamlSpecServer "SumPackage" (Proxy :: Proxy SumPackage))
