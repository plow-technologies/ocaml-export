{-# LANGUAGE TemplateHaskell #-}

module SumApp where

import Data.Proxy
import OCaml.Export
import Sum
import Servant

$(mkServer "SumPackage" (Proxy :: Proxy SumPackage))
