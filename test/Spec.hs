{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


import Control.Concurrent (forkIO)

import           Test.Hspec

import qualified File as File
import qualified FileApp as File

import qualified Product as Product
import           ProductApp

import qualified Sum as Sum
import           SumApp

import qualified Dependency as D

import qualified Options as Options


import Data.Proxy

import Network.Wai.Handler.Warp
import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.ByteString (unpack)
import Test.Aeson.GenericSpecs

import System.Process

import OCaml.Export

import Servant.API
import Servant
import Data.Text (Text)

import GHC.TypeLits
import Data.Constraint.Symbol (type (++))

import GHC.TypeLits
import GHC.TypeLits.List

logAllMiddleware :: Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
logAllMiddleware app req respond = do
    d <- requestBody req
    print $ requestHeaders req
    print $ d
    app req respond

-- npm start --prefix path/to/your/app

main :: IO ()
main = do
--  hspec Product.spec
--  hspec Sum.spec
--  hspec File.spec
--  hspec Options.spec
  hspec D.spec

  print $ mkOCamlTypeMetaData (Proxy :: Proxy Product.ProductPackage)

  _ <- forkIO $ run 8081 productPackageApp
  _ <- forkIO $ run 8082 sumPackageApp
  run 8083 File.filePackageApp

-- curl -i -d '"hi"' -H 'Content-type: application/json' -X POST http://localhost:8081/x/y
 
