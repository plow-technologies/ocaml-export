{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import           Test.Hspec

import qualified Options as Options
-- import qualified Product as Product
import qualified Sum as Sum
import Product

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
-- $(mkServer "ProductPackage" (Proxy :: Proxy ProductPackage))

-- server3 :: Server (MkOCamlSpecAPI ProductPackage)
-- server3 = (pure :<|> pure) :<|> pure :<|> pure
-- put each module of pures in parens
--server3 :: Server (MkOCamlSpecAPI ProductPackage)
--server3 = (pure :<|> pure) :<|> (pure :<|> pure) :<|> (pure :<|> pure)

$(mkServer "ProductPackage" (Proxy :: Proxy ProductPackage))

main :: IO ()
main = do
 -- print $ ocamlModuleTypeCount (Proxy :: Proxy ProductPackage)
  print $ ocamlPackageTypeCount (Proxy :: Proxy ProductPackage)
  hspec spec
  hspec Sum.spec
  hspec Options.spec

  run 8081 productPackageApp
  -- run 8081 nextApp
  -- run 8081 Api.productApp

-- curl -i -d '[{"name":"Javier","id":35,"created":"2017-11-22T12:40:55.797664Z"}]' -H 'Content-type: application/json' -X POST http://localhost:8081/Next/Person
-- curl -i -d '[{"name":"Javier","id":35,"created":"2017-11-22T12:40:55.797664Z"}]' -H 'Content-type: application/json' -X POST http://localhost:8081/Person/Person
