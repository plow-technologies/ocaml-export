{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import           Test.Hspec

import qualified Options as Options
import qualified Product as Product
import qualified Sum as Sum

import qualified Api as Api

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
{-
Haskell Algebraic Data Types

data : product/record, sum
newtype : compiler optimized type wrapper
type : type synonym

single sum no parameter : top level string
multiple sum, no parameter : top level string
single sum, one or more parameters : top level array
one or more sums, one or more parameters: tag string, contents top level or array

when the sum type has no parameters, it is always top level
data OnOrOff = On | Off
when there is at least one parameter it uses the tag system
data OnOrOff = On Int | Off
-}
logAllMiddleware :: Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
logAllMiddleware app req respond = do
    d <- requestBody req
    print $ requestHeaders req
    print $ d
    app req respond

-- npm start --prefix path/to/your/app

type Next = OCamlModule '[] '["Next"] :> Product.Person :> Product.Company

$(mkServer "Next" (Proxy :: Proxy Next))

type Following = OCamlModule '["Following"] '["First","Second"] :> (OCamlTypeInFile "Person" "test/input") :> Product.Company

main :: IO ()
main = do
  print $ ocamlTypeCount (Proxy :: Proxy Following)
  mkModuleWithSpec (Proxy :: Proxy Following) "test/output" "__tests__" "test/output/__tests__/golden" "localhost:8081"
  run 8081 nextApp
  -- run 8081 Api.productApp
{-
  Product.mkGoldenFiles
  hspec Product.spec
  hspec Sum.spec
  hspec Options.spec
-}
-- curl -i -d '[{"name":"Javier","id":35,"created":"2017-11-22T12:40:55.797664Z"}]' -H 'Content-type: application/json' -X POST http://localhost:8081/Next/Person
