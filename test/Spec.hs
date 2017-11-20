{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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

--test :: (forall (i :: k2). f0 i -> g0 i)
--                     -> HList (Map f0 as000) -> HList (Map g0 as000)
--test = hmap show

type SampleModule = Char :> Int -- :<|> String :<|> Int


type Dub = "Dubya" ::> SampleModule

type Yo = OCamlModule ["Here", "goodbye"] '[] ::> SampleModule

type Next = OCamlModule '["Next"] '[] ::> Product.Person :> Product.Company

main :: IO ()
main = do
--  print $ mkType (Proxy :: Proxy SampleModule)
--  print $ mkModule (Proxy :: Proxy Dub)
--  print $ mkType (Proxy :: Proxy Yo)
--  print "Hello"
--  print $ mkType (Proxy :: Proxy Next)
--  mkModule (Proxy :: Proxy Yo) "test/output"  
  mkModule (Proxy :: Proxy Next) "test/output"
  fail "adsf"
  
  -- run 8081 Api.productApp
{-
  Product.mkGoldenFiles
  hspec Product.spec
  hspec Sum.spec
  hspec Options.spec
-}
