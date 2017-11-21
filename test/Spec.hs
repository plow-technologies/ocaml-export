{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE OverloadedStrings #-}

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

-- type SampleModule = Char :> Int

-- type Dub = "Dubya" :> SampleModule

-- type Yo = OCamlModule ["Here", "goodbye"] '[] :> SampleModule

-- type Next = OCamlModule '["Next"] '[] :> Product.Person :> Product.Company

type Following = OCamlModule '["Following"] '[] :> (OCamlTypeInFile "Person" "test/input") :> Product.Company


-- type X = (ConcatSymbols '["a"]) :> Get '[JSON] Text
-- type X = "a" S.:> '[] S.:> S.Get '[S.JSON] Text
-- type X = "a" S.:> S.EmptyAPI S.:> S.Get '[S.JSON] Text

-- type X = (S.:>) ("a" S.:> "") S.Get '[S.JSON] Text

-- type X = ("a" S.:> "" S.:>) S.Get '[S.JSON] Text

-- type X = AppendSymbols '["a","b"] :> Get '[JSON] Text
-- type X = ("a" ++ "b") :> Get '[JSON] Text
-- type Y = ("a" ++ "b" ++ "c") :> Get '[JSON] Text
type X = (ConcatSymbols '["a", "b"] (Get '[JSON] Text))

-- type X = "a" :> (Get '[JSON] Text)

xServer :: Server X
xServer = pure "It worked"

xAPI :: Proxy X
xAPI = Proxy

xApp :: Application
xApp = serve xAPI xServer

main :: IO ()
main = do
--  print $ mkType (Proxy :: Proxy SampleModule)
--  print $ mkModule (Proxy :: Proxy Dub)
--  print $ mkType (Proxy :: Proxy Yo)
--  print "Hello"
--  print $ mkType (Proxy :: Proxy Next)
--  mkModule (Proxy :: Proxy Yo) "test/output"  
--  mkModule (Proxy :: Proxy Next) "test/output"
--  mkModuleWithSpec (Proxy :: Proxy Next) "test/output" "__tests__" "test/output/__tests__/golden" "localhost:8081"
  mkModuleWithSpec (Proxy :: Proxy Following) "test/output" "__tests__" "test/output/__tests__/golden" "localhost:8081"
  -- fail "adsf"
  run 8081 xApp
  -- run 8081 Api.productApp
{-
  Product.mkGoldenFiles
  hspec Product.spec
  hspec Sum.spec
  hspec Options.spec
-}
