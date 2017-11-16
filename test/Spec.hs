import           Test.Hspec

import qualified Product as Product
import qualified Sum as Sum

import qualified Api as Api

import Data.Proxy

import Network.Wai.Handler.Warp
import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.ByteString (unpack)
import Test.Aeson.GenericSpecs
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

main :: IO ()
main = do
  {-
  mkGoldenFileForType 2 (Proxy :: Proxy Product.Person) "test/interface/golden/__tests__/golden"
  mkGoldenFileForType 2 (Proxy :: Proxy Product.Card) "test/interface/golden/__tests__/golden"
  run 8081 Api.productApp
  -}
  -- run 8081 (logStdout Api.productApp)
  --run 8081 (logAllMiddleware Api.productApp)
  Product.mkGoldenFiles
  hspec Product.spec
  --hspec Sum.spec
