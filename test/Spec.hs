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

import Control.Concurrent (forkIO, killThread, threadDelay)

import System.Process
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

main :: IO ()
main = do
  run 8081 Api.productApp
{-
  Product.mkGoldenFiles
  hspec Product.spec
  hspec Sum.spec
  hspec Options.spec
-}
{-
  servantThread <- forkIO $ run 8081 Api.productApp

  threadDelay 1000000

  -- callCommand "npm install --prefix test/interface/golden"
  -- callCommand "npm run build --prefix test/interface/golden"
  callCommand "npm run test --prefix test/interface/golden"

  killThread servantThread
-}
