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

import Network.Wai.Handler.Warp
import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Process

-- base
import Control.Concurrent (forkIO)
import Data.Monoid ((<>))
import GHC.TypeLits

-- ByteString
import Data.ByteString (unpack)

-- hspec
import           Test.Hspec

-- hspec-aeson-golden
import Test.Aeson.GenericSpecs


-- servant
import Servant

-- servant-server
import Servant.API

-- text
import Data.Text (Text)

-- ocaml-export
import OCaml.Export
import qualified Dependency as D
import qualified File as File
import qualified FileApp as File
import qualified Options as Options
import qualified Product as Product
import ProductApp as Product
import qualified Sum as Sum
import SumApp

logAllMiddleware :: Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
logAllMiddleware app req respond = do
    d <- requestBody req
    print $ requestHeaders req
    print $ d
    app req respond

main :: IO ()
main = do
  hspec Product.spec
  hspec Sum.spec
  hspec File.spec
  hspec D.spec

  hspec $
    describe "mkOCamlTypeMetaData" $
      it "mkOCamlTypeMetaData on package A and B should equal mkOCamlTypeMetaData on B which has A as a dependency" $
        (mkOCamlTypeMetaData (Proxy :: Proxy Product.ProductPackage)) <> (mkOCamlTypeMetaData (Proxy :: Proxy D.DependencyPackageWithoutProduct))
          `shouldBe` mkOCamlTypeMetaData (Proxy :: Proxy D.DependencyPackage)

  print $ mkOCamlTypeMetaData (Proxy :: Proxy D.SubsPackage)
  
  _ <- forkIO $ run 8081 productPackageApp
  _ <- forkIO $ run 8082 sumPackageApp
  run 8083 File.filePackageApp
