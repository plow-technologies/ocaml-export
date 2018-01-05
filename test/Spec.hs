{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- base
import Control.Concurrent (forkIO)
import Data.Monoid ((<>))
-- hspec
import Test.Hspec
-- ocaml-export
import qualified Dependency as D
import qualified FileApp as File
import qualified Product as Product
import qualified ProductApp as Product
import qualified Sum as Sum
import SumApp
import OCaml.Export
-- warp
import Network.Wai.Handler.Warp

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
  
  _ <- forkIO $ run 8081 Product.productPackageApp
  _ <- forkIO $ run 8082 sumPackageApp
  run 8083 File.filePackageApp
