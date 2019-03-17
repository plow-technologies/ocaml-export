{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import OCaml.Export
import qualified Shared.Types.Reason.Package as SharedTypes

main :: IO ()
main = do
  mkPackageWithGolden
    (Proxy :: Proxy SharedTypes.SharedTypesPackage)
    "test/golden"
    SharedTypes.fileMap

  where
  mkPackageWithGolden proxy dir fileMap = do
    mkGoldenFiles proxy 5 dir
    mkPackage proxy (PackageOptions "." "../frontend/src/Exported" fileMap True $ Just $ SpecOptions "../frontend/__tests__/Exported" dir Nothing)
