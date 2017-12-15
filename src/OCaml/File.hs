{-|
Module      : OCaml.File
Description : Create OCaml files
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module OCaml.File where

-- base
import Data.ByteString (ByteString)
import Data.Monoid

-- directory
import System.Directory

-- text
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T

-- file-embed
import Data.FileEmbed (embedFile)


bsConfigFile :: ByteString
bsConfigFile = $(embedFile "bs-src/bsconfig.json")

packageFile :: ByteString
packageFile = $(embedFile "bs-src/package.json")

webpackConfigFile :: ByteString
webpackConfigFile = $(embedFile "bs-src/webpack.config.js")

data OCamlFile =
  OCamlFile
    { ocamlFilePath     :: FilePath
    , ocamlDeclarations :: [Text]
    }

data OCamlInterface =
  OCamlInterface
    { declars :: [Text]
    , inters :: [Text]
    , specs :: [Text]
    }

instance Monoid OCamlInterface where
  mappend a b = OCamlInterface (declars a <> declars b) (inters a <> inters b) (specs a <> specs b)
  mempty = OCamlInterface [] [] []

createOCamlFile :: FilePath -> OCamlFile -> IO ()
createOCamlFile rootDir ocamlFile = do
  createDirectoryIfMissing True rootDir
  let file = rootDir <> "/" <> ocamlFilePath ocamlFile <> ".ml"
      body = T.intercalate "\n\n" (ocamlDeclarations ocamlFile) <> "\n"    
  T.writeFile file body

createOCamlFiles :: FilePath ->  [OCamlFile] -> IO ()
createOCamlFiles root files = mapM_ (createOCamlFile root) files

createOCamlFileWithInterface :: FilePath -> FilePath -> FilePath -> OCamlInterface -> IO ()
createOCamlFileWithInterface rootDir specRootDir fileName ocamlInterface = do
  createDirectoryIfMissing True rootDir
  createDirectoryIfMissing True specRootDir
  let fp = rootDir <> "/" <> fileName
      body = T.intercalate "\n\n" (declars ocamlInterface) <> "\n"
      interfaceBody = T.intercalate "\n\n" (inters ocamlInterface) <> "\n"
  T.writeFile (fp <> ".ml") body
  T.writeFile (fp <> ".mli") interfaceBody
  if (length $ specs ocamlInterface) > 0
    then do
      let specfp = specRootDir <> "/" <> fileName
          specBody = "let () =\n" <> (T.intercalate "\n\n" (specs ocamlInterface) <> "\n")
      T.writeFile (specfp <> "_spec.ml") specBody
    else pure ()
  
createOCamlSpecFile :: FilePath -> FilePath -> Text -> IO ()
createOCamlSpecFile rootDir fileName ocamlSpec = do
  createDirectoryIfMissing True rootDir
  let fp = rootDir <> "/" <> fileName
      body = "let () =\n" <> ocamlSpec
  T.writeFile (fp <> "_spec.ml") body
