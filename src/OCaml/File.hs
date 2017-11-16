{-|
Module      : OCaml.File
Description : Create OCaml files
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

-}

{-# LANGUAGE OverloadedStrings #-}

module OCaml.File where

-- base
import Data.Monoid

-- directory
import System.Directory

-- text
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T

-- ocaml-export
import OCaml.BuckleScript.Decode
import OCaml.BuckleScript.Encode
import OCaml.BuckleScript.Spec
import OCaml.Common
import OCaml.Record
import OCaml.Type


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

mkOCamlInterface :: OCamlType a => a -> OCamlInterface
mkOCamlInterface a =
  OCamlInterface
    [toOCamlTypeSource a, toOCamlEncoderSourceWith (defaultOptions {includeOCamlInterface = True}) a, toOCamlDecoderSourceWith (defaultOptions {includeOCamlInterface = True}) a]
    [toOCamlTypeSource a, toOCamlEncoderInterface a, toOCamlDecoderInterface a]
    []

mkOCamlInterfaceWithSpec :: OCamlType a => Text -> Text -> Text -> a -> OCamlInterface
mkOCamlInterfaceWithSpec url goldenDir modul a =
  OCamlInterface
    [toOCamlTypeSource a, toOCamlEncoderSourceWith (defaultOptions {includeOCamlInterface = True}) a, toOCamlDecoderSourceWith (defaultOptions {includeOCamlInterface = True}) a]
    [toOCamlTypeSource a, toOCamlEncoderInterface a, toOCamlDecoderInterface a]
    [toOCamlSpec a modul url goldenDir]


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
      T.writeFile (specfp <> ".ml") specBody
    else pure ()
  
createOCamlSpecFile :: FilePath -> FilePath -> Text -> IO ()
createOCamlSpecFile rootDir fileName ocamlSpec = do
  createDirectoryIfMissing True rootDir
  let fp = rootDir <> "/" <> fileName
      body = "let () =\n" <> ocamlSpec
  T.writeFile (fp <> ".ml") body
{-
createOCamlSpecFile :: FilePath -> FilePath -> [(OCamlDatatype,Text,Text)] -> IO ()
createOCamlSpecFile rootDir fileName typs = do
  createDirectoryIfMissing True rootDir
  let fp = rootDir <> "/" <> fileName
      body = T.intercalate "\n\n" $ pprinter . (\(value,url,filepath) -> mkSampleServerAndGoldenSpec value url filepath) <$> typs
  T.writeFile (fp <> ".ml") body
-}
