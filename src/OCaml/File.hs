{-# LANGUAGE OverloadedStrings #-}

module OCaml.File where

import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import OCaml.Record
import OCaml.BuckleScript.Decode
import OCaml.BuckleScript.Encode
import OCaml.Common
import OCaml.Type
import           System.Directory

data OCamlFile =
  OCamlFile
    { ocamlFilePath     :: FilePath
    , ocamlDeclarations :: [Text]
    }
{-
data OCamlInterface =
  OCamlInterface
    { docamlFile :: OCamlFile
    , ocamlInterfaceDeclarations :: [Text]
    }
-}
data OCamlInterface =
  OCamlInterface
    { declars :: [Text]
    , inters :: [Text]
    }

instance Monoid OCamlInterface where
  mappend a b = OCamlInterface (declars a <> declars b) (inters a <> inters b)
  mempty = OCamlInterface [] []

mkOCamlInterface :: OCamlType a => a -> OCamlInterface
mkOCamlInterface a =
  OCamlInterface
    [toOCamlTypeSource a, toOCamlEncoderSourceWith (defaultOptions {includeOCamlInterface = True}) a, toOCamlDecoderSourceWith (defaultOptions {includeOCamlInterface = True}) a]
    [toOCamlTypeSource a, toOCamlEncoderInterface a, toOCamlDecoderInterface a]

createOCamlFile :: FilePath -> OCamlFile -> IO ()
createOCamlFile rootDir ocamlFile = do
  createDirectoryIfMissing True rootDir
  let file = rootDir <> "/" <> ocamlFilePath ocamlFile <> ".ml"
      body = T.intercalate "\n\n" (ocamlDeclarations ocamlFile) <> "\n"    
  T.writeFile file body

createOCamlFiles :: FilePath ->  [OCamlFile] -> IO ()
createOCamlFiles root files = mapM_ (createOCamlFile root) files

createOCamlFileWithInterface :: FilePath -> FilePath -> OCamlInterface -> IO ()
createOCamlFileWithInterface rootDir fileName ocamlInterface = do
  createDirectoryIfMissing True rootDir
  let fp = rootDir <> "/" <> fileName
      body = T.intercalate "\n\n" (declars ocamlInterface) <> "\n"
      interfaceBody = T.intercalate "\n\n" (inters ocamlInterface) <> "\n"
  T.writeFile (fp <> ".ml") body
  T.writeFile (fp <> ".mli") interfaceBody

{-
createOCamlFileWithInterface :: FilePath -> OCamlInterface -> IO ()
createOCamlFileWithInterface rootDir ocamlInterface = do
  createDirectoryIfMissing True rootDir
  let fp = rootDir <> "/" <> ocamlFilePath (docamlFile ocamlInterface)
      body = T.intercalate "\n\n" (ocamlDeclarations . docamlFile $ ocamlInterface) <> "\n"
      interfaceBody = T.intercalate "\n\n" (ocamlInterfaceDeclarations ocamlInterface) <> "\n"
  T.writeFile (fp <> ".ml") body
  T.writeFile (fp <> ".mli") interfaceBody
-}
