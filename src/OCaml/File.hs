{-# LANGUAGE OverloadedStrings #-}

module OCaml.File where

import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           System.Directory

data OCamlFile =
  OCamlFile
    { ocamlFilePath     :: FilePath
    , ocamlDeclarations :: [Text]
    }

data OCamlInterface =
  OCamlInterface
    { docamlFile :: OCamlFile
    , ocamlInterfaceDeclarations :: [Text]
    }

createOCamlFile :: FilePath -> OCamlFile -> IO ()
createOCamlFile rootDir ocamlFile = do
  createDirectoryIfMissing True rootDir
  let file = rootDir <> "/" <> ocamlFilePath ocamlFile <> ".ml"
      body = T.intercalate "\n\n" (ocamlDeclarations ocamlFile) <> "\n"    
  T.writeFile file body

createOCamlFiles :: FilePath ->  [OCamlFile] -> IO ()
createOCamlFiles root files = mapM_ (createOCamlFile root) files

createOCamlFileWithInterface :: FilePath -> OCamlInterface -> IO ()
createOCamlFileWithInterface rootDir ocamlInterface = do
  createDirectoryIfMissing True rootDir
  let fp = rootDir <> "/" <> ocamlFilePath (docamlFile ocamlInterface)
      body = T.intercalate "\n\n" (ocamlDeclarations . docamlFile $ ocamlInterface) <> "\n"
      interfaceBody = T.intercalate "\n\n" (ocamlInterfaceDeclarations ocamlInterface) <> "\n"
  T.writeFile (fp <> ".ml") body
  T.writeFile (fp <> ".mli") interfaceBody
