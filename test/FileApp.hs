{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TemplateHaskell #-}

module FileApp where

import Data.Proxy
import OCaml.Export
import File
import Servant
import Language.Haskell.TH.Syntax (runQ, sequenceQ)

import Data.ByteString (ByteString)

$(mkServer "FilePackage" (Proxy :: Proxy FilePackage))

-- xxxx = $(mkFile (Proxy :: Proxy (OCamlTypeInFile Person "test/ocaml/Person")))
xxxx :: [(String,ByteString)]
xxxx = $(mkFiles True False (Proxy :: Proxy (OCamlTypeInFile Person "test/ocaml/Person")))
-- xxxx = runQ $ sequenceQ $ mkFile (Proxy :: Proxy (OCamlTypeInFile Person "test/ocaml/Person"))
-- xxxx = $(sequenceQ $ mkFile (Proxy :: Proxy (OCamlTypeInFile Person "test/ocaml/Person")))
