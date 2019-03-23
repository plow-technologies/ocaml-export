{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.OCaml.BuckleScript.Internal.Orphan where

import OCaml.BuckleScript.Types -- (OCamlDatatype, OCamlType, toOCamlType)
import Servant.API (NoContent, Headers, getResponse)

instance OCamlType OCamlDatatype where
  toOCamlType = id

instance OCamlType NoContent where
  toOCamlType _ = OCamlPrimitive OUnit

-- TODO: Generate OCaml functions that can handle the response headers. PRs
-- welcome!
instance (OCamlType a) => OCamlType (Headers ls a) where
  toOCamlType = toOCamlType . getResponse
