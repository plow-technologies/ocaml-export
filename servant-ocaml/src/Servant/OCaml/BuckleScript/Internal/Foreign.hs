{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.OCaml.BuckleScript.Internal.Foreign where

import Data.Proxy (Proxy (Proxy))
import OCaml.BuckleScript.Types (OCamlDatatype, OCamlType, toOCamlType)
import Servant.Foreign
  (Foreign, GenerateList, HasForeign, HasForeignType, Req, listFromAPI, typeFor)

data LangOCaml

instance (OCamlType a) => HasForeignType LangOCaml OCamlDatatype a where
  typeFor _ _ _ =
    toOCamlType (Proxy :: Proxy a)

getEndpoints
  :: ( HasForeign LangOCaml OCamlDatatype api
     , GenerateList OCamlDatatype (Foreign OCamlDatatype api))
  => Proxy api
  -> [Req OCamlDatatype]
getEndpoints = listFromAPI (Proxy :: Proxy LangOCaml) (Proxy :: Proxy OCamlDatatype)
