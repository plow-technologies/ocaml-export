{-# LANGUAGE OverloadedStrings #-}

module OCaml.BuckleScript.Spec where

import Data.Monoid
import Data.Text (Text)
import OCaml.Common
import OCaml.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

mkServerAndGoldenSpec :: OCamlDatatype -> Text -> Text -> Doc
mkServerAndGoldenSpec (OCamlDatatype typeName _) url fp =
  "AesonSpec.sampleGoldenAndServerSpec" <+> (up <> "." <> "decode" <> up)
                         <+> (up <> "." <> "encode" <> up)
                         <+> (dquotes down)
                         <+> (dquotes . stext $ url)
                         <+> (dquotes (stext fp <> up <> ".json")) <> ";"
  where
    up = stext . textUppercaseFirst $ typeName
    down = stext . textLowercaseFirst $ typeName
mkServerAndGoldenSpec (OCamlPrimitive _) _url _fp = ""

toOCamlSpec :: OCamlType a => a -> Text -> Text -> Text
toOCamlSpec a url fp =
  pprinter $ mkServerAndGoldenSpec (toOCamlType a) url fp

-- collection of texts
