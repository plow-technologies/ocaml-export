{-# LANGUAGE OverloadedStrings #-}

module OCaml.BuckleScript.Spec where

import Data.Monoid
import Data.Text (Text)
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>), (</>))

-- ocaml-export
import OCaml.BuckleScript.Types
import OCaml.Common

mkSampleServerAndGoldenSpec :: OCamlDatatype -> Text -> Text -> Text -> Doc
mkSampleServerAndGoldenSpec (OCamlDatatype typeName _) modul url goldenDir =
  "  AesonSpec.sampleGoldenAndServerSpec" <+> (smodul <> "decode" <> up)
                         <+> (smodul <> "encode" <> up)
                         <+> (dquotes down)
                         <+> (dquotes . stext $ (url </> (textLowercaseFirst typeName)))
                         <+> (dquotes (stext $ goldenDir </> (textUppercaseFirst typeName) <> ".json")) <> ";"
  where
    up = stext . textUppercaseFirst $ typeName
    down = stext . textLowercaseFirst $ typeName
    smodul = stext $ if modul /= "" then modul <> "." else ""

mkSampleServerAndGoldenSpec (OCamlPrimitive _) _mod _url _fp = ""


toOCamlSpec :: OCamlType a => a -> Text -> Text -> Text -> Text
toOCamlSpec a modul url fp =
  pprinter $ mkSampleServerAndGoldenSpec (toOCamlType a) modul url fp
