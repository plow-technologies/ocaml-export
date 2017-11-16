{-# LANGUAGE OverloadedStrings #-}

module OCaml.BuckleScript.Spec where

import Data.Monoid
import Data.Text (Text)
import OCaml.Common
import OCaml.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))
{-
mkSampleServerAndGoldenSpec :: OCamlDatatype -> Text -> Text -> Doc
mkSampleServerAndGoldenSpec (OCamlDatatype typeName _) url fp =
  "AesonSpec.sampleGoldenAndServerSpec" <+> (up <> "." <> "decode" <> up)
                         <+> (up <> "." <> "encode" <> up)
                         <+> (dquotes down)
                         <+> (dquotes . stext $ url)
                         <+> (dquotes (stext fp <> up <> ".json")) <> ";"
  where
    up = stext . textUppercaseFirst $ typeName
    down = stext . textLowercaseFirst $ typeName
mkSampleServerAndGoldenSpec (OCamlPrimitive _) _url _fp = ""
-}

mkSampleServerAndGoldenSpec :: OCamlDatatype -> Text -> Text -> Text -> Doc
mkSampleServerAndGoldenSpec (OCamlDatatype typeName _) modul url goldenDir =
  "  AesonSpec.sampleGoldenAndServerSpec" <+> ((stext modul) <> "." <> "decode" <> up)
                         <+> ((stext modul) <> "." <> "encode" <> up)
                         <+> (dquotes down)
                         <+> (dquotes . stext $ (url <> "/" <> (textLowercaseFirst typeName)))
                         <+> (dquotes (stext goldenDir <> up <> ".json")) <> ";"
  where
    up = stext . textUppercaseFirst $ typeName
    down = stext . textLowercaseFirst $ typeName
mkSampleServerAndGoldenSpec (OCamlPrimitive _) _mod _url _fp = ""


toOCamlSpec :: OCamlType a => a -> Text -> Text -> Text -> Text
toOCamlSpec a modul url fp =
  pprinter $ mkSampleServerAndGoldenSpec (toOCamlType a) modul url fp

-- collection of texts
