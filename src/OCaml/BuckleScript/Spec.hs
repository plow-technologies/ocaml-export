{-# LANGUAGE OverloadedStrings #-}

module OCaml.BuckleScript.Spec where

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>), (</>))

-- ocaml-export
import OCaml.BuckleScript.Types
import OCaml.Common

mkSampleServerAndGoldenSpec :: OCamlDatatype -> [Text] -> Text -> Text -> Doc
mkSampleServerAndGoldenSpec (OCamlDatatype typeName _) modules url goldenDir =
  "  AesonSpec.sampleGoldenAndServerSpec" <+> (smodul <> "decode" <> up)
                         <+> (smodul <> "encode" <> up)
                         <+> (dquotes down)
                         <+> (dquotes . stext $ ((url </> urlModul) </> typeName))
                         <+> (dquotes (stext $ goldenDir </> (textUppercaseFirst typeName) <> ".json")) <> ";"
  where
    up = stext . textUppercaseFirst $ typeName
    down = stext . textLowercaseFirst $ typeName
    modul = T.intercalate "." (textUppercaseFirst <$> modules) :: Text
    smodul = stext $ if modul /= "" then modul <> "." else ""
    urlModul = T.intercalate "/" (textUppercaseFirst <$> modules) :: Text

mkSampleServerAndGoldenSpec (OCamlPrimitive _) _mod _url _fp = ""


toOCamlSpec :: OCamlType a => a -> [Text] -> Text -> Text -> Text
toOCamlSpec a modules url fp =
  pprinter $ mkSampleServerAndGoldenSpec (toOCamlType a) modules url fp
