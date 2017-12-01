{-# LANGUAGE OverloadedStrings #-}

module OCaml.BuckleScript.Spec
  ( mkSampleServerAndGoldenSpec
  , toOCamlSpec
  ) where

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>), (</>))

-- ocaml-export
import OCaml.BuckleScript.Types hiding (getOCamlValues)
import OCaml.Common

mkSampleServerAndGoldenSpec :: OCamlDatatype -> [Text] -> Text -> Text -> Doc
mkSampleServerAndGoldenSpec (OCamlDatatype typeName constructors) modules url goldenDir =
  "  AesonSpec.sampleGoldenAndServerSpec" <+> decoders
                         <+> encoders
                         <+> (dquotes down)
                         <+> (dquotes . stext $ ((url </> urlModul) </> typeName))
                         <+> (dquotes (stext $ goldenDir </> (textUppercaseFirst typeName) <> ".json")) <> ";"
  where
    (tprDecoders, tprEncoders) = renderTypeParameterVals constructors
    decoders = if tprDecoders == "" then (smodul <> "decode" <> up) else ("(" <> smodul <> "decode" <> up <+> (stext tprDecoders) <> ")")
    encoders = if tprEncoders == "" then (smodul <> "encode" <> up) else ("(" <> smodul <> "encode" <> up <+> (stext tprEncoders) <> ")")
    up = stext . textUppercaseFirst $ typeName
    down = stext . textLowercaseFirst $ typeName
    modul = T.intercalate "." (textUppercaseFirst <$> modules) :: Text
    smodul = stext $ if modul /= "" then modul <> "." else ""
    urlModul = T.intercalate "/" (textUppercaseFirst <$> modules) :: Text

mkSampleServerAndGoldenSpec (OCamlPrimitive _) _mod _url _fp = ""

renderTypeParameterVals :: OCamlConstructor -> (Text,Text)
renderTypeParameterVals (OCamlValueConstructor vc) = renderTypeParameterValsAux $ getOCamlValues vc
renderTypeParameterVals (OCamlSumOfRecordConstructor _ vc) = renderTypeParameterValsAux $ getOCamlValues vc
renderTypeParameterVals _ = ("","")

renderTypeParameterValsAux :: [OCamlValue] -> (Text, Text)
renderTypeParameterValsAux ocamlValues =
  if size > 0
  then
  ( T.intercalate " " $ body <$> [0..(size-1)]
  , T.intercalate " " $ body2 <$> [0..(size-1)]
  )
  else
    ("","")
  where
    size = length $ getTypeParameterRefNames ocamlValues
    body i = "(fun (x : Js_json.t) -> Aeson.Decode.wrapResult (Aeson.Decode.singleEnumerator Aeson.Helper.TypeParameterRef" <> (T.pack . show $ i) <> ") x)"
    body2 i = "(fun _x -> Aeson.Encode.singleEnumerator Aeson.Helper.TypeParameterRef" <> (T.pack . show $ i) <> ")"
    
getOCamlValues :: ValueConstructor -> [OCamlValue]
getOCamlValues (NamedConstructor     _ value) = [value]
getOCamlValues (RecordConstructor    _ value) = [value]
getOCamlValues (MultipleConstructors cs)      = concat $ getOCamlValues <$> cs

toOCamlSpec :: OCamlType a => a -> [Text] -> Text -> Text -> Text
toOCamlSpec a modules url fp =
  pprinter $ mkSampleServerAndGoldenSpec (toOCamlType a) modules url fp
