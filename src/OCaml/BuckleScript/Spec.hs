{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OCaml.BuckleScript.Spec
  ( mkSampleServerAndGoldenSpec
  , toOCamlSpec
  , typeInFileToOCamlSpec
  ) where

-- base
import Data.Monoid
-- text
import Data.Text (Text)
import qualified Data.Text as T
-- ocaml-export
import OCaml.BuckleScript.Types hiding (getOCamlValues)
import OCaml.Internal.Common
-- wl-pprint
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>), (</>))

mkSampleServerAndGoldenSpec :: OCamlDatatype -> [Text] -> Text -> Text -> Doc
mkSampleServerAndGoldenSpec (OCamlDatatype _ typeName constructors) modules url goldenDir =
  indent 2 $
    "AesonSpec.sampleGoldenAndServerSpec" <$$> (indent 2 $
          decoders
      <$$> encoders
      <$$> (dquotes down)
      <$$> (dquotes . stext $ ((url </> urlModul) </> typeName))
      <$$> (dquotes (stext $ goldenDir </> textUppercaseFirst typeName)) <> ";")
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
    ( T.intercalate " " $ mkDecoders <$> [0..(size-1)]
    , T.intercalate " " $ mkEncoders <$> [0..(size-1)]
    )
  else
    ("","")
  where
    size = length $ getTypeParameterRefNames ocamlValues
    mkDecoders _ = "AesonSpec.decodeIntWithResult"
    mkEncoders _ = "Aeson.Encode.int"
    -- body i = "(fun (x : Js_json.t) -> Aeson.Decode.wrapResult (Aeson.Decode.singleEnumerator Aeson.Helper.TypeParameterRef" <> (T.pack . show $ i) <> ") x)"
    -- body2 i = "(fun _x -> Aeson.Encode.singleEnumerator Aeson.Helper.TypeParameterRef" <> (T.pack . show $ i) <> ")"
    
getOCamlValues :: ValueConstructor -> [OCamlValue]
getOCamlValues (NamedConstructor     _ value) = [value]
getOCamlValues (RecordConstructor    _ value) = [value]
getOCamlValues (MultipleConstructors cs)      = concat $ getOCamlValues <$> cs

toOCamlSpec :: OCamlType a => a -> [Text] -> Text -> Text -> Text
toOCamlSpec a modules url fp =
  pprinter $ mkSampleServerAndGoldenSpec (toOCamlType a) modules url fp

-- | OCamlTypeInFile do not require an instance of OCamlType since they are
--   hand written OCaml files for Haskell types. Use typeable to get the
--   type name.
typeInFileToOCamlSpec :: Text -> [Text] -> Text -> Text -> Text
typeInFileToOCamlSpec aTyConName modules url fp =
  pprinter $ mkSampleServerAndGoldenSpec
    (OCamlDatatype (HaskellTypeMetaData aTyConName "" "")
       aTyConName $ OCamlValueConstructor $ NamedConstructor aTyConName $ OCamlEmpty)
    modules
    url
    fp

{-
toOCamlSpec2 :: Text -> [Text] -> Text -> Text -> Text
toOCamlSpec2 a modules url fp =
  pprinter $ mkSampleServerAndGoldenSpec (OCamlDatatype (HaskellTypeMetaData a "" "") a $ OCamlValueConstructor $ NamedConstructor a $ OCamlEmpty) modules url fp
-}
