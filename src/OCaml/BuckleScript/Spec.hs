{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OCaml.BuckleScript.Spec
  ( mkSampleServerAndGoldenSpec
  , mkGoldenDirSpec
  , toOCamlSpec
  , typeInFileToOCamlSpec
  ) where

-- base
import Data.Monoid ((<>))
-- text
import Data.Text (Text)
import qualified Data.Text as T
-- ocaml-export
import OCaml.BuckleScript.Types hiding (getOCamlValues)
import OCaml.Internal.Common
-- wl-pprint
import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), (</>))

mkSampleServerAndGoldenSpec :: OCamlDatatype -> Maybe Int -> [Text] -> Text -> Text -> Doc
mkSampleServerAndGoldenSpec (OCamlDatatype _ typeName constructors) mParameterRefCount modules url goldenDir =
  indent 2 $
    "AesonSpec.sampleGoldenAndServerSpec" <$$> (indent 2 $
          decoders
      <$$> encoders
      <$$> (dquotes down)
      <$$> (dquotes . stext $ ((url </> urlModul) </> typeName))
      <$$> (dquotes (stext $ goldenDir </> textUppercaseFirst typeName)) <> ";")
  where
    (tprDecoders, tprEncoders) = renderTypeParameterVals mParameterRefCount constructors
    decoders = if tprDecoders == "" then (smodul <> "decode" <> up) else ("(" <> smodul <> "decode" <> up <+> (stext tprDecoders) <> ")")
    encoders = if tprEncoders == "" then (smodul <> "encode" <> up) else ("(" <> smodul <> "encode" <> up <+> (stext tprEncoders) <> ")")
    up = stext . textUppercaseFirst $ typeName
    down = stext . textLowercaseFirst $ typeName
    modul = T.intercalate "." (textUppercaseFirst <$> modules) :: Text
    smodul = stext $ if modul /= "" then modul <> "." else ""
    urlModul = T.intercalate "/" (textUppercaseFirst <$> modules) :: Text
mkSampleServerAndGoldenSpec (OCamlPrimitive _) _ _mod _url _fp = ""

-- | test OCaml type serialization (bs-aeson) against Haskell produced golden files
mkGoldenDirSpec :: OCamlDatatype -> Maybe Int -> [Text] -> Text -> Doc
mkGoldenDirSpec (OCamlDatatype _ typeName constructors) mParameterRefCount modules goldenDir =
  indent 2 $
    "AesonSpec.goldenDirSpec" <$$> (indent 2 $
          decoders
      <$$> encoders
      <$$> (dquotes down)
      <$$> (dquotes (stext $ goldenDir </> textUppercaseFirst typeName)) <> ";")
  where
    (tprDecoders, tprEncoders) = renderTypeParameterVals mParameterRefCount constructors
    decoders = if tprDecoders == "" then (smodul <> "decode" <> up) else ("(" <> smodul <> "decode" <> up <+> (stext tprDecoders) <> ")")
    encoders = if tprEncoders == "" then (smodul <> "encode" <> up) else ("(" <> smodul <> "encode" <> up <+> (stext tprEncoders) <> ")")
    up = stext . textUppercaseFirst $ typeName
    down = stext . textLowercaseFirst $ typeName
    modul = T.intercalate "." (textUppercaseFirst <$> modules) :: Text
    smodul = stext $ if modul /= "" then modul <> "." else ""

mkGoldenDirSpec (OCamlPrimitive _) _ _ _ = ""


renderTypeParameterVals :: Maybe Int -> OCamlConstructor -> (Text,Text)
renderTypeParameterVals (Just count) _ = renderTypeParameterValsAux count
renderTypeParameterVals _ (OCamlValueConstructor vc) =
  renderTypeParameterValsAux . length . getTypeParameterRefNames . getOCamlValues $ vc
renderTypeParameterVals _ (OCamlSumOfRecordConstructor _ vc) =
  renderTypeParameterValsAux . length . getTypeParameterRefNames . getOCamlValues $ vc
renderTypeParameterVals _ _ = ("","")

renderTypeParameterValsAux :: Int -> (Text, Text)
renderTypeParameterValsAux size =
  if size > 0
  then
    ( T.intercalate " " $ mkDecoders <$> [0..(size-1)]
    , T.intercalate " " $ mkEncoders <$> [0..(size-1)]
    )
  else
    ("","")
  where
    mkDecoders _ = "AesonSpec.decodeIntWithResult"
    mkEncoders _ = "Aeson.Encode.int"
    
getOCamlValues :: ValueConstructor -> [OCamlValue]
getOCamlValues (NamedConstructor     _ value) = [value]
getOCamlValues (RecordConstructor    _ value) = [value]
getOCamlValues (MultipleConstructors cs)      = concat $ getOCamlValues <$> cs

toOCamlSpec :: OCamlType a => a -> [Text] -> Maybe Text -> Text -> Text
toOCamlSpec a modules mUrl fp =
  case mUrl of
    Just url -> pprinter $ mkSampleServerAndGoldenSpec (toOCamlType a) Nothing modules url fp
    Nothing  -> pprinter $ mkGoldenDirSpec (toOCamlType a) Nothing modules fp

-- | OCamlTypeInFile do not require an instance of OCamlType since they are
--   hand written OCaml files for Haskell types. Use typeable to get the
--   type name.
typeInFileToOCamlSpec :: OCamlType a => a -> Int -> [Text] -> Maybe Text -> Text -> Text
typeInFileToOCamlSpec a typeParameterRefCount modules mUrl fp =
  case mUrl of
    Just url ->
      pprinter $ mkSampleServerAndGoldenSpec (toOCamlType a) (Just typeParameterRefCount) modules url fp
    Nothing -> pprinter $ mkGoldenDirSpec (toOCamlType a) (Just typeParameterRefCount) modules fp
