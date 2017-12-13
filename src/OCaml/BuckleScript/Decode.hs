{-|
Module      : OCaml.BuckleScript.Decode
Description : Make a JSON decoder for an OCamlDatatype that matches Generic aeson FromJSON
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module OCaml.BuckleScript.Decode
  ( toOCamlDecoderSourceWith
  , toOCamlDecoderInterfaceWith
  ) where

-- base
import Control.Monad.Reader
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Proxy (Proxy (..))        

-- aeson
import qualified Data.Aeson.Types as Aeson (Options(..))

-- containers
import qualified Data.Map.Strict as Map

-- text
import           Data.Text (Text)
import qualified Data.Text as T

-- wl-pprint
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

-- ocaml-export
import           OCaml.BuckleScript.Types hiding (getOCamlValues)
import           OCaml.Common


-- | Render OCamlDatatype into an OCaml declaration
class HasDecoder a where
  render :: Maybe OCamlTypeMetaData -> a -> Reader Options Doc

-- | Render OCamlPrimitive decoders and type parameters
class HasDecoderRef a where
  renderRef :: Maybe OCamlTypeMetaData -> a -> Reader Options Doc

-- | Render OCamlDataype into an OCaml interface
class HasDecoderInterface a where
  renderInterface :: Maybe OCamlTypeMetaData -> a -> Reader Options Doc

instance HasDecoderInterface OCamlDatatype where
  renderInterface mOCamlTypeMetaData datatype@(OCamlDatatype _ typeName constructor@(OCamlSumOfRecordConstructor _ (MultipleConstructors constructors))) = do
    fnName <- renderRef mOCamlTypeMetaData datatype
    let typeParameterInterfaces = linesBetween $ catMaybes (renderSumRecordInterface typeName . OCamlValueConstructor <$> constructors)
        (typeParameterEncoders, typeParameters) = renderTypeParameterVals constructor
    pure $ typeParameterInterfaces
      <$$> "val" <+> fnName <+> ":" <+> typeParameterEncoders <> "Js_json.t ->" <+> "(" <> typeParameters <> (stext . textLowercaseFirst $ typeName) <> ", string)" <+> "Js_result.t"
    

  renderInterface mOCamlTypeMetaData datatype@(OCamlDatatype _ typeName constructors) = do
    fnName <- renderRef mOCamlTypeMetaData datatype
    let (typeParameterEncoders, typeParameters) = renderTypeParameterVals constructors
    pure $ "val" <+> fnName <+> ":" <+> typeParameterEncoders <> "Js_json.t ->" <+> "(" <> typeParameters <> (stext . textLowercaseFirst $ typeName) <> ", string)" <+> "Js_result.t"

  renderInterface _ _ = pure ""


renderSumRecord :: Maybe OCamlTypeMetaData -> Text -> OCamlConstructor -> Reader Options (Maybe Doc)
renderSumRecord mOCamlTypeMetaData typeName (OCamlValueConstructor (RecordConstructor name value)) = do
  let sumRecordName = typeName <> name
  fnBody <- render mOCamlTypeMetaData (OCamlValueConstructor $ RecordConstructor (typeName <> name) value)
  ocamlInterface <- asks includeOCamlInterface
  if ocamlInterface
    then
      pure $ Just $ "let decode" <> stext sumRecordName <+> "json =" <$$> fnBody
    else
      pure $ Just $ "let decode" <> stext sumRecordName <+> "(json : Js_json.t)" <+> ":(" <> (stext $ textLowercaseFirst sumRecordName) <> ", string)" <+> "Js_result.t =" <$$> fnBody

renderSumRecord _ _ _ = return Nothing


instance HasDecoder OCamlDatatype where
  -- Sum with records
  render mOCamlTypeMetaData datatype@(OCamlDatatype _ typeName constructor@(OCamlSumOfRecordConstructor _ (MultipleConstructors constructors))) = do
    fnName <- renderRef mOCamlTypeMetaData datatype
    fnBody <- mapM (renderSum mOCamlTypeMetaData) (OCamlSumOfRecordConstructor typeName <$> constructors)
    typeParameterDeclarations <- linesBetween <$> catMaybes <$> sequence (renderSumRecord mOCamlTypeMetaData typeName . OCamlValueConstructor <$> constructors)
    ocamlInterface <- asks includeOCamlInterface    
    if ocamlInterface
      then do
       pure $ typeParameterDeclarations <$$>
         ("let" <+> fnName <+> "json =") <$$>
         (indent 2 ("match Aeson.Decode.(field \"tag\" string json) with" <$$> foldl1 (<$$>) fnBody <$$> fnFooter))
      else do
        let (typeParameterSignatures,typeParameters) = renderTypeParameters constructor
        pure $ typeParameterDeclarations <$$>
          ("let" <+> fnName <+> typeParameterSignatures <+> "(json : Js_json.t)" <+> typeParameters <> ":(" <> stext (textLowercaseFirst typeName) <> ", string) Js_result.t =") <$$>
          (indent 2 ("match Aeson.Decode.(field \"tag\" string json) with" <$$> foldl1 (<$$>) fnBody <$$> fnFooter))
    where
      fnFooter =
              "| err -> Js_result.Error (\"Unknown tag value found '\" ^ err ^ \"'.\")"
         <$$> "| exception Aeson.Decode.DecodeError message -> Js_result.Error message"


  render mOCamlTypeMetaData datatype@(OCamlDatatype _ name constructor@(OCamlEnumeratorConstructor constructors)) = do
    fnName <- renderRef mOCamlTypeMetaData datatype
    fnBody <- mapM (render mOCamlTypeMetaData) constructors
    ocamlInterface <- asks includeOCamlInterface
    if ocamlInterface
      then do
        pure $ "let" <+> fnName <+> "json ="
          <$$> indent 2 ("match Js_json.decodeString json with"
          <$$> foldl1 (<$$>) fnBody <$$> fnFooter fnName)
      else do
        let (typeParameterSignatures,typeParameters) = renderTypeParameters constructor
            returnType =  "(" <> typeParameters <> (stext . textLowercaseFirst $ name) <> ", string) Js_result.t ="
        pure $ "let" <+> fnName <+> typeParameterSignatures <+>"(json : Js_json.t) :" <> returnType
          <$$> indent 2 ("match Js_json.decodeString json with"
          <$$> foldl1 (<$$>) fnBody <$$> fnFooter fnName)
    where
      fnFooter fnName
        =    "| Some err -> Js_result.Error (\"" <> fnName <> ": unknown enumeration '\" ^ err ^ \"'.\")"
        <$$> "| None -> Js_result.Error \"" <> fnName <> ": expected a top-level JSON string.\""
      
  render mOCamlTypeMetaData datatype@(OCamlDatatype _ name constructor) = do
    fnName <- renderRef mOCamlTypeMetaData datatype
    fnBody <- render mOCamlTypeMetaData constructor
    ocamlInterface <- asks includeOCamlInterface
    if ocamlInterface
      then do
        let typeParameters = getTypeParameters constructor
            renderedTypeParameters = foldl (<>) "" $ stext <$> L.intersperse " " ((\t -> "decode" <> (textUppercaseFirst t)) <$> typeParameters)
        pure $ "let" <+> fnName <+> renderedTypeParameters <+> "json" <+> "=" <$$> fnBody
      else do
        let (typeParameterSignatures,typeParameters) = renderTypeParameters constructor
            returnType = "(" <> typeParameters <> (stext . textLowercaseFirst $ name) <> ", string) Js_result.t ="
        pure $ "let" <+> fnName <+> typeParameterSignatures <+> "(json : Js_json.t) :" <> returnType <$$> fnBody

  render mOCamlTypeMetaData (OCamlPrimitive primitive) = renderRef mOCamlTypeMetaData primitive

instance HasDecoderRef OCamlDatatype where
  -- this should only catch type parameters
  renderRef mOCamlTypeMetaData datatype@(OCamlDatatype typeRef name _) =
    if isTypeParameterRef datatype
    then
      pure $ parens ("fun a -> unwrapResult" <+> parens ("decode" <> (stext . textUppercaseFirst $ name) <+> "a"))
    else do
      case mOCamlTypeMetaData of
        Nothing -> pure $ "decode" <> (stext . textUppercaseFirst $ name)
        Just (OCamlTypeMetaData _ pFPath pSubMod) -> do
          ds <- asks dependencies
          case Map.lookup typeRef ds of
            Just (OCamlTypeMetaData tName cFPath cSubMod) -> pure $ "decode" <> (stext . textUppercaseFirst $ name)
            Nothing -> fail ("expected to find dependency:\n\n" ++ show typeRef ++ "\n\nin\n\n" ++ show ds)
      
  renderRef mOCamlTypeMetaData (OCamlPrimitive primitive) = renderRef mOCamlTypeMetaData primitive

instance HasDecoder OCamlConstructor where
  render mOCamlTypeMetaData (OCamlValueConstructor (NamedConstructor name value)) = do
    decoder <- render mOCamlTypeMetaData value
    return $ indent 2 $ "match Aeson.Decode." <> decoder <+> "json" <+> "with"
        <$$> "| v -> Js_result.Ok" <+> parens (stext name <+> "v")
        <$$> "| exception Aeson.Decode.DecodeError msg -> Js_result.Error (\"decode" <> (stext . textUppercaseFirst $ name) <> ": \" ^ msg)"
           
  render mOCamlTypeMetaData (OCamlValueConstructor (RecordConstructor name value)) = do
    decoders <- render mOCamlTypeMetaData value
    pure
         $ "  match Aeson.Decode."
      <$$> (indent 4 ("{" <+> decoders <$$> "}"))
      <$$> "  with"
      <$$> "  | v -> Js_result.Ok v"
      <$$> "  | exception Aeson.Decode.DecodeError message -> Js_result.Error (\"decode" <> (stext . textUppercaseFirst $ name) <> ": \" ^ message)"

  render mOCamlTypeMetaData (OCamlValueConstructor (MultipleConstructors constructors)) = do
    decoders <- mapM (renderSum mOCamlTypeMetaData . OCamlValueConstructor) constructors
    pure $ indent 2 "match Aeson.Decode.(field \"tag\" string json) with"
      <$$> indent 2 (foldl (<$$>) "" decoders)
      <$$> indent 2 fnFooter
    where
      fnFooter = "| err -> Js_result.Error (\"Unknown tag value found '\" ^ err ^ \"'.\")"
            <$$> "| exception Aeson.Decode.DecodeError message -> Js_result.Error message"

  render _ _ = pure ""

renderResult :: Maybe OCamlTypeMetaData -> Text -> OCamlDatatype -> Reader Options Doc
renderResult mOCamlTypeMetaData jsonFieldname (OCamlDatatype _ datatypeName _constructor) =
  pure
    $ "(field" <+> dquotes (stext jsonFieldname)
    <+> "(fun a -> unwrapResult (decode" <> (stext . textUppercaseFirst $ datatypeName) <+> "a)))"
renderResult mOCamlTypeMetaData jsonFieldname datatype@(OCamlPrimitive _primitive) = do
  dv <- renderRef mOCamlTypeMetaData datatype
  pure $ "(field" <+> dquotes (stext jsonFieldname) <+> dv <> ")"

instance HasDecoder OCamlValue where
  render mOCamlTypeMetaData (OCamlRef name) = do
    pure $ "(fun a -> unwrapResult (decode" <> (stext . textUppercaseFirst $ name) <+> "a))"

  render mOCamlTypeMetaData (OCamlPrimitiveRef primitive) = renderRef mOCamlTypeMetaData primitive

  render mOCamlTypeMetaData (OCamlTypeParameterRef name) =
    pure $ "(fun a -> unwrapResult (decode" <> (stext . textUppercaseFirst $ name) <+> "a))"

  render mOCamlTypeMetaData (Values x y) = do
    dx <- render mOCamlTypeMetaData x
    dy <- render mOCamlTypeMetaData y
    return $ dx <$$> ";" <+> dy

  render mOCamlTypeMetaData (OCamlField name (OCamlPrimitiveRef (OOption datatype))) = do
    ao <- asks aesonOptions
    let jsonFieldname = T.pack . Aeson.fieldLabelModifier ao . T.unpack $ name
    optional <- renderResult mOCamlTypeMetaData jsonFieldname datatype
    return $ (stext name) <+> "=" <+> "optional" <+> optional <+> "json"

  render mOCamlTypeMetaData (OCamlField name value) = do
    ao <- asks aesonOptions
    let jsonFieldname = T.pack . Aeson.fieldLabelModifier ao . T.unpack $ name    
    dv <- render mOCamlTypeMetaData value
    return $ (stext name) <+> "=" <+> "field" <+> dquotes (stext jsonFieldname) <+> dv <+> "json"

  render _ OCamlEmpty = pure (stext "")

instance HasDecoder EnumeratorConstructor where
  render mOCamlTypeMetaData (EnumeratorConstructor name) = pure $ "| Some \"" <> stext name <> "\" -> Js_result.Ok" <+> stext name
  
instance HasDecoderRef OCamlPrimitive where
  renderRef _ OUnit = pure $ parens "()"
  renderRef _ ODate = pure "date"
  renderRef _ OInt = pure "int"
  renderRef _ OBool = pure "bool"
  renderRef _ OChar = pure "char"
  renderRef _ OFloat = pure "float"
  renderRef _ OString = pure "string"

  renderRef _ (OList (OCamlPrimitive OChar)) = pure "string"

  renderRef mOCamlTypeMetaData (OList (OCamlDatatype _ name _)) =
    pure . parens $ "list" <+> (parens $ "fun a -> unwrapResult (decode" <> (stext . textUppercaseFirst $ name) <+> "a)")

  renderRef mOCamlTypeMetaData (OList datatype) = do
    dt <- renderRef mOCamlTypeMetaData datatype
    pure . parens $ "list" <+> dt

  renderRef _ (OOption _) = pure ""

  renderRef mOCamlTypeMetaData (OEither v0 v1) = do
    dv0 <- renderRef mOCamlTypeMetaData v0
    dv1 <- renderRef mOCamlTypeMetaData v1
    pure $ parens $ "either" <+> dv0 <+> dv1

  renderRef mOCamlTypeMetaData (OTuple2 v0 v1) = do
    dv0 <- renderRef mOCamlTypeMetaData v0
    dv1 <- renderRef mOCamlTypeMetaData v1
    pure $ parens $ "pair" <+> dv0 <+> dv1

  renderRef mOCamlTypeMetaData (OTuple3 v0 v1 v2) = do
    dv0 <- renderRef mOCamlTypeMetaData v0
    dv1 <- renderRef mOCamlTypeMetaData v1
    dv2 <- renderRef mOCamlTypeMetaData v2
    pure $ parens $ "tuple3" <+> dv0 <+> dv1 <+> dv2

  renderRef mOCamlTypeMetaData (OTuple4 v0 v1 v2 v3) = do
    dv0 <- renderRef mOCamlTypeMetaData v0
    dv1 <- renderRef mOCamlTypeMetaData v1
    dv2 <- renderRef mOCamlTypeMetaData v2
    dv3 <- renderRef mOCamlTypeMetaData v3
    pure $ parens $ "tuple4" <+> dv0 <+> dv1 <+> dv2 <+> dv3

  renderRef mOCamlTypeMetaData (OTuple5 v0 v1 v2 v3 v4) = do
    dv0 <- renderRef mOCamlTypeMetaData v0
    dv1 <- renderRef mOCamlTypeMetaData v1
    dv2 <- renderRef mOCamlTypeMetaData v2
    dv3 <- renderRef mOCamlTypeMetaData v3
    dv4 <- renderRef mOCamlTypeMetaData v4
    pure $ parens $ "tuple5" <+> dv0 <+> dv1 <+> dv2 <+> dv3 <+> dv4

  renderRef mOCamlTypeMetaData (OTuple6 v0 v1 v2 v3 v4 v5) = do
    dv0 <- renderRef mOCamlTypeMetaData v0
    dv1 <- renderRef mOCamlTypeMetaData v1
    dv2 <- renderRef mOCamlTypeMetaData v2
    dv3 <- renderRef mOCamlTypeMetaData v3
    dv4 <- renderRef mOCamlTypeMetaData v4
    dv5 <- renderRef mOCamlTypeMetaData v5
    pure $ parens $ "tuple6" <+> dv0 <+> dv1 <+> dv2 <+> dv3 <+> dv4 <+> dv5

-- Util
    
-- | "<name>" -> decode <name>
renderSumCondition :: T.Text -> Doc -> Reader Options Doc
renderSumCondition name contents = do
  ao <- asks aesonOptions
  let jsonConstructorName = T.pack . Aeson.constructorTagModifier ao . T.unpack $ name
  pure $ "|" <+> dquotes (stext jsonConstructorName) <+> "->" <$$>
    indent 3 contents

-- | Render a sum type constructor in context of a data type with multiple
-- constructors.
renderSum :: Maybe OCamlTypeMetaData -> OCamlConstructor -> Reader Options Doc
renderSum mo (OCamlValueConstructor (NamedConstructor name OCamlEmpty)) = renderSumCondition name ("Js_result.Ok" <+> stext name) 
renderSum mo (OCamlValueConstructor (NamedConstructor name v@(Values _ _))) = do
  val <- rArgs mo name v
  renderSumCondition name val

renderSum mo (OCamlValueConstructor (NamedConstructor name value)) = do
  ao <- asks aesonOptions
  let jsonConstructorName = T.pack . Aeson.constructorTagModifier ao . T.unpack $ name
  val <- render mo value
  renderSumCondition name $ parens
    ("match Aeson.Decode." <> parens ("field \"contents\"" <+> val <+> "json") <+> "with"
      <$$>
        indent 1
          (    "| v -> Js_result.Ok (" <> (stext name) <+> "v)"
          <$$> "| exception Aeson.Decode.DecodeError message -> Js_result.Error (\"" <> (stext jsonConstructorName) <> ": \" ^ message)"
          )           <> line
    )

renderSum mo (OCamlValueConstructor (RecordConstructor name value)) = do
  val <- render mo value
  renderSumCondition name val

renderSum mo (OCamlValueConstructor (MultipleConstructors constrs)) =
  foldl1 (<$+$>) <$> mapM (renderSum mo . OCamlValueConstructor) constrs

renderSum mo (OCamlSumOfRecordConstructor typeName (RecordConstructor name _value)) =
  renderOutsideEncoder typeName name

renderSum mo (OCamlSumOfRecordConstructor typeName (MultipleConstructors constrs)) =
  foldl1 (<$+$>) <$> mapM (renderSum mo) (OCamlSumOfRecordConstructor typeName <$> constrs)

renderSum _ (OCamlEnumeratorConstructor _) = pure "" -- handled elsewhere
renderSum _ _ = pure ""

renderOutsideEncoder :: Text -> Text -> Reader Options Doc
renderOutsideEncoder typeName name =
  pure $
        "|" <+> (dquotes . stext $ name) <+> "->"
   <$$> indent 3 ("(match" <+> "decode" <> (stext $ typeName <> textUppercaseFirst name) <+> "json" <+> "with"
   <$$> indent 1 ("| Js_result.Ok v -> Js_result.Ok" <+> (parens ((stext $ textUppercaseFirst name) <+> "v"))
   <$$> "| Js_result.Error message -> Js_result.Error" <+> (parens $ (dquotes $ "decode" <> stext typeName <> ": ") <+> "^ message"))
   <$$> ")")


flattenOCamlValue :: OCamlValue -> [OCamlValue]
flattenOCamlValue (Values l r) = flattenOCamlValue l ++ flattenOCamlValue r
flattenOCamlValue val = [val]

-- | Render the decoding of a constructor's arguments. Note the constructor must
-- be from a data type with multiple constructors and that it has multiple
-- constructors itself.
rArgs :: Maybe OCamlTypeMetaData -> Text -> OCamlValue -> Reader Options Doc
rArgs mo name vals = do
  v <- mk mo name 0 $ (Just <$> flattenOCamlValue vals) ++ [Nothing]
  pure $
    parens $ "match Aeson.Decode.(field \"contents\" Js.Json.decodeArray json) with"
    <$$> (indent 1 "| Some v ->"
    <$$> (indent 3 v)
    <$$> (indent 1 "| None -> Js_result.Error (\"" <> (stext name) <+> "expected an array.\")" )) <> line

mk :: Maybe OCamlTypeMetaData -> Text -> Int -> [Maybe OCamlValue] -> Reader Options Doc
mk mo name i [x] =
  case x of
    Nothing -> do
      let ts = foldl (<>) "" $ L.intersperse ", " ((stext . T.append "v" . T.pack . show) <$> [0..i-1])
      pure $ indent 1 $ "Js_result.Ok" <+> parens (stext name <+> parens ts)
    Just _ -> pure ""
mk mo name i (x:xs) =
  case x of
    Nothing -> mk mo name i xs
    Just x' -> do
      renderedVal <- render mo x'
      renderedInternal <- mk mo name (i+1) xs
      let iDoc = (stext . T.pack . show $ i)
      pure $ indent 1 $ "(match Aeson.Decode." <> renderedVal <+> ("v." <> parens iDoc) <+> "with"
        <$$>
          indent 1
            ("| v" <> iDoc <+> "->" <$$> (indent 2 renderedInternal)
            <$$> "| exception Aeson.Decode.DecodeError message -> Js_result.Error (\"" <> (stext name) <> ": \" ^ message)"
            )
        <$$> ")"

mk _mo _name _i [] = pure ""

renderTypeParameterValsAux :: [OCamlValue] -> (Doc,Doc)
renderTypeParameterValsAux ocamlValues =
  let typeParameterNames = (<>) "'" <$> getTypeParameterRefNames ocamlValues
      typeDecs = (foldl (<>) "" $ L.intersperse " -> " $ (\t -> "(Js_json.t -> (" <> (stext t) <> ", string) Js_result.t)") <$> typeParameterNames) <> " -> "
  in
      if length typeParameterNames > 0
      then
        if length typeParameterNames > 1
          then (typeDecs, foldl (<>) "" $ ["("] <> (L.intersperse ", " $ stext <$> typeParameterNames) <> [") "])
          else (typeDecs, stext . flip (<>) " " . head $ typeParameterNames)
      else ("","")

renderTypeParameterVals :: OCamlConstructor -> (Doc,Doc)
renderTypeParameterVals (OCamlValueConstructor vc) = renderTypeParameterValsAux $ getOCamlValues vc
renderTypeParameterVals (OCamlSumOfRecordConstructor _ vc) = renderTypeParameterValsAux $ getOCamlValues vc
renderTypeParameterVals _ = ("","")


renderTypeParametersAux :: [OCamlValue] -> (Doc,Doc)
renderTypeParametersAux ocamlValues = do
  let typeParameterNames = getTypeParameterRefNames ocamlValues
      typeDecs = (\t -> "(type " <> (stext t) <> ")") <$> typeParameterNames :: [Doc]
      encoderDecs  = (\t -> "(decode" <> (stext $ textUppercaseFirst t) <+> ":" <+> "Js_json.t -> (" <> (stext t) <> ", string) Js_result.t)" ) <$> typeParameterNames :: [Doc]
      typeParams = foldl (<>) "" $ if length typeParameterNames > 1 then  ["("] <> (L.intersperse ", " $ stext <$> typeParameterNames) <> [") "] else ((\x -> stext $ x <> " ") <$> typeParameterNames) :: [Doc]
  (foldl (<+>) "" (typeDecs ++ encoderDecs), typeParams )

getOCamlValues :: ValueConstructor -> [OCamlValue]
getOCamlValues (NamedConstructor     _ value) = [value]
getOCamlValues (RecordConstructor    _ value) = [value]
getOCamlValues (MultipleConstructors cs)      = concat $ getOCamlValues <$> cs

renderTypeParameters :: OCamlConstructor -> (Doc,Doc)
renderTypeParameters (OCamlValueConstructor vc) = renderTypeParametersAux $ getOCamlValues vc
renderTypeParameters (OCamlSumOfRecordConstructor _ vc) = renderTypeParametersAux $ getOCamlValues vc
renderTypeParameters _ = ("","")

-- | Render an OCaml val interface for a record constructor in a sum of records
renderSumRecordInterface :: Text -> OCamlConstructor -> Maybe Doc
renderSumRecordInterface typeName (OCamlValueConstructor (RecordConstructor name _value)) =
  Just $ "val decode" <> (stext $ typeName <> name) <+> ":" <+> "Js_json.t" <+> "->" <+> parens (stext (textLowercaseFirst $ typeName <> name) <> comma <+> "string") <+> "Js_result.t"
renderSumRecordInterface _ _ = Nothing


-- Exported

toOCamlDecoderInterfaceWith :: forall a. OCamlType a => Options -> a -> T.Text
toOCamlDecoderInterfaceWith options a =
  case toOCamlType (Proxy :: Proxy a) of
    OCamlDatatype haskellTypeMetaData _ _ ->
      case Map.lookup haskellTypeMetaData (dependencies options) of
        Just ocamlTypeMetaData -> pprinter $ runReader (renderInterface (Just ocamlTypeMetaData) (toOCamlType a)) options
        Nothing -> ""          
    _ -> pprinter $ runReader (renderInterface Nothing (toOCamlType a)) options

toOCamlDecoderSourceWith :: forall a. OCamlType a => Options -> a -> T.Text
toOCamlDecoderSourceWith options a =
  case toOCamlType (Proxy :: Proxy a) of
    OCamlDatatype haskellTypeMetaData _ _ ->
      case Map.lookup haskellTypeMetaData (dependencies options) of
        Just ocamlTypeMetaData -> pprinter $ runReader (render (Just ocamlTypeMetaData) (toOCamlType a)) options
        Nothing -> ""          
    _ -> pprinter $ runReader (render Nothing (toOCamlType a)) options
