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
{-# LANGUAGE TypeOperators #-}

module OCaml.BuckleScript.Decode
  ( toOCamlDecoderRef
  , toOCamlDecoderRefWith
  , toOCamlDecoderSource
  , toOCamlDecoderSourceWith
  , toOCamlDecoderInterface
  ) where

import           Control.Monad.Reader
import qualified Data.List as L
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           OCaml.Common
import           OCaml.Type hiding (getOCamlValues)
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))


-- | Render OCamlDatatype into an OCaml declaration
class HasDecoder a where
  render :: a -> Reader Options Doc

-- | Render OCamlPrimitive decoders and type parameters
class HasDecoderRef a where
  renderRef :: a -> Reader Options Doc

-- | Render OCamlDataype into an OCaml interface
class HasDecoderInterface a where
  renderInterface :: a -> Reader Options Doc

instance HasDecoderInterface OCamlDatatype where
  renderInterface datatype@(OCamlDatatype typeName constructor@(OCamlSumOfRecordConstructor _ (MultipleConstructors constructors))) = do
    fnName <- renderRef datatype
    let typeParameterInterfaces = linesBetween $ catMaybes (renderSumRecordInterface typeName . OCamlValueConstructor <$> constructors)
        (typeParameterEncoders, typeParameters) = renderTypeParameterVals constructor
    pure $ typeParameterInterfaces
      <$$> "val" <+> fnName <+> ":" <+> typeParameterEncoders <> "Js_json.t ->" <+> "(" <> typeParameters <> (stext . textLowercaseFirst $ typeName) <> ", string)" <+> "Js_result.t"
    

  renderInterface datatype@(OCamlDatatype typeName constructors) = do
    fnName <- renderRef datatype
    let (typeParameterEncoders, typeParameters) = renderTypeParameterVals constructors
    pure $ "val" <+> fnName <+> ":" <+> typeParameterEncoders <> "Js_json.t ->" <+> "(" <> typeParameters <> (stext . textLowercaseFirst $ typeName) <> ", string)" <+> "Js_result.t"

  renderInterface _ = pure ""


renderSumRecord :: Text -> OCamlConstructor -> Reader Options (Maybe Doc)
renderSumRecord typeName (OCamlValueConstructor (RecordConstructor name value)) = do
  let sumRecordName = typeName <> name
  fnBody <- render (OCamlValueConstructor $ RecordConstructor (typeName <> name) value)
  ocamlInterface <- asks includeOCamlInterface
  if ocamlInterface
    then
      pure $ Just $ "let decode" <> stext sumRecordName <+> "json =" <$$> fnBody
    else
      pure $ Just $ "let decode" <> stext sumRecordName <+> "(json : Js_json.t)" <+> ":(" <> (stext $ textLowercaseFirst sumRecordName) <> ", string)" <+> "Js_result.t =" <$$> fnBody

renderSumRecord _ _ = return Nothing


instance HasDecoder OCamlDatatype where
  -- Sum with records
  render datatype@(OCamlDatatype typeName constructor@(OCamlSumOfRecordConstructor _ (MultipleConstructors constructors))) = do
    fnName <- renderRef datatype
    fnBody <- mapM renderSum (OCamlSumOfRecordConstructor typeName <$> constructors)
    typeParameterDeclarations <- linesBetween <$> catMaybes <$> sequence (renderSumRecord typeName . OCamlValueConstructor <$> constructors)
    ocamlInterface <- asks includeOCamlInterface    
    if ocamlInterface
      then do
       pure $ typeParameterDeclarations <$$>
         ("let" <+> fnName <+> "json =") <$$>
         (indent 2 ("match Json.Decode.(field \"tag\" string json) with" <$$> foldl1 (<$$>) fnBody <$$> fnFooter))
      else do
        let (typeParameterSignatures,typeParameters) = renderTypeParameters constructor
        pure $ typeParameterDeclarations <$$>
          ("let" <+> fnName <+> typeParameterSignatures <+> "(json : Js_json.t)" <+> typeParameters <> ":(" <> stext (textLowercaseFirst typeName) <> ", string) Js_result.t =") <$$>
          (indent 2 ("match Json.Decode.(field \"tag\" string json) with" <$$> foldl1 (<$$>) fnBody <$$> fnFooter))
    where
      fnFooter =
              "| err -> Js_result.Error (\"Unknown tag value found '\" ^ err ^ \"'.\")"
         <$$> "| exception Json.Decode.DecodeError message -> Js_result.Error message"


  render datatype@(OCamlDatatype name constructor@(OCamlEnumeratorConstructor constructors)) = do
    fnName <- renderRef datatype
    fnBody <- mapM render constructors
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
      
  render datatype@(OCamlDatatype name constructor) = do
    fnName <- renderRef datatype
    fnBody <- render constructor
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

  render (OCamlPrimitive primitive) = renderRef primitive

instance HasDecoderRef OCamlDatatype where
  -- this should only catch type parameters
  renderRef datatype@(OCamlDatatype name _) =
    if isTypeParameterRef datatype
    then
      pure $ parens ("fun a -> unwrapResult" <+> parens ("decode" <> (stext . textUppercaseFirst $ name) <+> "a"))
    else
      pure $ "decode" <> (stext . textUppercaseFirst $ name)
      
  renderRef (OCamlPrimitive primitive) = renderRef primitive

instance HasDecoder OCamlConstructor where
  render (OCamlValueConstructor (NamedConstructor name value)) = do
    decoder <- render value
    return $ indent 2 $ "match Json.Decode." <> decoder <+> "json" <+> "with"
        <$$> "| v -> Js_result.Ok" <+> parens (stext name <+> "v")
        <$$> "| exception Json.Decode.DecodeError msg -> Js_result.Error (\"decode" <> (stext . textUppercaseFirst $ name) <> ": \" ^ msg)"
           
  render (OCamlValueConstructor (RecordConstructor name value)) = do
    decoders <- render value
    pure
         $ "  match Json.Decode."
      <$$> (indent 4 ("{" <+> decoders <$$> "}"))
      <$$> "  with"
      <$$> "  | v -> Js_result.Ok v"
      <$$> "  | exception Json.Decode.DecodeError message -> Js_result.Error (\"decode" <> (stext . textUppercaseFirst $ name) <> ": \" ^ message)"

  render (OCamlValueConstructor (MultipleConstructors constructors)) = do
    decoders <- mapM (renderSum . OCamlValueConstructor) constructors
    pure $ indent 2 "match Json.Decode.(field \"tag\" string json) with"
      <$$> indent 2 (foldl (<$$>) "" decoders)
      <$$> indent 2 fnFooter
    where
      fnFooter = "| err -> Js_result.Error (\"Unknown tag value found '\" ^ err ^ \"'.\")"
            <$$> "| exception Json.Decode.DecodeError message -> Js_result.Error message"

  render _ = pure ""

instance HasDecoder OCamlValue where
  render (OCamlRef name) = do
    pure $ "(fun a -> unwrapResult (decode" <> (stext . textUppercaseFirst $ name) <+> "a))"

  render (OCamlPrimitiveRef primitive) = renderRef primitive

  render (OCamlTypeParameterRef name) =
    pure $ "(fun a -> unwrapResult (decode" <> (stext . textUppercaseFirst $ name) <+> "a))"

  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> ";" <+> dy

  -- renderRef has separate rules for type parameters and non primitive types
  -- however in the case of OOption, they should be rendered the same way
  -- to remove Js_result.t
  render (OCamlField name (OCamlPrimitiveRef (OOption (OCamlDatatype datatypeName _)))) =
    pure $ (stext name) <+> "=" <+> "optional (field" <+> dquotes (stext name)
      <+> "(fun a -> unwrapResult (decode" <> (stext . textUppercaseFirst $ datatypeName) <+> "a)))" <+> "json"

  render (OCamlField name (OCamlPrimitiveRef (OOption datatype))) = do
    dv <- renderRef datatype
    return $ (stext name) <+> "=" <+> "optional (field" <+> dquotes (stext name) <+> dv <> ")" <+> "json"

  render (OCamlField name value) = do
    dv <- render value
    return $ (stext name) <+> "=" <+> "field" <+> dquotes (stext name) <+> dv <+> "json"

  render OCamlEmpty = pure (stext "")

instance HasDecoder EnumeratorConstructor where
  render (EnumeratorConstructor name) = pure $ "| Some \"" <> stext name <> "\" -> Js_result.Ok" <+> stext name
  
instance HasDecoderRef OCamlPrimitive where
  renderRef OUnit = pure $ parens "()"
  renderRef ODate = pure "date"
  renderRef OInt = pure "int"
  renderRef OBool = pure "bool"
  renderRef OChar = pure "char"
  renderRef OFloat = pure "float"
  renderRef OString = pure "string"

  renderRef (OList (OCamlPrimitive OChar)) = pure "string"

  renderRef (OList (OCamlDatatype name _)) =
    pure . parens $ "list" <+> (parens $ "fun a -> unwrapResult (decode" <> (stext . textUppercaseFirst $ name) <+> "a)")

  renderRef (OList datatype) = do
    dt <- renderRef datatype
    pure . parens $ "list" <+> dt

  renderRef (ODict key value) = do
    d <- renderRef (OList (OCamlPrimitive (OTuple2 (OCamlPrimitive key) value)))
    pure . parens $ "map Dict.fromList" <+> d

  renderRef (OOption datatype) = do
    dt <- renderRef datatype
    pure . parens $ "maybe" <+> dt

  renderRef (OTuple2 v0 v1) = do
    dv0 <- renderRef v0
    dv1 <- renderRef v1
    pure $ parens $ "pair" <+> dv0 <+> dv1

  renderRef (OTuple3 v0 v1 v2) = do
    dv0 <- renderRef v0
    dv1 <- renderRef v1
    dv2 <- renderRef v2
    pure $ parens $ "tuple3" <+> dv0 <+> dv1 <+> dv2

  renderRef (OTuple4 v0 v1 v2 v3) = do
    dv0 <- renderRef v0
    dv1 <- renderRef v1
    dv2 <- renderRef v2
    dv3 <- renderRef v3
    pure $ parens $ "tuple4" <+> dv0 <+> dv1 <+> dv2 <+> dv3

  renderRef (OTuple5 v0 v1 v2 v3 v4) = do
    dv0 <- renderRef v0
    dv1 <- renderRef v1
    dv2 <- renderRef v2
    dv3 <- renderRef v3
    dv4 <- renderRef v4
    pure $ parens $ "tuple5" <+> dv0 <+> dv1 <+> dv2 <+> dv3 <+> dv4

  renderRef (OTuple6 v0 v1 v2 v3 v4 v5) = do
    dv0 <- renderRef v0
    dv1 <- renderRef v1
    dv2 <- renderRef v2
    dv3 <- renderRef v3
    dv4 <- renderRef v4
    dv5 <- renderRef v5
    pure $ parens $ "tuple6" <+> dv0 <+> dv1 <+> dv2 <+> dv3 <+> dv4 <+> dv5


toOCamlDecoderInterface :: OCamlType a => a -> T.Text
toOCamlDecoderInterface x =
  pprinter $ runReader (renderInterface (toOCamlType x)) defaultOptions
  
toOCamlDecoderRefWith :: OCamlType a => Options -> a -> T.Text
toOCamlDecoderRefWith options x = pprinter $ runReader (renderRef (toOCamlType x)) options

toOCamlDecoderRef :: OCamlType a => a -> T.Text
toOCamlDecoderRef = toOCamlDecoderRefWith defaultOptions

toOCamlDecoderSourceWith :: OCamlType a => Options -> a -> T.Text
toOCamlDecoderSourceWith options x = pprinter $ runReader (render (toOCamlType x)) options

toOCamlDecoderSource :: OCamlType a => a -> T.Text
toOCamlDecoderSource = toOCamlDecoderSourceWith defaultOptions

-- Util
    
-- | "<name>" -> decode <name>
renderSumCondition :: T.Text -> Doc -> Reader Options Doc
renderSumCondition name contents =
  pure $ "|" <+> dquotes (stext name) <+> "->" <$$>
    indent 3 contents

-- | Render a sum type constructor in context of a data type with multiple
-- constructors.
renderSum :: OCamlConstructor -> Reader Options Doc
renderSum (OCamlValueConstructor (NamedConstructor name OCamlEmpty)) = renderSumCondition name ("Js_result.Ok" <+> stext name) 
renderSum (OCamlValueConstructor (NamedConstructor name v@(Values _ _))) = do
  val <- rArgs name v
  renderSumCondition name val

renderSum (OCamlValueConstructor (NamedConstructor name value)) = do
  val <- render value
  renderSumCondition name $ parens
    ("match Json.Decode." <> parens ("field \"contents\"" <+> val <+> "json") <+> "with"
      <$$>
        indent 1
          (    "| v -> Js_result.Ok (" <> (stext name) <+> "v)"
          <$$> "| exception Json.Decode.DecodeError message -> Js_result.Error (\"" <> (stext name) <> ": \" ^ message)"
          )           <> line
    )

renderSum (OCamlValueConstructor (RecordConstructor name value)) = do
  val <- render value
  renderSumCondition name val

renderSum (OCamlValueConstructor (MultipleConstructors constrs)) =
  foldl1 (<$+$>) <$> mapM (renderSum . OCamlValueConstructor) constrs

renderSum (OCamlSumOfRecordConstructor typeName (RecordConstructor name _value)) =
  renderOutsideEncoder typeName name

renderSum (OCamlSumOfRecordConstructor typeName (MultipleConstructors constrs)) =
  foldl1 (<$+$>) <$> mapM renderSum (OCamlSumOfRecordConstructor typeName <$> constrs)

renderSum (OCamlEnumeratorConstructor _) = pure "" -- handled elsewhere
renderSum _ = pure ""

renderOutsideEncoder :: Text -> Text -> Reader Options Doc
renderOutsideEncoder typeName name =  
   return $
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
rArgs :: Text -> OCamlValue -> Reader Options Doc
rArgs name vals = do
  v <- mk name 0 $ (Just <$> flattenOCamlValue vals) ++ [Nothing]
  pure $
    parens $ "match Json.Decode.(field \"contents\" Js.Json.decodeArray json) with"
    <$$> (indent 1 "| Some v ->"
    <$$> (indent 3 v)
    <$$> (indent 1 "| None -> Js_result.Error (\"" <> (stext name) <+> "expected an array.\")" )) <> line

mk :: Text -> Int -> [Maybe OCamlValue] -> Reader Options Doc
mk name i [x] =
  case x of
    Nothing -> do
      let ts = foldl (<>) "" $ L.intersperse ", " ((stext . T.append "v" . T.pack . show) <$> [0..i-1])
      pure $ indent 1 $ "Js_result.Ok" <+> parens (stext name <+> parens ts)
    Just _ -> pure ""
mk name i (x:xs) =
  case x of
    Nothing -> mk name i xs
    Just x' -> do
      renderedVal <- render x'
      renderedInternal <- mk name (i+1) xs
      let iDoc = (stext . T.pack . show $ i)
      pure $ indent 1 $ "(match Json.Decode." <> renderedVal <+> ("v." <> parens iDoc) <+> "with"
        <$$>
          indent 1
            ("| v" <> iDoc <+> "->" <$$> (indent 2 renderedInternal)
            <$$> "| exception Json.Decode.DecodeError message -> Js_result.Error (\"" <> (stext name) <> ": \" ^ message)"
            )
        <$$> ")"

mk _name _i [] = pure ""

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
