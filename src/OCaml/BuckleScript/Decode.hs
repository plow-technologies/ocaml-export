{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module OCaml.BuckleScript.Decode
  ( toOCamlDecoderRef
  , toOCamlDecoderRefWith
  , toOCamlDecoderSource
  , toOCamlDecoderSourceWith
  ) where

import           Control.Monad.Reader
import qualified Data.List as L
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           OCaml.Common
import           OCaml.Type
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasDecoder a where
  render :: a -> Reader Options Doc

class HasDecoderRef a where
  renderRef :: a -> Reader Options Doc

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

instance HasDecoder OCamlDatatype where
  render d@(OCamlDatatype name cs@(OCamlEnumeratorConstructor constructors)) = do
    dv <- mapM render constructors
    fnName <- renderRef d
    let (tps,aps) = renderTypeParameters cs
        returnType =  "(" <> aps <> (stext . textLowercaseFirst $ name) <> ", string) Js_result.t ="
    pure $
      "let" <+> fnName <+> tps <+>"(json : Js_json.t) :" <> returnType <$$>
      indent 2 ("match Js_json.decodeString json with" <$$>
      foldl1 (<$$>) dv <$$> footer fnName)
    where
      footer fnName
        =    "| Some err -> Js_result.Error (\"" <> fnName <> ": unknown enumeration '\" ^ err ^ \"'.\")"
        <$$> "| None -> Js_result.Error \"" <> fnName <> ": expected a top-level JSON string.\""
      
  render d@(OCamlDatatype name constructor) = do
    fnName <- renderRef d
    ctor <- render constructor
    let (tps,aps) = renderTypeParameters constructor
        returnType = "(" <> aps <> (stext . textLowercaseFirst $ name) <> ", string) Js_result.t ="
    pure
       $   "let" <+> fnName <+> tps <+> "(json : Js_json.t) :" <> returnType
      <$$> ctor

  render (OCamlPrimitive primitive) = renderRef primitive

instance HasDecoderRef OCamlDatatype where
  renderRef (OCamlDatatype name _) = pure $ "decode" <> stext name
  renderRef (OCamlPrimitive primitive) = renderRef primitive

instance HasDecoder OCamlConstructor where
  render (OCamlValueConstructor (NamedConstructor name value)) = do
    dv <- render value
    return
       $   indent 2
           ("match Json.Decode." <> dv <+> "json" <+> "with"
      <$$> "| v -> Js_result.Ok" <+> parens (stext name <+> "v")
      <$$> "| exception Json.Decode.DecodeError msg -> Js_result.Error (\"decode" <> (stext name) <> ": \" ^ msg)"
           )
           
  render (OCamlValueConstructor (RecordConstructor name value)) = do
    dv <- render value
    pure
         $ "  match Json.Decode."
      <$$> (indent 4 ("{" <+> dv <$$> "}"))
      <$$> "  with"
      <$$> "  | v -> Js_result.Ok v"
      <$$> "  | exception Json.Decode.DecodeError message -> Js_result.Error (\"decode" <> stext name <> ": \" ^ message)"

  render (OCamlValueConstructor (MultipleConstructors constructors)) = do
    renderedConstructors <- mapM (renderSum . OCamlValueConstructor) constructors
    pure $ indent 2 "match Json.Decode.(field \"tag\" string json) with"
      <$$> indent 2 (foldl (<$$>) "" renderedConstructors)
      <$$> indent 2 footer
    where
      footer = "| err -> Js_result.Error (\"Unknown tag value found '\" ^ err ^ \"'.\")"
          <$$> "| exception Json.Decode.DecodeError message -> Js_result.Error message"

  render (OCamlEnumeratorConstructor _constructors) = fail "OCamlEnumeratorConstructor should be handled at the OCamlDataType level."
  render (OCamlSumOfRecordConstructor _name _constructors) = fail "OCamlEnumeratorConstructor should be handled at the OCamlDataType level."
    
-- | required "contents"
requiredContents :: Doc
requiredContents = "required" <+> dquotes "contents"

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

renderSum (OCamlEnumeratorConstructor _) = pure "" -- handled elsewhere

renderSum (OCamlSumOfRecordConstructor _ _) = pure "" -- handled elsewhere

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
  

renderConstructorArgs :: Int -> OCamlValue -> Reader Options (Int, Doc)
renderConstructorArgs i (Values l r) = do
  (iL, rndrL) <- renderConstructorArgs i l
  (iR, rndrR) <- renderConstructorArgs (iL + 1) r
  pure (iR, rndrL <$$> rndrR)
renderConstructorArgs i val = do
  rndrVal <- render val
  -- let index = parens $ "index" <+> int i <+> rndrVal
  pure (i,
             "(match Json.Decode.(field \"contents\"" <+> rndrVal <+> "json) with"
        <$$>
          indent 1
            ("| Some v" <> (stext . T.pack . show $ i) <+> "->"
            <$$> "| None -> Js_result.Error (\"\")"
            )
        <$$> ")"
       )

instance HasDecoder OCamlValue where
  render (OCamlRef name) = do
    pure $ "(fun a -> unwrapResult (decode" <> stext name <+> "a))"

  render (OCamlPrimitiveRef primitive) = renderRef primitive

  render (OCamlTypeParameterRef name) =
    pure $ ("(fun a -> unwrapResult (decode" <> stext (textUppercaseFirst name)) <+> "a))"

  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> ";" <+> dy

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
  renderRef (OList (OCamlPrimitive OChar)) = pure "string"

  renderRef (OList (OCamlDatatype name _)) = do

    return . parens $ "list" <+> (parens $ "fun a -> unwrapResult (decode" <> stext name <+> "a)")

  renderRef (OList datatype) = do
    dt <- renderRef datatype
    return . parens $ "list" <+> dt

  renderRef (ODict key value) = do
    d <- renderRef (OList (OCamlPrimitive (OTuple2 (OCamlPrimitive key) value)))
    return . parens $ "map Dict.fromList" <+> d

  renderRef (OOption datatype) = do
    dt <- renderRef datatype
    return . parens $ "maybe" <+> dt

  renderRef (OTuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
--    return $ "Json.Decode." <> (parens $ "pair" <+> dx <+> dy)
    pure $ parens $ "pair" <+> dx <+> dy
    
  renderRef OUnit = pure $ parens "succeed ()"
  renderRef ODate = pure "decodeDate"
  renderRef OInt = pure "int"
  renderRef OBool = pure "bool"
  renderRef OChar = pure "char"
  renderRef OFloat = pure "float"
  renderRef OString = pure "string"

toOCamlDecoderRefWith :: OCamlType a => Options -> a -> T.Text
toOCamlDecoderRefWith options x = pprinter $ runReader (renderRef (toOCamlType x)) options

toOCamlDecoderRef :: OCamlType a => a -> T.Text
toOCamlDecoderRef = toOCamlDecoderRefWith defaultOptions

toOCamlDecoderSourceWith :: OCamlType a => Options -> a -> T.Text
toOCamlDecoderSourceWith options x = pprinter $ runReader (render (toOCamlType x)) options

toOCamlDecoderSource :: OCamlType a => a -> T.Text
toOCamlDecoderSource = toOCamlDecoderSourceWith defaultOptions

