{-|
Module      : OCaml.BuckleScript.Encode
Description : Make a JSON encoder for an OCamlDatatype that matches Generic aeson ToJSON
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}

module OCaml.BuckleScript.Encode 
  ( toOCamlEncoderSourceWith
  , toOCamlEncoderInterfaceWith
  ) where

-- base
import           Control.Monad.Reader
import qualified Data.List as L
import           Data.Maybe (catMaybes)
import           Data.Monoid

-- aeson
import qualified Data.Aeson.Types as Aeson (Options(..))

-- text
import           Data.Text (Text)
import qualified Data.Text as T

-- wl-pprint
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

-- ocaml-export
import           OCaml.BuckleScript.Types
import           OCaml.Common

-- | Render the encoder function
class HasEncoder a where
  render :: a -> Reader Options Doc

-- | Render the encode type signature
class HasEncoderRef a where
  renderRef :: a -> Reader Options Doc

-- | Render the encoder interface
class HasEncoderInterface a where
  renderTypeInterface :: a -> Reader Options Doc


instance HasEncoderInterface OCamlDatatype where
  -- sum that has at least one record type
  renderTypeInterface datatype@(OCamlDatatype _ typeName constructor@(OCamlSumOfRecordConstructor _ (MultipleConstructors constructors))) = do
    fnName <- renderRef datatype
    let (typeParameterSignatures,typeParameters) = renderTypeParameterVals constructor
        sumRecordDeclarations = linesBetween $ catMaybes (renderSumRecordInterface typeName . OCamlValueConstructor <$> constructors)
        encodeFnName = stext . textLowercaseFirst $ typeName
    pure $ sumRecordDeclarations
      <$$> "val" <+> fnName <+> ":" <+> typeParameterSignatures <+> typeParameters <> encodeFnName <+> "->" <+> "Js_json.t"
    
  -- other data types
  renderTypeInterface datatype@(OCamlDatatype _ typeName constructor) = do
    fnName <- renderRef datatype
    let (typeParameterSignatures,typeParameters) = renderTypeParameterVals constructor
        encodeFnName = stext . textLowercaseFirst $ typeName
    pure $ "val" <+> fnName <+> ":" <+> typeParameterSignatures <+> typeParameters <> encodeFnName <+> "->" <+> "Js_json.t"

  -- no need to render for primitives
  renderTypeInterface _ = pure ""

instance HasEncoder OCamlDatatype where
  -- sum that has at least one record constructor
  render datatype@(OCamlDatatype _ typeName constructor@(OCamlSumOfRecordConstructor _ (MultipleConstructors constructors))) = do
    ocamlInterface <- asks includeOCamlInterface
    fnName <- renderRef datatype
    typeParameterDeclarations <- linesBetween <$> catMaybes <$> sequence (renderSumRecord typeName . OCamlValueConstructor <$> constructors)
    fnBody <- mapM renderSum (OCamlSumOfRecordConstructor typeName <$> constructors)
    
    if ocamlInterface
      then do
       let typeParameters = renderEncodeTypeParameters constructor
       pure $ typeParameterDeclarations
         <$$> ("let" <+> fnName <+> typeParameters <+> "x =")
         <$$> (indent 2 ("match x with" <$$> foldl1 (<$$>) fnBody))
      else do
        let (typeParameterSignatures,typeParameters) = renderTypeParameters constructor
            encodeFnName = stext $ textLowercaseFirst typeName
        pure $ typeParameterDeclarations
          <$$> ("let" <+> fnName <+> typeParameterSignatures <+> "(x :" <+> typeParameters <> encodeFnName <> ") :Js_json.t =")
          <$$> (indent 2 ("match x with" <$$> foldl1 (<$$>) fnBody))

  -- sum
  render datatype@(OCamlDatatype _ typeName constructor@(OCamlValueConstructor (MultipleConstructors constructors))) = do
    ocamlInterface <- asks includeOCamlInterface
    fnName <- renderRef datatype
    dc <- mapM renderSum (OCamlValueConstructor <$> constructors)
    if ocamlInterface
      then do
        let encodeTypeParameters = renderEncodeTypeParameters constructor
        pure $
          ("let" <+> fnName <+> encodeTypeParameters <+> "x =") <$$>
          (indent 2 ("match x with" <$$> foldl1 (<$$>) dc))
      else do
        let (typeParameterSignatures,typeParameters) = renderTypeParameters constructor
            encodeFnName = stext $ textLowercaseFirst typeName
        pure $
          ("let" <+> fnName <+> typeParameterSignatures <+> "(x :" <+> typeParameters <> encodeFnName <> ") :Js_json.t =") <$$>
          (indent 2 ("match x with" <$$> foldl1 (<$$>) dc))

  -- product or record
  render datatype@(OCamlDatatype _ typeName constructor) = do
    ocamlInterface <- asks includeOCamlInterface
    fnName <- renderRef datatype
    renderedConstructor <- render constructor
    if ocamlInterface
      then do
        let typeParameters = renderEncodeTypeParameters constructor
        pure $
          "let" <+> fnName <+> typeParameters <+> "x =" <$$> (indent 2 renderedConstructor)
      else do
        let (typeParameterSignatures,typeParameters) = renderTypeParameters constructor
            encodeFnName = stext $ textLowercaseFirst typeName
        pure $ "let" <+> fnName <+> typeParameterSignatures <+> "(x :" <+> typeParameters <> encodeFnName <> ") :Js_json.t ="
          <$$> indent 2 renderedConstructor

  -- primitive
  render (OCamlPrimitive primitive) = renderRef primitive

-- | produce encode function name for data types and primitives
instance HasEncoderRef OCamlDatatype where
  renderRef (OCamlDatatype _ name _) = pure $ "encode" <> (stext . textUppercaseFirst $ name)
  renderRef (OCamlPrimitive primitive) = renderRef primitive

instance HasEncoder OCamlConstructor where
  -- Single constructor, no values: empty array
  render (OCamlValueConstructor (NamedConstructor _name OCamlEmpty)) =
    pure $ "Aeson.Encode.list []"

  -- Single constructor, multiple values: create array with values
  render (OCamlValueConstructor (NamedConstructor name value)) = do
    let constructorParams = constructorParameters 0 value
    (encoders, _) <- renderVariable constructorParams value
    
    let encoders' =
          if length constructorParams > 1
          then "Aeson.Encode.array" <+> arraybrackets encoders
          else encoders
        constructorParams' =
          if length constructorParams > 1
          then ["("] <> (L.intersperse "," constructorParams) <> [")"]
          else (L.intersperse "," constructorParams)

    pure $ "match x with" <$$>
      "|" <+> stext name <+> foldl1 (<>) constructorParams' <+> "->" <$$> indent 3 encoders'

  -- Record constructor
  render (OCamlValueConstructor (RecordConstructor _ value)) = do
    recordValue <- render value
    pure . nest 2 $ "Aeson.Encode.object_" <$$> "[" <+> recordValue <$$> "]"

  -- Sum
  render (OCamlValueConstructor (MultipleConstructors constrs)) = do
    sums <- mapM renderSum (OCamlValueConstructor <$> constrs)
    pure $ "match x with" <$$> foldl1 (<$$>) sums

  -- Enumerator
  render ec@(OCamlEnumeratorConstructor _constructors) =
    (<$$>) "match x with" <$> renderSum ec

  render _  = return ""


-- | special rendering function for sum with record types
renderSumRecord :: Text -> OCamlConstructor -> Reader Options (Maybe Doc)
renderSumRecord typeName (OCamlValueConstructor (RecordConstructor name value)) = do
  ocamlInterface <- asks includeOCamlInterface
  let sumRecordName = typeName <> name
  sumRecordBody <- render (OCamlValueConstructor $ RecordConstructor sumRecordName value)
  if ocamlInterface
    then
      pure $ Just $ "let encode" <> stext sumRecordName <+> "x ="
        <$$> indent 2 sumRecordBody
    else
      pure $ Just $ "let encode" <> stext sumRecordName <+> "(x : " <> (stext $ textLowercaseFirst sumRecordName) <> ") :Js_json.t ="
        <$$> indent 2 sumRecordBody

renderSumRecord _ _ = return Nothing

-- | special rendering function for an encoders interface
renderSumRecordInterface :: Text -> OCamlConstructor -> Maybe Doc
renderSumRecordInterface typeName (OCamlValueConstructor (RecordConstructor name _value)) =
  let sumRecordName = typeName <> name 
  in Just $ "val encode" <> stext sumRecordName <+> ":" <+> (stext . textLowercaseFirst $ sumRecordName) <+> "->" <+> "Js_json.t"
renderSumRecordInterface _ _ = Nothing

-- |
renderSumOfRecordEncoder :: Text -> Text -> Reader Options Doc
renderSumOfRecordEncoder typeName name = do
  ao <- asks aesonOptions
  let jsonConstructorName = T.pack . Aeson.constructorTagModifier ao . T.unpack $ name
  pure $
         "|" <+> stext name <+> "y0 ->"
    <$$> "   (match (Js.Json.decodeObject (encode" <> (stext . textUppercaseFirst $ typeName) <> stext name <+> "y0)) with"
    <$$> "    | Some dict ->"
    <$$> "       Js.Dict.set dict \"tag\" (Js.Json.string \"" <> stext jsonConstructorName <> "\");"
    <$$> "       Js.Json.object_ dict"
    <$$> "    | None ->"
    <$$> "       Aeson.Encode.object_ []"
    <$$> "   )"

-- | render product values
jsonEncodeObject :: Doc -> Doc -> Maybe Doc -> Doc
jsonEncodeObject constructor tag mContents =
  case mContents of
    Nothing -> constructor <$$> indent 3 ("Aeson.Encode.object_" <$$> indent 2 ("[" <+> tag <$$> "]"))
    Just contents -> constructor <$$> indent 3 ("Aeson.Encode.object_" <$$> indent 2 ("[" <+> tag <$$> contents <$$> "]"))

-- | render body rules for sum types
renderSum :: OCamlConstructor -> Reader Options Doc
renderSum (OCamlValueConstructor (NamedConstructor name OCamlEmpty)) = do
  ao <- asks aesonOptions
  let jsonConstructorName = T.pack . Aeson.constructorTagModifier ao . T.unpack $ name
      constructorMatchCase = "|" <+> stext name <+> "->"
      encodeTag = pair (dquotes "tag") ("Aeson.Encode.string" <+> dquotes (stext jsonConstructorName))
  pure $ jsonEncodeObject constructorMatchCase encodeTag Nothing

renderSum (OCamlValueConstructor (NamedConstructor name value)) = do
  let constructorParams = constructorParameters 0 value
  ao <- asks aesonOptions
  let jsonConstructorName = T.pack . Aeson.constructorTagModifier ao . T.unpack $ name
  (encoders, _) <- renderVariable constructorParams value
  let encoders' =
        if length constructorParams > 1
        then "Aeson.Encode.array" <+> arraybrackets encoders
        else encoders
      constructorParams' =
        if length constructorParams > 1
        then ["("] <> (L.intersperse "," constructorParams) <> [")"]
        else (L.intersperse "," constructorParams)
      constructorMatchCase  = "|" <+> stext name <+> foldl1 (<>) constructorParams' <+> "->"
      encodeTag = pair (dquotes "tag") ("Aeson.Encode.string" <+> dquotes (stext jsonConstructorName))
      encodeContents  = ";" <+> pair (dquotes "contents") encoders'

  pure $ jsonEncodeObject constructorMatchCase encodeTag (Just encodeContents)

renderSum (OCamlValueConstructor (RecordConstructor name value)) = do
  ao <- asks aesonOptions
  let jsonConstructorName = T.pack . Aeson.constructorTagModifier ao . T.unpack $ name
  encoder <- render value
  let constructorMatchCase = "|" <+> stext name <+> "->"
      encodeTag = pair (dquotes "tag") (dquotes $ stext jsonConstructorName)
      encodeContents = comma <+> encoder
  pure $ jsonEncodeObject constructorMatchCase encodeTag (Just encodeContents)

renderSum (OCamlValueConstructor (MultipleConstructors constructors)) = do
  encoders <- mapM renderSum (OCamlValueConstructor <$> constructors)
  pure $ foldl1 (<$$>) encoders

renderSum (OCamlSumOfRecordConstructor typeName (RecordConstructor name _value)) = do
  renderSumOfRecordEncoder typeName name
  
renderSum (OCamlSumOfRecordConstructor typeName (MultipleConstructors constructors)) = do
  encoders <- mapM renderSum (OCamlSumOfRecordConstructor typeName <$> constructors)
  pure $ foldl1 (<$$>) encoders

renderSum (OCamlEnumeratorConstructor constructors) =
  pure $ foldl1 (<$$>) $ (\(EnumeratorConstructor name) -> "|" <+> stext name <+> "->" <$$> "   Aeson.Encode.string" <+> dquotes (stext name)) <$> constructors

renderSum _ = return ""

instance HasEncoder OCamlValue where
  render (OCamlField name value) = do
    valueBody <- render value
    ao <- asks aesonOptions
    let jsonFieldname = T.pack . Aeson.fieldLabelModifier ao . T.unpack $ name
    return . spaceparens $
      dquotes (stext jsonFieldname) <> comma <+>
      (valueBody <+> "x." <> stext name)
  render (OCamlTypeParameterRef name) =
    pure $ "encode" <> (stext . textUppercaseFirst $ name)
  render (OCamlPrimitiveRef primitive) = renderRef primitive
  render (OCamlRef _ name) = pure $ "encode" <> (stext . textUppercaseFirst $ name)
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> ";" <+> dy
  render _ = error "HasEncoderRef OCamlValue: should not happen"

instance HasEncoderRef OCamlPrimitive where
  renderRef ODate   = pure "Aeson.Encode.date"
  renderRef OUnit   = pure "Aeson.Encode.null"
  renderRef OInt    = pure "Aeson.Encode.int"
  renderRef OChar   = pure "Aeson.Encode.string"
  renderRef OBool   = pure "Aeson.Encode.boolean"
  renderRef OFloat  = pure "Aeson.Encode.float"
  renderRef OString = pure "Aeson.Encode.string"
  renderRef (OList (OCamlPrimitive OChar)) = pure "Aeson.Encode.string"

  renderRef (OList datatype) = do
    dd <- renderRef datatype
    return . parens $ "Aeson.Encode.list" <+> dd

  renderRef (OOption datatype) = do
    dd <- renderRef datatype
    return $ "Aeson.Encode.optional" <+> dd

  renderRef (OEither t0 t1) = do
    dt0 <- renderRef t0
    dt1 <- renderRef t1
    return $ "Aeson.Encode.either" <+> dt0 <+> dt1

  renderRef (OTuple2 t0 t1) = do
    dt0 <- renderRef t0
    dt1 <- renderRef t1
    return $ "Aeson.Encode.pair" <+> dt0 <+> dt1

  renderRef (OTuple3 t0 t1 t2) = do
    dt0 <- renderRef t0
    dt1 <- renderRef t1
    dt2 <- renderRef t2
    return $ "Aeson.Encode.tuple3" <+> dt0 <+> dt1 <+> dt2

  renderRef (OTuple4 t0 t1 t2 t3) = do
    dt0 <- renderRef t0
    dt1 <- renderRef t1
    dt2 <- renderRef t2
    dt3 <- renderRef t3
    return $ "Aeson.Encode.tuple4" <+> dt0 <+> dt1 <+> dt2 <+> dt3
    
  renderRef (OTuple5 t0 t1 t2 t3 t4) = do
    dt0 <- renderRef t0
    dt1 <- renderRef t1
    dt2 <- renderRef t2
    dt3 <- renderRef t3
    dt4 <- renderRef t4
    return $ "Aeson.Encode.tuple5" <+> dt0 <+> dt1 <+> dt2 <+> dt3 <+> dt4

  renderRef (OTuple6 t0 t1 t2 t3 t4 t5) = do
    dt0 <- renderRef t0
    dt1 <- renderRef t1
    dt2 <- renderRef t2
    dt3 <- renderRef t3
    dt4 <- renderRef t4
    dt5 <- renderRef t5
    return $ "Aeson.Encode.tuple6" <+> dt0 <+> dt1 <+> dt2 <+> dt3 <+> dt4 <+> dt5

  renderRef (ODict k v) = do
    dk <- renderRef k
    dv <- renderRef v
    return . parens $ "Js.Encode.dict" <+> dk <+> dv

-- | Variable names for the members of constructors
--   Used in pattern matches
constructorParameters :: Int -> OCamlValue -> [Doc]
constructorParameters _ OCamlEmpty = [ empty ]
constructorParameters i (Values l r) =
    left ++ right
  where
    left = constructorParameters i l
    right = constructorParameters (length left + i) r
constructorParameters i _ = [ "y" <> int i ]


-- | render JSON encoders for OCamlValues. It runs recersively on Values.
--   [Doc] helps build encoders for arrays and tuples
--   should only use fst of return type, snd [Doc] is to help with recursion
renderVariable :: [Doc] -> OCamlValue -> Reader Options (Doc, [Doc])
renderVariable (d : ds) v@(OCamlRef {}) = do
  v' <- render v
  return (v' <+> d, ds)
renderVariable ds OCamlEmpty = return (empty, ds)
renderVariable (_ : ds) (OCamlPrimitiveRef OUnit) =
  return ("Aeson.Encode.null", ds)
renderVariable (d : ds) (OCamlPrimitiveRef ref) = do
  r <- renderRef ref
  return (r <+> d, ds)
renderVariable (d : ds) ref@(OCamlTypeParameterRef _) = do
  r <- render ref
  return (r <+> d, ds)
renderVariable ds (Values l r) = do
  (left, dsl) <- renderVariable ds l
  (right, dsr) <- renderVariable dsl r
  return (left <+> ";" <+> right, dsr)
renderVariable ds f@(OCamlField _ _) = do
  f' <- render f
  return (f', ds)
renderVariable [] _ = error "Amount of variables does not match variables."

-- Util

-- | For an OCamlConstructor, get its OCamlValues as a list, if it has
--   type parameters, render the encoder type signatures for each type parameter
--   as `('a0 -> Js_json.t)` and `'a0`. This is for the interface.
--   fst Doc is type parameter encoder signature
--   snd Doc is list of type parameters which will be rendered as part of the main
--   type's values.
--   For `Either a b`: `("('a0 -> Js_json.t) ('a1 -> Js_json.t)","('a0, 'a1)")`
renderTypeParameterVals :: OCamlConstructor -> (Doc,Doc)
renderTypeParameterVals (OCamlValueConstructor vc) = renderTypeParameterValsAux $ getOCamlValues vc
renderTypeParameterVals (OCamlSumOfRecordConstructor _ vc) = renderTypeParameterValsAux $ getOCamlValues vc
renderTypeParameterVals _ = ("","")

-- | Helper function for renderTypeParameterVals
renderTypeParameterValsAux :: [OCamlValue] -> (Doc,Doc)
renderTypeParameterValsAux ocamlValues =
  let typeParameterNames = (<>) "'" <$> getTypeParameterRefNames ocamlValues
      typeDecs = (\t -> "(" <> (stext t) <+> "-> Js_json.t)") <$> typeParameterNames
  in
      if length typeDecs > 0
      then
        let ts =
              if length typeParameterNames > 1
              then foldl (<>) "" $ ["("] <> (L.intersperse ", " $ stext <$> typeParameterNames) <> [") "]
              else
                -- `flip (<>) " "` means add a space to the end
                if length typeParameterNames == 1 then stext . flip (<>) " " . head $ typeParameterNames
                else ""
        in ((foldl (<>) "" $  (L.intersperse " -> " typeDecs)) <> " ->", ts)
      else ("","")

-- | Render type parameters as encode functions. This is for a let declaration that has
--   a complete type signature.
--   `Either a b` : `("(encodeA0 : 'a0 -> Js_json.t) (encodeA0 : 'a1 -> Js_json.t)", "('a0, 'a1)")`
renderTypeParameters :: OCamlConstructor -> (Doc,Doc)
renderTypeParameters (OCamlValueConstructor vc) = renderTypeParametersAux $ getOCamlValues vc
renderTypeParameters (OCamlSumOfRecordConstructor _ vc) = renderTypeParametersAux $ getOCamlValues vc
renderTypeParameters _ = ("","")

-- | Helper function for renderTypeParameters
renderTypeParametersAux :: [OCamlValue] -> (Doc,Doc)
renderTypeParametersAux ocamlValues = do
  let typeParameterNames = getTypeParameterRefNames ocamlValues
      typeDecs = (\t -> "(type " <> (stext t) <> ")") <$> typeParameterNames :: [Doc]
      parserDecs  = (\t -> "(encode" <> (stext $ textUppercaseFirst t) <+> ":" <+> (stext t) <+> "-> Js_json.t)" ) <$> typeParameterNames :: [Doc]
      typeParams = foldl (<>) "" $ if length typeParameterNames > 1 then  ["("] <> (L.intersperse ", " $ stext <$> typeParameterNames) <> [") "] else ((\x -> stext $ x <> " ") <$> typeParameterNames) :: [Doc]
  (foldl (<+>) "" (typeDecs ++ parserDecs), typeParams )

-- | render type parameter encoder names for all of a constructors type parameters
--   `Either a b`: `"encodeA0 encodeA1"`
renderEncodeTypeParameters :: OCamlConstructor -> Doc
renderEncodeTypeParameters constructor =
  foldl (<>) "" $ stext <$> L.intersperse " " ((\t -> "encode" <> (textUppercaseFirst t)) <$> getTypeParameters constructor)


-- | export

toOCamlEncoderInterfaceWith :: OCamlType a => Options -> a -> T.Text
toOCamlEncoderInterfaceWith options x =
  pprinter $ runReader (renderTypeInterface (toOCamlType x)) options

toOCamlEncoderSourceWith :: OCamlType a => Options -> a -> T.Text
toOCamlEncoderSourceWith options x =
  pprinter $ runReader (render (toOCamlType x)) options
