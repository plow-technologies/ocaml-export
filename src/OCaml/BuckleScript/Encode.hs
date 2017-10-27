{-# LANGUAGE OverloadedStrings #-}

module OCaml.BuckleScript.Encode 
  ( toOCamlEncoderRef
  , toOCamlEncoderRefWith
  , toOCamlEncoderSource
  , toOCamlEncoderSourceWith
  ) where

import           Control.Monad.Reader
import qualified Data.List as L
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           OCaml.Common
import           OCaml.Type
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasEncoder a where
  render :: a -> Reader Options Doc

class HasEncoderRef a where
  renderRef :: a -> Reader Options Doc

renderTypeParametersAux :: [OCamlValue] -> (Doc,Doc)
renderTypeParametersAux ocamlValues = do
  let typeParameterNames = getTypeParameterRefNames ocamlValues
      typeDecs = (\t -> "(type " <> (stext t) <> ")") <$> typeParameterNames :: [Doc]
      parserDecs  = (\t -> "(encode" <> (stext $ textUppercaseFirst t) <+> ":" <+> (stext t) <+> "-> Js_json.t)" ) <$> typeParameterNames :: [Doc]
      typeParams = foldl (<>) "" $ if length typeParameterNames > 1 then  ["("] <> (L.intersperse ", " $ stext <$> typeParameterNames) <> [") "] else ((\x -> stext $ x <> " ") <$> typeParameterNames) :: [Doc]
  (foldl (<+>) "" (typeDecs ++ parserDecs), typeParams )

getOCamlValues :: ValueConstructor -> [OCamlValue]
getOCamlValues (NamedConstructor     _ value) = [value]
getOCamlValues (RecordConstructor    _ value) = [value]
getOCamlValues (MultipleConstructors cs)      = concat $ getOCamlValues <$> cs

renderTypeParameters :: OCamlConstructor -> (Doc,Doc)
renderTypeParameters (OCamlValueConstructor vc) = renderTypeParametersAux $ getOCamlValues vc
renderTypeParameters (OCamlSumOfRecordConstructor _ vc) = renderTypeParametersAux $ getOCamlValues vc
renderTypeParameters _ = ("","")

instance HasEncoder OCamlDatatype where
  -- handle case where SumWithRecords exists
  render d@(OCamlDatatype typeName c@(OCamlSumOfRecordConstructor _ (MultipleConstructors constrs))) = do  
    fnName <- renderRef d
    docs <- catMaybes <$> sequence (renderSumRecord typeName . OCamlValueConstructor <$> constrs)
    let vs = msuffix (line <> line) docs
    dc <- mapM renderSum (OCamlSumOfRecordConstructor typeName <$> constrs)
    let (tps,aps) = renderTypeParameters c
    return $ vs <$$>
      ("let" <+> fnName <+> tps <+> "(x :" <+> aps <> stext (textLowercaseFirst typeName) <> ") :Js_json.t =") <$$>
      (indent 2 ("match x with" <$$> foldl1 (<$$>) dc))
    
  render d@(OCamlDatatype typeName c@(OCamlValueConstructor (MultipleConstructors constrs))) = do
    fnName <- renderRef d
    dc <- mapM renderSum (OCamlValueConstructor <$> constrs)
    let (tps,aps) = renderTypeParameters c
    return $
      ("let" <+> fnName <+> tps <+> "(x :" <+> aps <> stext (textLowercaseFirst typeName) <> ") :Js_json.t =") <$$>
      (indent 2 ("match x with" <$$> foldl1 (<$$>) dc))

  render d@(OCamlDatatype name constructor) = do
    fnName <- renderRef d
    ctor <- render constructor
    let (tps,aps) = renderTypeParameters constructor
    return $
      ("let" <+> fnName <+> tps <+> "(x :" <+> aps <> stext (textLowercaseFirst name) <> ") :Js_json.t =") <$$>
      (indent 2 ctor)

  render (OCamlPrimitive primitive) = renderRef primitive
    
instance HasEncoderRef OCamlDatatype where
  renderRef (OCamlDatatype name _) = pure $ "encode" <> stext name
  renderRef (OCamlPrimitive primitive) = renderRef primitive

instance HasEncoder OCamlConstructor where
  -- Single constructor, no values: empty array
  render (OCamlValueConstructor (NamedConstructor _name OCamlEmpty)) =
    return $ "Json.Encode.list []"

  -- Single constructor, multiple values: create array with values
  render (OCamlValueConstructor (NamedConstructor name value)) = do
    let ps = constructorParameters 0 value

    (dc, _) <- renderVariable ps value
    let dc' = if length ps > 1 then "Json.Encode.array" <+> arraybrackets dc else dc
        ps' = if length ps > 1 then ["("] <> (L.intersperse "," ps) <> [")"] else (L.intersperse "," ps)

    return $ "match x with" <$$>
      "|" <+> stext name <+> foldl1 (<>) ps' <+> "->" <$$> "   " <> dc'
  
  render (OCamlValueConstructor (RecordConstructor _ value)) = do
    dv <- render value
    return . nest 2 $ "Json.Encode.object_" <$$> "[" <+> dv <$$> "]"

  render (OCamlValueConstructor (MultipleConstructors constrs)) = do
    dc <- mapM renderSum (OCamlValueConstructor <$> constrs)
    return $ "match x with" <$$> foldl1 (<$$>) dc

  render ec@(OCamlEnumeratorConstructor _constructors) = do
    dv <- renderSum ec
    return $ "match x with" <$$> dv

  render _  = return ""

renderSumRecord :: Text -> OCamlConstructor -> Reader Options (Maybe Doc)
renderSumRecord typeName (OCamlValueConstructor (RecordConstructor name value)) = do
  s <- render (OCamlValueConstructor $ RecordConstructor (typeName <> name) value)
  return $ Just $ "let encode" <> (stext newName) <> " (x : " <> (stext $ textLowercaseFirst newName) <> ") :Js_json.t =" <$$> (indent 2 s)
  where
    newName = typeName <> name
renderSumRecord _ _ = return Nothing

jsonEncodeObject :: Doc -> Doc -> Maybe Doc -> Doc
jsonEncodeObject constructor tag mContents =
  case mContents of
    Nothing -> constructor <$$> nest 5 ("   Json.Encode.object_" <$$> ("[" <+> tag <$$> "]"))
    Just contents -> constructor <$$> nest 5 ("   Json.Encode.object_" <$$> ("[" <+> tag <$$> contents <$$> "]"))

renderOutsideEncoder :: Text -> Text -> Reader Options Doc
renderOutsideEncoder typeName name =  
   return $
         "|" <+> (stext name) <+> "y0 ->"
    <$$> "   (match (Js.Json.decodeObject (encode" <> (stext typeName) <> (stext name) <+> "y0)) with"
    <$$> "    | Some dict ->"
    <$$> "       Js.Dict.set dict \"tag\" (Js.Json.string \"" <> (stext name) <> "\");"
    <$$> "       Js.Json.object_ dict"
    <$$> "    | None ->"
    <$$> "       Json.Encode.object_ []"
    <$$> "   )"
    
  
renderSum :: OCamlConstructor -> Reader Options Doc
renderSum (OCamlValueConstructor (NamedConstructor name OCamlEmpty)) = do
  let cs = "|" <+> stext name <+> "->"
  let tag = pair (dquotes "tag") ("Json.Encode.string" <+> dquotes (stext name))
  return $ jsonEncodeObject cs tag Nothing

renderSum (OCamlValueConstructor (NamedConstructor name value)) = do
  let ps = constructorParameters 0 value

  (dc, _) <- renderVariable ps value
  let dc' = if length ps > 1 then "Json.Encode.array" <+> arraybrackets dc else dc
      ps' = if length ps > 1 then ["("] <> (L.intersperse "," ps) <> [")"] else (L.intersperse "," ps)
      cs  = "|" <+> stext name <+> foldl1 (<>) ps' <+> "->"
      tag = pair (dquotes "tag") ("Json.Encode.string" <+> dquotes (stext name))
      ct  = ";" <+> pair (dquotes "contents") dc'

  return $ jsonEncodeObject cs tag (Just ct)

renderSum (OCamlValueConstructor (RecordConstructor name value)) = do
  dv <- render value
  let cs = "|" <+> stext name <+> "->"
  let tag = pair (dquotes "tag") (dquotes $ stext name)
  let ct = comma <+> dv
  return $ jsonEncodeObject cs tag (Just ct)

renderSum (OCamlValueConstructor (MultipleConstructors constrs)) = do
  dc <- mapM renderSum (OCamlValueConstructor <$> constrs)
  return $ foldl1 (<$$>) dc

renderSum (OCamlSumOfRecordConstructor typeName (RecordConstructor name _value)) = do
  renderOutsideEncoder typeName name
  
renderSum (OCamlSumOfRecordConstructor typeName (MultipleConstructors constrs)) = do
  dc <- mapM renderSum (OCamlSumOfRecordConstructor typeName <$> constrs)
  return $ foldl1 (<$$>) dc  

renderSum (OCamlEnumeratorConstructor constructors) =
  return $ foldl1 (<$$>) $ (\(EnumeratorConstructor name) -> "|" <+> stext name <+> "->" <$$> "   Json.Encode.string" <+> dquotes (stext name)) <$> constructors

renderSum _ = return ""
{-
renderEnumeration :: OCamlConstructor -> Reader Options Doc
renderEnumeration (OCamlConstructor (NamedConstructor name _)) =
  return $ "|" <+> stext name <+> "->" <$$>
      "   Json.Encode.string" <+> dquotes (stext name)
renderEnumeration (OCamlConstructor (MultipleConstructors constrs)) = do
  dc <- mapM renderEnumeration constrs
  return $ foldl1 (<$$>) dc
renderEnumeration c = render c
-}

instance HasEncoder OCamlValue where
  render (OCamlField name value) = do
    fieldModifier <- asks fieldLabelModifier
    valueBody <- render value
    return . spaceparens $
      dquotes (stext (fieldModifier name)) <> comma <+>
      (valueBody <+> "x." <> stext name)
  render (OCamlTypeParameterRef name) =
    return $ ("encode" <> stext (textUppercaseFirst name))
  render (OCamlPrimitiveRef primitive) = renderRef primitive
  render (OCamlRef name) = pure $ "encode" <> stext name
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> ";" <+> dy
  render _ = error "HasEncoderRef OCamlValue: should not happen"

instance HasEncoderRef OCamlPrimitive where
  renderRef ODate   = pure $ parens "Json.Encode.string << toString"
  renderRef OUnit   = pure "Json.Encode.null"
  renderRef OInt    = pure "Json.Encode.int"
  renderRef OChar   = pure "Json.Encode.string"
  renderRef OBool   = pure "Json.Encode.boolean"
  renderRef OFloat  = pure "Json.Encode.float"
  renderRef OString = pure "Json.Encode.string"
  renderRef (OList (OCamlPrimitive OChar)) = pure "Json.Encode.string"
  renderRef (OList datatype) = do
    dd <- renderRef datatype
    return . parens $ "Json.Encode.list" <+> dd
  renderRef (OOption datatype) = do
    dd <- renderRef datatype
    return . parens $ "fun a -> Option.default Json.Encode.null (Option.map" <+> dd <+> "a)"
  renderRef (OTuple2 a b) = do
    da <- renderRef a
    db <- renderRef b
    return . parens $ "fun (a,b) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b" <+> " |]"
  renderRef (OTuple3 a b c) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    return . parens $ "fun (a,b,c) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b ;" <+> dc <+> "c" <+> "|]"
  renderRef (OTuple4 a b c d) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    return . parens $ "fun (a,b,c,d) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b ;" <+> dc <+> "c ;" <+> dd <+> "d" <+> "|]"
  renderRef (OTuple5 a b c d e) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    de <- renderRef e
    return . parens $ "fun (a,b,c,d,e) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b ;" <+> dc <+> "c ;" <+> dd <+> "d ;" <+> de <+> "e" <+> "|]"
  renderRef (OTuple6 a b c d e f) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    de <- renderRef e
    df <- renderRef f
    return . parens $ "fun (a,b,c,d,e,f) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b ;" <+> dc <+> "c ;" <+> dd <+> "d ;" <+> de <+> "e ;" <+> df <+> "f" <+> "|]"
  renderRef (ODict k v) = do
    dk <- renderRef k
    dv <- renderRef v
    return . parens $ "Js.Encode.dict" <+> dk <+> dv

toOCamlEncoderRefWith :: OCamlType a => Options -> a -> T.Text
toOCamlEncoderRefWith options x =
  pprinter $ runReader (renderRef (toOCamlType x)) options

toOCamlEncoderRef :: OCamlType a => a -> T.Text
toOCamlEncoderRef = toOCamlEncoderRefWith defaultOptions

toOCamlEncoderSourceWith :: OCamlType a => Options -> a -> T.Text
toOCamlEncoderSourceWith options x =
  pprinter $ runReader (render (toOCamlType x)) options

toOCamlEncoderSource :: OCamlType a => a -> T.Text
toOCamlEncoderSource = toOCamlEncoderSourceWith defaultOptions

-- | Variable names for the members of constructors
-- Used in pattern matches
constructorParameters :: Int -> OCamlValue -> [Doc]
constructorParameters _ OCamlEmpty = [ empty ]
constructorParameters i (Values l r) =
    left ++ right
  where
    left = constructorParameters i l
    right = constructorParameters (length left + i) r
constructorParameters i _ = [ "y" <> int i ]


-- | Encode variables following the recipe of an OCamlValue
renderVariable :: [Doc] -> OCamlValue -> Reader Options (Doc, [Doc])
renderVariable (d : ds) v@(OCamlRef {}) = do
  v' <- render v
  return (v' <+> d, ds)
renderVariable ds OCamlEmpty = return (empty, ds)
renderVariable (_ : ds) (OCamlPrimitiveRef OUnit) =
  return ("Json.Encode.null", ds)
renderVariable (d : ds) (OCamlPrimitiveRef ref) = do
  r <- renderRef ref
  return (r <+> d, ds)
renderVariable ds (Values l r) = do
  (left, dsl) <- renderVariable ds l
  (right, dsr) <- renderVariable dsl r
  return (left <+> ";" <+> right, dsr)
renderVariable ds f@(OCamlField _ _) = do
  f' <- render f
  return (f', ds)
renderVariable [] _ = error "Amount of variables does not match variables."
renderVariable _  _ = error "This variable is not intended to be rendered."  
