{-# LANGUAGE OverloadedStrings #-}

module OCaml.BuckleScript.Encode 
  ( toReasonEncoderRef
  , toReasonEncoderRefWith
  , toReasonEncoderSource
  , toReasonEncoderSourceWith
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

instance HasEncoder ReasonDatatype where
  -- handle case where SumWithRecords exists
  render d@(ReasonDatatype typeName mc@(MultipleConstructors constrs)) = do
    fnName <- renderRef d
      
    if isSumWithRecords mc
      then do
        docs <- catMaybes <$> sequence (renderSumRecord typeName <$> constrs)
        let vs = msuffix (line <> line) docs
        dc <- mapM renderSum constrs
        return $ vs <$$>
          ("let" <+> fnName <+> "(x :" <+> stext (textLowercaseFirst typeName) <> ") =") <$$>
          (indent 2 ("match x with" <$$> foldl1 (<$$>) dc))
      else do
        let rndr = if isEnumeration mc then renderEnumeration else renderSum
        dc <- mapM rndr constrs
        return $
          ("let" <+> fnName <+> "(x :" <+> stext (textLowercaseFirst typeName) <> ") =") <$$>
          (indent 2 ("match x with" <$$> foldl1 (<$$>) dc))

  render d@(ReasonDatatype name constructor) = do
    fnName <- renderRef d
    ctor <- render constructor
    return $
      ("let" <+> fnName <+> "(x :" <+> stext (textLowercaseFirst name) <> ") =") <$$>
      (indent 2 ctor)

  render (ReasonPrimitive primitive) = renderRef primitive
    
instance HasEncoderRef ReasonDatatype where
  renderRef (ReasonDatatype name _) = pure $ "encode" <> stext name
  renderRef (ReasonPrimitive primitive) = renderRef primitive

instance HasEncoder ReasonConstructor where
  -- Single constructor, no values: empty array
  render (NamedConstructor _name ReasonEmpty) =
    return $ "Json.Encode.list []"

  -- Single constructor, multiple values: create array with values
  render (NamedConstructor name value) = do
    let ps = constructorParameters 0 value

    (dc, _) <- renderVariable ps value
    let dc' = if length ps > 1 then "Json.Encode.array" <+> arraybrackets dc else dc
        ps' = if length ps > 1 then ["("] <> (L.intersperse "," ps) <> [")"] else (L.intersperse "," ps)

    return $ "match x with" <$$>
      "|" <+> stext name <+> foldl1 (<>) ps' <+> "->" <$$> "   " <> dc'
  
  render (RecordConstructor _ value) = do
    dv <- render value
    return . nest 2 $ "Json.Encode.object_" <$$> "[" <+> dv <$$> "]"

  render mc@(MultipleConstructors constrs) = do
    let extra = if isSumWithRecords mc then "extra extra" else ""
    let rndr = if isEnumeration mc then renderEnumeration else renderSum
    dc <- mapM rndr constrs
    return $ extra <> "match x with" <$$> foldl1 (<$$>) dc

renderSumRecord :: Text -> ReasonConstructor -> Reader Options (Maybe Doc)
renderSumRecord typeName c@(RecordConstructor name value) = do
  s <- render (RecordConstructor (typeName <> name) value)
  return $ Just $ "let encode" <> (stext newName) <> " (x : " <> (stext $ textLowercaseFirst newName) <> ") =" <$$> (indent 2 s)
  where
    newName = typeName <> name
renderSumRecord _ _ = return Nothing

jsonEncodeObject :: Doc -> Doc -> Maybe Doc -> Doc
jsonEncodeObject constructor tag mContents =
  case mContents of
    Nothing -> constructor <$$> nest 5 ("   Json.Encode.object_" <$$> ("[" <+> tag <$$> "]"))
    Just contents -> constructor <$$> nest 5 ("   Json.Encode.object_" <$$> ("[" <+> tag <$$> contents <$$> "]"))
  
renderSum :: ReasonConstructor -> Reader Options Doc
renderSum (NamedConstructor name ReasonEmpty) = do
  let cs = "|" <+> stext name <+> "->"
  let tag = pair (dquotes "tag") ("Json.Encode.string" <+> dquotes (stext name))
  return $ jsonEncodeObject cs tag Nothing

renderSum (NamedConstructor name value) = do
  let ps = constructorParameters 0 value

  (dc, _) <- renderVariable ps value
  let dc' = if length ps > 1 then "Json.Encode.array" <+> arraybrackets dc else dc
      ps' = if length ps > 1 then ["("] <> (L.intersperse "," ps) <> [")"] else (L.intersperse "," ps)
      cs  = "|" <+> stext name <+> foldl1 (<>) ps' <+> "->"
      tag = pair (dquotes "tag") ("Json.Encode.string" <+> dquotes (stext name))
      ct  = ";" <+> pair (dquotes "contents") dc'

  return $ jsonEncodeObject cs tag (Just ct)

renderSum (RecordConstructor name value) = do
  dv <- render value
  let cs = "|" <+> stext name <+> "->"
  let tag = pair (dquotes "tag") (dquotes $ stext name)
  let ct = comma <+> dv
  return $ jsonEncodeObject cs tag (Just ct)

renderSum (MultipleConstructors constrs) = do
  dc <- mapM renderSum constrs
  return $ foldl1 (<$$>) dc


renderEnumeration :: ReasonConstructor -> Reader Options Doc
renderEnumeration (NamedConstructor name _) =
  return $ "|" <+> stext name <+> "->" <$$>
      "   Json.Encode.string" <+> dquotes (stext name)
renderEnumeration (MultipleConstructors constrs) = do
  dc <- mapM renderEnumeration constrs
  return $ foldl1 (<$$>) dc
renderEnumeration c = render c


instance HasEncoder ReasonValue where
  render (ReasonField name value) = do
    fieldModifier <- asks fieldLabelModifier
    valueBody <- render value
    return . spaceparens $
      dquotes (stext (fieldModifier name)) <> comma <+>
      (valueBody <+> "x." <> stext name)
  render (ReasonPrimitiveRef primitive) = renderRef primitive
  render (ReasonRef name) = pure $ "encode" <> stext name
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> ";" <+> dy
  render _ = error "HasEncoderRef ReasonValue: should not happen"

instance HasEncoderRef ReasonPrimitive where
  renderRef RDate   = pure $ parens "Json.Encode.string << toString"
  renderRef RUnit   = pure "Json.Encode.null"
  renderRef RInt    = pure "Json.Encode.int"
  renderRef RChar   = pure "Json.Encode.string"
  renderRef RBool   = pure "Json.Encode.boolean"
  renderRef RFloat  = pure "Json.Encode.float"
  renderRef RString = pure "Json.Encode.string"
  renderRef (RList (ReasonPrimitive RChar)) = pure "Json.Encode.string"
  renderRef (RList datatype) = do
    dd <- renderRef datatype
    return . parens $ "Json.Encode.list" <+> dd
  renderRef (RMaybe datatype) = do
    dd <- renderRef datatype
    return . parens $ "fun a -> Option.default Json.Encode.null (Option.map" <+> dd <+> "a)"
  renderRef (RTuple2 a b) = do
    da <- renderRef a
    db <- renderRef b
    return . parens $ "fun (a,b) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b" <+> " |]"
  renderRef (RTuple3 a b c) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    return . parens $ "fun (a,b,c) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b ;" <+> dc <+> "c" <+> "|]"
  renderRef (RTuple4 a b c d) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    return . parens $ "fun (a,b,c,d) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b ;" <+> dc <+> "c ;" <+> dd <+> "d" <+> "|]"
  renderRef (RTuple5 a b c d e) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    de <- renderRef e
    return . parens $ "fun (a,b,c,d,e) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b ;" <+> dc <+> "c ;" <+> dd <+> "d ;" <+> de <+> "e" <+> "|]"
  renderRef (RTuple6 a b c d e f) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    de <- renderRef e
    df <- renderRef f
    return . parens $ "fun (a,b,c,d,e,f) -> Json.Encode.array [|" <+> da <+> "a ;" <+> db <+> "b ;" <+> dc <+> "c ;" <+> dd <+> "d ;" <+> de <+> "e ;" <+> df <+> "f" <+> "|]"
  renderRef (RDict k v) = do
    dk <- renderRef k
    dv <- renderRef v
    return . parens $ "Js.Encode.dict" <+> dk <+> dv

toReasonEncoderRefWith :: ReasonType a => Options -> a -> T.Text
toReasonEncoderRefWith options x =
  pprinter $ runReader (renderRef (toReasonType x)) options

toReasonEncoderRef :: ReasonType a => a -> T.Text
toReasonEncoderRef = toReasonEncoderRefWith defaultOptions

toReasonEncoderSourceWith :: ReasonType a => Options -> a -> T.Text
toReasonEncoderSourceWith options x =
  pprinter $ runReader (render (toReasonType x)) options

toReasonEncoderSource :: ReasonType a => a -> T.Text
toReasonEncoderSource = toReasonEncoderSourceWith defaultOptions

-- | Variable names for the members of constructors
-- Used in pattern matches
constructorParameters :: Int -> ReasonValue -> [Doc]
constructorParameters _ ReasonEmpty = [ empty ]
constructorParameters i (Values l r) =
    left ++ right
  where
    left = constructorParameters i l
    right = constructorParameters (length left + i) r
constructorParameters i _ = [ "y" <> int i ]


-- | Encode variables following the recipe of an ReasonValue
renderVariable :: [Doc] -> ReasonValue -> Reader Options (Doc, [Doc])
renderVariable (d : ds) v@(ReasonRef {}) = do
  v' <- render v
  return (v' <+> d, ds)
renderVariable ds ReasonEmpty = return (empty, ds)
renderVariable (_ : ds) (ReasonPrimitiveRef RUnit) =
  return ("Json.Encode.null", ds)
renderVariable (d : ds) (ReasonPrimitiveRef ref) = do
  r <- renderRef ref
  return (r <+> d, ds)
renderVariable ds (Values l r) = do
  (left, dsl) <- renderVariable ds l
  (right, dsr) <- renderVariable dsl r
  return (left <+> ";" <+> right, dsr)
renderVariable ds f@(ReasonField _ _) = do
  f' <- render f
  return (f', ds)
renderVariable [] _ = error "Amount of variables does not match variables"
