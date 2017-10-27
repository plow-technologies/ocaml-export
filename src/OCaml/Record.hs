{-# LANGUAGE OverloadedStrings #-}

module OCaml.Record
  ( toOCamlTypeRef
  , toOCamlTypeRefWith
  , toOCamlTypeSource
  , toOCamlTypeSourceWith
  ) where

import           Control.Monad.Reader
import           Data.List (nub)
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           OCaml.Common
import           OCaml.Type
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasType a where
  render :: a -> Reader Options Doc

class HasRecordType a where
  renderRecord :: a -> Reader Options Doc

class HasTypeRef a where
  renderRef :: a -> Reader Options Doc

getOCamlTypeParameterRef :: OCamlValue -> [Text]
getOCamlTypeParameterRef (OCamlTypeParameterRef name) = ["'" <> name]
getOCamlTypeParameterRef (OCamlField _ v1) = getOCamlTypeParameterRef v1
getOCamlTypeParameterRef (Values v1 v2) = getOCamlTypeParameterRef v1 ++ getOCamlTypeParameterRef v2
getOCamlTypeParameterRef (OCamlPrimitiveRef (OList v1)) = renderDatatype v1
getOCamlTypeParameterRef (OCamlPrimitiveRef (OOption v1)) = renderDatatype v1
getOCamlTypeParameterRef (OCamlPrimitiveRef (OTuple2 v1 v2)) = (renderDatatype v1) ++ (renderDatatype v2)
getOCamlTypeParameterRef (OCamlPrimitiveRef (OTuple3 v1 v2 v3)) = (renderDatatype v1) ++ (renderDatatype v2) ++ (renderDatatype v3)
getOCamlTypeParameterRef (OCamlPrimitiveRef (OTuple4 v1 v2 v3 v4)) = (renderDatatype v1) ++ (renderDatatype v2) ++ (renderDatatype v3) ++ (renderDatatype v4)
getOCamlTypeParameterRef (OCamlPrimitiveRef (OTuple5 v1 v2 v3 v4 v5)) = (renderDatatype v1) ++ (renderDatatype v2) ++ (renderDatatype v3) ++ (renderDatatype v4) ++ (renderDatatype v5)
getOCamlTypeParameterRef (OCamlPrimitiveRef (OTuple6 v1 v2 v3 v4 v5 v6)) = (renderDatatype v1) ++ (renderDatatype v2) ++ (renderDatatype v3) ++ (renderDatatype v4) ++ (renderDatatype v5) ++ (renderDatatype v6)
getOCamlTypeParameterRef _ = []

getOCamlValues :: ValueConstructor -> [Text]
getOCamlValues (NamedConstructor     _ value) = getOCamlTypeParameterRef value
getOCamlValues (RecordConstructor    _ value) = getOCamlTypeParameterRef value
getOCamlValues (MultipleConstructors cs)      = concat $ getOCamlValues <$> cs

renderTypeParameters' :: OCamlConstructor -> [Text]
renderTypeParameters' (OCamlValueConstructor vc) = getOCamlValues vc
renderTypeParameters' (OCamlSumOfRecordConstructor _ vc) = getOCamlValues vc
renderTypeParameters' _ = []

renderDatatype :: OCamlDatatype -> [Text]
renderDatatype (OCamlDatatype _ constructor) = renderTypeParameters' constructor
renderDatatype (OCamlPrimitive _primitive) = []

renderTypeParameters :: OCamlConstructor -> Doc
renderTypeParameters constructor = mkDocList $ stext <$> (nub $ renderTypeParameters' constructor)


-- | For Haskell Sum of Records, create OCaml record types of each RecordConstructorn
makeAuxTypeDef :: Text -> ValueConstructor -> Reader Options (Maybe (Doc,(Text,ValueConstructor)))
makeAuxTypeDef typeName c@(RecordConstructor rName _rValue) = do
  let newName = (stext $ textLowercaseFirst typeName) <> (stext rName)
  ctor <- render c
  return $ Just ((nest 2 $ "type" <+> newName <+> "=" <$$> ctor), (rName, (RecordConstructor (typeName <> rName) _rValue)))
makeAuxTypeDef _ _ = return Nothing

-- | A Haskell Sum of Records needs to be transformed into OCaml record types
--   and a sum type. Replace RecordConstructor with NamedConstructor.
replaceRecordConstructors :: [(Text,ValueConstructor)] -> ValueConstructor -> ValueConstructor
replaceRecordConstructors newConstructors rc@(RecordConstructor oldName _) = 
  case length mrc > 0 of
    False -> rc
    True  -> head mrc
  where
    rplc (oldName', (RecordConstructor newName _rValue)) =
      if oldName == oldName' then (Just $ NamedConstructor oldName' (OCamlRef newName)) else Nothing
    rplc _ = Nothing
    mrc = catMaybes $ rplc <$> newConstructors
replaceRecordConstructors _ rc = rc

instance HasType OCamlDatatype where
  render d@(OCamlDatatype typeName constructor@(OCamlSumOfRecordConstructor _ (MultipleConstructors css))) = do
    -- for each constructor, if it is a record constructor
    -- make a special new one, other wise do normal things
    vs' <- catMaybes <$> sequence (makeAuxTypeDef typeName <$> css)
    let vs = msuffix (line <> line) (fst <$> vs')
    let cs' = replaceRecordConstructors (snd <$> vs') <$> css
    let typeParameters = renderTypeParameters constructor
    name <- renderRef d
    ctor <- render (OCamlValueConstructor $ MultipleConstructors cs')
    return $ vs <> (nest 2 $ "type" <+> typeParameters <+> name <+> "=" <$$> "|" <+> ctor)

  render d@(OCamlDatatype _ constructor@(OCamlValueConstructor (RecordConstructor _ _))) = do
    let typeParameters = renderTypeParameters constructor
    name <- renderRef d
    ctor <- render constructor
    return . nest 2 $ "type" <+> typeParameters <+> name <+> "=" <$$> ctor

  render d@(OCamlDatatype _typeName cs@(OCamlValueConstructor (MultipleConstructors _css))) = do
    let typeParameters = renderTypeParameters cs
    name <- renderRef d
    ctor <- render cs
    return . nest 2 $ "type" <+> typeParameters <+> name <+> "=" <$$> "|" <+> ctor

  render d@(OCamlDatatype _ constructor) = do
    let typeParameters = renderTypeParameters constructor
    name <- renderRef d
    ctor <- render constructor
    return . nest 2 $ "type" <+> typeParameters <+> name <+> "=" <$$> "|" <+> ctor

  render (OCamlPrimitive primitive) = renderRef primitive

instance HasTypeRef OCamlDatatype where
  renderRef d@(OCamlDatatype typeName _) = pure (stext $ (if isTypeParameterRef d then "'" else "") <> textLowercaseFirst typeName)
  renderRef (OCamlPrimitive primitive) = renderRef primitive

instance HasType OCamlConstructor where
  render (OCamlValueConstructor value) = render value
  render (OCamlSumOfRecordConstructor _ value) = render value
  render (OCamlEnumeratorConstructor constructors) = do
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType ValueConstructor where
  render (RecordConstructor _ value) = do
    dv <- renderRecord value
    return $ "{" <+> dv <$$> "}"
  render (NamedConstructor constructorName (OCamlEmpty)) = do
    dv <- render OCamlEmpty
    return $ stext constructorName <+> dv
  render (NamedConstructor constructorName value) = do
    dv <- render value
    return $ stext constructorName <+> "of" <+> dv
  render (MultipleConstructors constructors) = do
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType EnumeratorConstructor where
  render (EnumeratorConstructor name) = pure (stext name)

instance HasType OCamlValue where
  render (OCamlRef name) = pure (stext $ textLowercaseFirst name)
  render (OCamlTypeParameterRef name) = pure (stext ("'" <> name))
  render (OCamlPrimitiveRef primitive) = reasonRefParens primitive <$> renderRef primitive
  render OCamlEmpty = pure (text "")
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <+> "*" <+> dy
  render (OCamlField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- renderRecord value
    return $ stext (fieldModifier name) <+> ":" <+> dv

instance HasRecordType OCamlValue where
  renderRecord (OCamlPrimitiveRef primitive) = renderRef primitive
  renderRecord (Values x y) = do
    dx <- renderRecord x
    dy <- renderRecord y
    return $ dx <$$> ";" <+> dy
  renderRecord value = render value

instance HasTypeRef OCamlPrimitive where
  renderRef (OList (OCamlPrimitive OChar)) = renderRef OString
  renderRef (OList datatype) = do
    dt <- renderRef datatype
    return $ parens dt <+> "list"
  renderRef (OTuple2 a b) = do
    da <- renderRef a
    db <- renderRef b
    return . parens $ da <+> "*" <+> db
  renderRef (OTuple3 a b c) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    return . parens $ da <+> "*" <+> db <+> "*" <+> dc
  renderRef (OTuple4 a b c d) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    return . parens $ da <+> "*" <+> db <+> "*" <+> dc <+> "*" <+> dd
  renderRef (OTuple5 a b c d e) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    de <- renderRef e
    return . parens $ da <+> "*" <+> db <+> "*" <+> dc <+> "*" <+> dd <+> "*" <+> de
  renderRef (OTuple6 a b c d e f) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    de <- renderRef e
    df <- renderRef f
    return . parens $ da <+> "*" <+> db <+> "*" <+> dc <+> "*" <+> dd <+> "*" <+> de <+> "*" <+> df
  renderRef (OOption datatype) = do
    dt <- renderRef datatype
    return $ parens dt <+> "option"
  renderRef (ODict k v) = do
    dk <- renderRef k
    dv <- renderRef v
    return $ "Dict" <+> parens dk <+> parens dv
  renderRef OInt    = pure "int"
  renderRef ODate   = pure "Js_date.t"
  renderRef OBool   = pure "bool"
  renderRef OChar   = pure "string"
  renderRef OString = pure "string"
  renderRef OUnit   = pure "unit"
  renderRef OFloat  = pure "float"
--  'a Js.Dict.t
toOCamlTypeRefWith :: OCamlType a => Options -> a -> T.Text
toOCamlTypeRefWith options x =
  pprinter $ runReader (renderRef (toOCamlType x)) options

toOCamlTypeRef :: OCamlType a => a -> T.Text
toOCamlTypeRef = toOCamlTypeRefWith defaultOptions

toOCamlTypeSourceWith :: OCamlType a => Options -> a -> T.Text
toOCamlTypeSourceWith options x =
  pprinter $ runReader (render (toOCamlType x)) options

toOCamlTypeSource :: OCamlType a => a -> T.Text
toOCamlTypeSource = toOCamlTypeSourceWith defaultOptions

-- | Puts parentheses around the doc of an elm ref if it contains spaces.
reasonRefParens :: OCamlPrimitive -> Doc -> Doc
reasonRefParens (OList (OCamlPrimitive OChar)) = id
reasonRefParens (OList _) = parens
reasonRefParens (OOption _) = parens
reasonRefParens (ODict _ _) = parens
reasonRefParens _ = id
