{-# LANGUAGE OverloadedStrings #-}

module OCaml.Record
  ( toOCamlTypeRef
  , toOCamlTypeRefWith
  , toOCamlTypeSource
  , toOCamlTypeSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           OCaml.Common
import           OCaml.Type
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

import qualified Data.List as L

class HasType a where
  render :: a -> Reader Options Doc

class HasRecordType a where
  renderRecord :: a -> Reader Options Doc

class HasTypeRef a where
  renderRef :: a -> Reader Options Doc


getOCamlTypeParameterRef :: OCamlValue -> [Doc]
getOCamlTypeParameterRef (OCamlTypeParameterRef name) = [stext ("'" <> name)]
getOCamlTypeParameterRef (OCamlField _ v1) = getOCamlTypeParameterRef v1
getOCamlTypeParameterRef (Values v1 v2) = getOCamlTypeParameterRef v1 ++ getOCamlTypeParameterRef v2
getOCamlTypeParameterRef _ = []

getOCamlValues :: ValueConstructor -> [Doc]
getOCamlValues (NamedConstructor     _ value) = getOCamlTypeParameterRef value
getOCamlValues (RecordConstructor    _ value) = getOCamlTypeParameterRef value
getOCamlValues (MultipleConstructors cs)      = concat $ getOCamlValues <$> cs

renderTypeParameters :: OCamlConstructor -> Reader Options Doc
renderTypeParameters (OCamlValueConstructor vc) = do
  let vc' = (getOCamlValues vc)
  return $ foldl (<+>) "" $ if length vc' > 1 then  ["("] <> (L.intersperse "," vc') <> [")"] else (vc')
renderTypeParameters (OCamlSumOfRecordConstructor vc) = do
  let vc' = (getOCamlValues vc)
  return $ foldl (<+>) "" $ if length vc' > 1 then  ["("] <> (L.intersperse "," vc') <> [")"] else (vc')
renderTypeParameters _ = return ""

{-
ps' = if length ps > 1 then ["("] <> (L.intersperse "," ps) <> [")"] else (L.intersperse "," ps)

makeTypeParameter :: OCamlValue -> Reader Options (Maybe Doc)
makeTypeParameter (OCamlTypeParamterRef name) = return $ Just name
makeTypeParameter _ = Nothing
-}

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
  render d@(OCamlDatatype typeName constructor@(OCamlSumOfRecordConstructor (MultipleConstructors css))) = do
    -- for each constructor, if it is a record constructor
    -- make a special new one, other wise do normal things
    vs' <- catMaybes <$> sequence (makeAuxTypeDef typeName <$> css)
    let vs = msuffix (line <> line) (fst <$> vs')
    let cs' = replaceRecordConstructors (snd <$> vs') <$> css
    typeParameters <- renderTypeParameters constructor
    name <- renderRef d
    ctor <- render (OCamlValueConstructor $ MultipleConstructors cs')
    return $ vs <> (nest 2 $ "type" <+> typeParameters <+> name <+> "=" <$$> "|" <+> ctor)

  render d@(OCamlDatatype _ constructor@(OCamlValueConstructor (RecordConstructor _ _))) = do
    typeParameters <- renderTypeParameters constructor
    name <- renderRef d
    ctor <- render constructor
    return . nest 2 $ "type" <+> typeParameters <+> name <+> "=" <$$> ctor

  render d@(OCamlDatatype _typeName cs@(OCamlValueConstructor (MultipleConstructors _css))) = do
    typeParameters <- renderTypeParameters cs
    name <- renderRef d
    ctor <- render cs
    return . nest 2 $ "type" <+> typeParameters <+> name <+> "=" <$$> "|" <+> ctor

  render d@(OCamlDatatype _ constructor) = do
    typeParameters <- renderTypeParameters constructor
    name <- renderRef d
    ctor <- render constructor
    return . nest 2 $ "type" <+> typeParameters <+> name <+> "=" <$$> "|" <+> ctor

  render (OCamlPrimitive primitive) = renderRef primitive

instance HasTypeRef OCamlDatatype where
  renderRef (OCamlDatatype typeName _) = pure (stext $ textLowercaseFirst typeName)
  renderRef (OCamlPrimitive primitive) = renderRef primitive

instance HasType OCamlConstructor where
  render (OCamlValueConstructor value) = render value
  render (OCamlSumOfRecordConstructor value) = render value
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
  renderRef ODate   = pure "Js.Date"
  renderRef OBool   = pure "bool"
  renderRef OChar   = pure "string"
  renderRef OString = pure "string"
  renderRef OUnit   = pure "unit"
  renderRef OFloat  = pure "Js.Float"
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
