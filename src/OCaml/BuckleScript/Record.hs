{-|
Module      : OCaml.BuckleScript.Record
Description : Create OCaml data types from Haskell data types
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

-}


{-# LANGUAGE OverloadedStrings #-}

module OCaml.BuckleScript.Record
  ( toOCamlTypeRef
  , toOCamlTypeRefWith
  , toOCamlTypeSource
  , toOCamlTypeSourceWith

  , toOCamlTypeSourceWith'
  ) where

import           Control.Monad.Reader
import           Data.List (nub)
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

-- ocaml-export
import           OCaml.BuckleScript.Types
import           OCaml.Common


-- | render a Haskell data type in OCaml
class HasType a where
  render :: a -> Reader Options Doc

class HasRecordType a where
  renderRecord :: a -> Reader Options Doc

class HasTypeRef a where
  renderRef :: a -> Reader Options Doc

instance HasType OCamlDatatype where
  render datatype@(OCamlDatatype _ typeName constructor@(OCamlSumOfRecordConstructor _ (MultipleConstructors constructors))) = do
    -- For each constructor, if it is a record constructor, declare a type for that record
    -- before and separate form the main sum type.
    sumRecordsData <- catMaybes <$> sequence (renderSumRecord typeName <$> constructors)
    let sumRecords = msuffix (line <> line) (fst <$> sumRecordsData)
        newConstructors = replaceRecordConstructors (snd <$> sumRecordsData) <$> constructors
        typeParameters = renderTypeParameters constructor
    fnName <- renderRef datatype
    fnBody <- render (OCamlValueConstructor $ MultipleConstructors newConstructors)
    pure $ sumRecords <> ("type" <+> typeParameters <+> fnName <+> "=" <$$> indent 2 ("|" <+> fnBody))

  render datatype@(OCamlDatatype _ _ constructor@(OCamlValueConstructor (RecordConstructor _ _))) = do
    let typeParameters = renderTypeParameters constructor
    fnName <- renderRef datatype
    fnBody <- render constructor
    pure $ "type" <+> typeParameters <+> fnName <+> "=" <$$> indent 2 fnBody

  render datatype@(OCamlDatatype _ _ constructor) = do
    let typeParameters = renderTypeParameters constructor
    fnName <- renderRef datatype
    fnBody <- render constructor
    pure $ "type" <+> typeParameters <+> fnName <+> "=" <$$> indent 2 ("|" <+> fnBody)

  render (OCamlPrimitive primitive) = renderRef primitive

instance HasTypeRef OCamlDatatype where
  renderRef datatype@(OCamlDatatype _ typeName _) = pure $ stext $ (if isTypeParameterRef datatype then "'" else "") <> textLowercaseFirst typeName
  renderRef (OCamlPrimitive primitive) = renderRef primitive

instance HasType OCamlConstructor where
  render (OCamlValueConstructor value) = render value
  render (OCamlSumOfRecordConstructor _ value) = render value
  render (OCamlEnumeratorConstructor constructors) = do
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType ValueConstructor where
  -- record
  render (RecordConstructor _ value) = do
    fields <- renderRecord value
    pure $ "{" <+> fields <$$> "}"

  -- enumerator
  render (NamedConstructor constructorName (OCamlEmpty)) = do
    pure $ stext constructorName

  -- product
  render (NamedConstructor constructorName value) = do
    types <- render value
    pure $ stext constructorName <+> "of" <+> types

  -- sum
  render (MultipleConstructors constructors) = do
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType EnumeratorConstructor where
  render (EnumeratorConstructor name) = pure (stext name)

instance HasType OCamlValue where
  render (OCamlRef name) = pure (stext $ textLowercaseFirst name)
  render (OCamlTypeParameterRef name) = pure (stext ("'" <> name))
  render (OCamlPrimitiveRef primitive) = ocamlRefParens primitive <$> renderRef primitive
  render OCamlEmpty = pure (text "")
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <+> "*" <+> dy
  render (OCamlField name value) = do
    dv <- renderRecord value
    return $ stext name <+> ":" <+> dv

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
  renderRef (OEither k v) = do
    dk <- renderRef k
    dv <- renderRef v
    return $ (parens $ dk <> "," <+> dv) <+> "Aeson.Compatibility.Either.t"
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

toOCamlTypeSourceWith' :: OCamlType a => a -> Doc
toOCamlTypeSourceWith' x = runReader (render (toOCamlType x)) defaultOptions


toOCamlTypeSource :: OCamlType a => a -> T.Text
toOCamlTypeSource = toOCamlTypeSourceWith defaultOptions


-- Util functions

-- | A Haskell Sum of Records needs to be transformed into OCaml record types
--   and a sum type. Replace RecordConstructor with NamedConstructor.
replaceRecordConstructors :: [(Text,ValueConstructor)] -> ValueConstructor -> ValueConstructor
replaceRecordConstructors newConstructors recordConstructor@(RecordConstructor oldName _) = 
  case length newRecordConstructor > 0 of
    False -> recordConstructor
    True  -> head newRecordConstructor
  where
    replace (oldName', (RecordConstructor newName _value)) =
      if oldName == oldName' then (Just $ NamedConstructor oldName' (OCamlRef newName)) else Nothing
    replace _ = Nothing
    newRecordConstructor = catMaybes $ replace <$> newConstructors

replaceRecordConstructors _ rc = rc

-- | Given a constructor, output a list of type parameters.
--   (Maybe a) -> 'a0 list -> ["'a0"]
--   (Either a b) -> 'a0 'a1 list -> ["'a0","'a1"]
renderTypeParameters :: OCamlConstructor -> Doc
renderTypeParameters constructor = mkDocList $ stext . (<>) "'" <$> (nub $ getTypeParameters constructor)

-- | For Haskell Sum of Records, create OCaml record types of each RecordConstructor
renderSumRecord :: Text -> ValueConstructor -> Reader Options (Maybe (Doc,(Text,ValueConstructor)))
renderSumRecord typeName constructor@(RecordConstructor name value) = do
  let sumRecordName = typeName <> name
  functionBody <- render constructor
  pure $ Just (("type" <+> (stext (textLowercaseFirst sumRecordName)) <+> "=" <$$> indent 2 functionBody), (name, (RecordConstructor sumRecordName value)))
renderSumRecord _ _ = return Nothing


-- | Puts parentheses around the doc of an OCaml ref if it contains spaces.
ocamlRefParens :: OCamlPrimitive -> Doc -> Doc
ocamlRefParens (OList (OCamlPrimitive OChar)) = id
ocamlRefParens (OList _) = parens
ocamlRefParens (OOption _) = parens
ocamlRefParens (ODict _ _) = parens
ocamlRefParens _ = id
