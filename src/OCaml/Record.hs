{-# LANGUAGE OverloadedStrings #-}

module OCaml.Record
  ( toReasonTypeRef
  , toReasonTypeRefWith
  , toReasonTypeSource
  , toReasonTypeSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Maybe (catMaybes)
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
{-
makeAuxTypeDef :: [Doc] -> Text -> ReasonConstructor -> Reader Options Doc
makeAuxTypeDef (d:ds) typeName c@(RecordConstructor rName _rValue) = do
  let newName = (stext $ textLowercaseFirst typeName) <> (stext rName)
  ctor <- render c
  return . nest 2 $ "type" <+> newName <+> "=" <$$> ctor
makeAuxTypeDef ds _ _ = return ""
-}
-- | For Haskell Sum of Records, create OCaml record types of each RecordConstructorn
makeAuxTypeDef :: Text -> ReasonConstructor -> Reader Options (Maybe (Doc,(Text,ReasonConstructor)))
makeAuxTypeDef typeName c@(RecordConstructor rName _rValue) = do
  let newName = (stext $ textLowercaseFirst typeName) <> (stext rName)
  ctor <- render c
  return $ Just ((nest 2 $ "type" <+> newName <+> "=" <$$> ctor), (rName, RecordConstructor (typeName <> rName) _rValue))
makeAuxTypeDef _ _ = return Nothing

-- | A Haskell Sum of Records needs to be transformed into OCaml record types
--   and a sum type. Replace RecordConstructor with NamedConstructor.
replaceRecordConstructors :: [(Text,ReasonConstructor)] -> ReasonConstructor -> ReasonConstructor
replaceRecordConstructors newConstructors rc@(RecordConstructor oldName _)  = 
  case length mrc > 0 of
    False -> rc
    True  -> head mrc
  where
    rplc (oldName', rc@(RecordConstructor newName _rValue)) =
      if oldName == oldName' then (Just $ NamedConstructor oldName' (ReasonRef newName)) else Nothing
    rplc _ = Nothing
    mrc = catMaybes $ rplc <$> newConstructors
replaceRecordConstructors _ rc = rc

instance HasType ReasonDatatype where
  render d@(ReasonDatatype _ constructor@(RecordConstructor _ _)) = do
    name <- renderRef d
    ctor <- render constructor
    return . nest 2 $ "type" <+> name <+> "=" <$$> ctor
  render d@(ReasonDatatype typeName cs@(MultipleConstructors css)) = do
    if isSumWithRecords cs
      then do
        -- for each constructor, if it is a record constructor
        -- make a special new one, other wise do normal things
        vs' <- catMaybes <$> sequence (makeAuxTypeDef typeName <$> css)
        let vs = msuffix (line <> line) (fst <$> vs')
        let cs' = replaceRecordConstructors (snd <$> vs') <$> css 
        name <- renderRef d
        ctor <- render (MultipleConstructors cs')
        return $ vs <> (nest 2 $ "type" <+> name <+> "=" <$$> "|" <+> ctor)
      else do
        name <- renderRef d
        ctor <- render cs
        return . nest 2 $ "type" <+> name <+> "=" <$$> "|" <+> ctor
  render d@(ReasonDatatype _ constructor) = do
    name <- renderRef d
    ctor <- render constructor
    return . nest 2 $ "type" <+> name <+> "=" <$$> "|" <+> ctor
  render (ReasonPrimitive primitive) = renderRef primitive

instance HasTypeRef ReasonDatatype where
  renderRef (ReasonDatatype typeName _) = pure (stext $ textLowercaseFirst typeName)
  renderRef (ReasonPrimitive primitive) = renderRef primitive

instance HasType ReasonConstructor where
  render (RecordConstructor _ value) = do
    dv <- renderRecord value
    return $ "{" <+> dv <$$> "}"
  render (NamedConstructor constructorName (ReasonEmpty)) = do
    dv <- render ReasonEmpty
    return $ stext constructorName <+> dv
  render (NamedConstructor constructorName value) = do
    dv <- render value
    return $ stext constructorName <+> "of" <+> dv
  render (MultipleConstructors constructors) = do
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType ReasonValue where
  render (ReasonRef name) = pure (stext $ textLowercaseFirst name)
  render (ReasonPrimitiveRef primitive) = reasonRefParens primitive <$> renderRef primitive
  render ReasonEmpty = pure (text "")
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <+> "*" <+> dy
  render (ReasonField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- renderRecord value
    return $ stext (fieldModifier name) <+> ":" <+> dv

instance HasRecordType ReasonValue where
  renderRecord (ReasonPrimitiveRef primitive) = renderRef primitive
  renderRecord (Values x y) = do
    dx <- renderRecord x
    dy <- renderRecord y
    return $ dx <$$> ";" <+> dy
  renderRecord value = render value

instance HasTypeRef ReasonPrimitive where
  renderRef (RList (ReasonPrimitive RChar)) = renderRef RString
  renderRef (RList datatype) = do
    dt <- renderRef datatype
    return $ parens dt <+> "list"
  renderRef (RTuple2 a b) = do
    da <- renderRef a
    db <- renderRef b
    return . parens $ da <+> "*" <+> db
  renderRef (RTuple3 a b c) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    return . parens $ da <+> "*" <+> db <+> "*" <+> dc
  renderRef (RTuple4 a b c d) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    return . parens $ da <+> "*" <+> db <+> "*" <+> dc <+> "*" <+> dd
  renderRef (RTuple5 a b c d e) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    de <- renderRef e
    return . parens $ da <+> "*" <+> db <+> "*" <+> dc <+> "*" <+> dd <+> "*" <+> de
  renderRef (RTuple6 a b c d e f) = do
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    dd <- renderRef d
    de <- renderRef e
    df <- renderRef f
    return . parens $ da <+> "*" <+> db <+> "*" <+> dc <+> "*" <+> dd <+> "*" <+> de <+> "*" <+> df
  renderRef (RMaybe datatype) = do
    dt <- renderRef datatype
    return $ parens dt <+> "option"
  renderRef (RDict k v) = do
    dk <- renderRef k
    dv <- renderRef v
    return $ "Dict" <+> parens dk <+> parens dv
  renderRef RInt    = pure "int"
  renderRef RDate   = pure "Js.Date"
  renderRef RBool   = pure "bool"
  renderRef RChar   = pure "string"
  renderRef RString = pure "string"
  renderRef RUnit   = pure "unit"
  renderRef RFloat  = pure "Js.Float"
--  'a Js.Dict.t
toReasonTypeRefWith :: ReasonType a => Options -> a -> T.Text
toReasonTypeRefWith options x =
  pprinter $ runReader (renderRef (toReasonType x)) options

toReasonTypeRef :: ReasonType a => a -> T.Text
toReasonTypeRef = toReasonTypeRefWith defaultOptions

toReasonTypeSourceWith :: ReasonType a => Options -> a -> T.Text
toReasonTypeSourceWith options x =
  pprinter $ runReader (render (toReasonType x)) options

toReasonTypeSource :: ReasonType a => a -> T.Text
toReasonTypeSource = toReasonTypeSourceWith defaultOptions

-- | Puts parentheses around the doc of an elm ref if it contains spaces.
reasonRefParens :: ReasonPrimitive -> Doc -> Doc
reasonRefParens (RList (ReasonPrimitive RChar)) = id
reasonRefParens (RList _) = parens
reasonRefParens (RMaybe _) = parens
reasonRefParens (RDict _ _) = parens
reasonRefParens _ = id
