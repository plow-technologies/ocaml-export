{-# LANGUAGE OverloadedStrings #-}

module Reason.Record
  ( toReasonTypeRef
  , toReasonTypeRefWith
  , toReasonTypeSource
  , toReasonTypeSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.Text as T
import           Reason.Common
import           Reason.Type
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasType a where
  render :: a -> Reader Options Doc

class HasRecordType a where
  renderRecord :: a -> Reader Options Doc

class HasTypeRef a where
  renderRef :: a -> Reader Options Doc

instance HasType ReasonDatatype where
  render d@(ReasonDatatype _ constructor@(RecordConstructor _ _)) = do
    name <- renderRef d
    ctor <- render constructor
    return . nest 2 $ "type" <+> name <+> "=" <$$> ctor
  render d@(ReasonDatatype _ constructor) = do
    name <- renderRef d
    ctor <- render constructor
    return . nest 2 $ "type" <+> name <+> "=" <$$> "|" <+> ctor <+> ";"
  render (ReasonPrimitive primitive) = renderRef primitive

instance HasTypeRef ReasonDatatype where
  renderRef (ReasonDatatype typeName _) = pure (stext $ textLowercaseFirst typeName)
  renderRef (ReasonPrimitive primitive) = renderRef primitive

instance HasType ReasonConstructor where
  render (RecordConstructor _ value) = do
    dv <- renderRecord value
    return $ "{" <+> dv <$$> "};"
  render (NamedConstructor constructorName value) = do
    dv <- render value
    return $ stext constructorName <+> dv
  render (MultipleConstructors constructors) = do
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType ReasonValue where
  render (ReasonRef name) = pure (stext name)
  render (ReasonPrimitiveRef primitive) = reasonRefParens primitive <$> renderRef primitive
  render ReasonEmpty = pure (text "")
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <+> dy
  render (ReasonField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- renderRecord value
    return $ stext (fieldModifier name) <+> ":" <+> dv

instance HasRecordType ReasonValue where
  renderRecord (ReasonPrimitiveRef primitive) = renderRef primitive
  renderRecord (Values x y) = do
    dx <- renderRecord x
    dy <- renderRecord y
    return $ dx <$$> comma <+> dy
  renderRecord value = render value

instance HasTypeRef ReasonPrimitive where
  renderRef (RList (ReasonPrimitive RChar)) = renderRef RString
  renderRef (RList datatype) = do
    dt <- renderRef datatype
    return $ "list" <+> parens dt
  renderRef (RTuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return . parens $ dx <> comma <+> dy
  renderRef (RTuple3 x y z) = do
    dx <- renderRef x
    dy <- renderRef y
    dz <- renderRef z
    return . parens $ dx <> comma <+> dy <+> comma <+> dz
  renderRef (RTuple4 x y z a) = do
    dx <- renderRef x
    dy <- renderRef y
    dz <- renderRef z
    da <- renderRef a
    return . parens $ dx <> comma <+> dy <+> comma <+> dz  <+> comma <+> da
  renderRef (RTuple5 x y z a b) = do
    dx <- renderRef x
    dy <- renderRef y
    dz <- renderRef z
    da <- renderRef a
    db <- renderRef b
    return . parens $ dx <> comma <+> dy <+> comma <+> dz <+> comma <+> da <+> comma <+> db
  renderRef (RTuple6 x y z a b c) = do
    dx <- renderRef x
    dy <- renderRef y
    dz <- renderRef z
    da <- renderRef a
    db <- renderRef b
    dc <- renderRef c
    return . parens $ dx <> comma <+> dy <+> comma <+> dz <+> comma <+> da <+> comma <+> db <+> comma <+> dc
  renderRef (RMaybe datatype) = do
    dt <- renderRef datatype
    return $ "option" <+> parens dt
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
