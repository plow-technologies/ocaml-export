
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Reason.Decode
  ( toReasonDecoderRef
  , toReasonDecoderRefWith
  , toReasonDecoderSource
  , toReasonDecoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.Text as T
import           Reason.Common
import           Reason.Type
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasDecoder a where
  render :: a -> Reader Options Doc

class HasDecoderRef a where
  renderRef :: a -> Reader Options Doc

instance HasDecoder ReasonDatatype where
  render d@(ReasonDatatype name constructor) = do
    fnName <- renderRef d
    ctor <- render constructor
    return $
      (fnName <+> ": Decoder" <+> stext name) <$$>
      (fnName <+> "=" <$$> indent 4 ctor)
  render (ReasonPrimitive primitive) = renderRef primitive

instance HasDecoderRef ReasonDatatype where
  renderRef (ReasonDatatype name _) = pure $ "decode" <> stext name
  renderRef (ReasonPrimitive primitive) = renderRef primitive

instance HasDecoder ReasonConstructor where
  render (NamedConstructor name value) = do
    dv <- render value
    return $ "decode" <+> stext name <$$> indent 4 dv
  render (RecordConstructor name value) = do
    dv <- render value
    return $ "decode" <+> stext name <$$> indent 4 dv
  render mc@(MultipleConstructors constrs) = do
      cstrs <- mapM renderSum constrs
      pure $ constructorName <$$> indent 4
        ("|> andThen" <$$>
          indent 4 (newlineparens ("\\x ->" <$$>
            (indent 4 $ "case x of" <$$>
              (indent 4 $ foldl1 (<$+$>) cstrs <$+$>
               "_ ->" <$$> indent 4 "fail \"Constructor not matched\""
              )
            )
          ))
        )
    where
      constructorName :: Doc
      constructorName =
        if isEnumeration mc then "string" else "field \"tag\" string"

-- | required "contents"
requiredContents :: Doc
requiredContents = "required" <+> dquotes "contents"

-- | "<name>" -> decode <name>
renderSumCondition :: T.Text -> Doc -> Reader Options Doc
renderSumCondition name contents =
  pure $ dquotes (stext name) <+> "->" <$$>
    indent 4
      ("decode" <+> stext name <$$> indent 4 contents)

-- | Render a sum type constructor in context of a data type with multiple
-- constructors.
renderSum :: ReasonConstructor -> Reader Options Doc
renderSum (NamedConstructor name ReasonEmpty) = renderSumCondition name mempty
renderSum (NamedConstructor name v@(Values _ _)) = do
  (_, val) <- renderConstructorArgs 0 v
  renderSumCondition name val
renderSum (NamedConstructor name value) = do
  val <- render value
  renderSumCondition name $ "|>" <+> requiredContents <+> val
renderSum (RecordConstructor name value) = do
  val <- render value
  renderSumCondition name val
renderSum (MultipleConstructors constrs) =
  foldl1 (<$+$>) <$> mapM renderSum constrs

-- | Render the decoding of a constructor's arguments. Note the constructor must
-- be from a data type with multiple constructors and that it has multiple
-- constructors itself.
renderConstructorArgs :: Int -> ReasonValue -> Reader Options (Int, Doc)
renderConstructorArgs i (Values l r) = do
  (iL, rndrL) <- renderConstructorArgs i l
  (iR, rndrR) <- renderConstructorArgs (iL + 1) r
  pure (iR, rndrL <$$> rndrR)
renderConstructorArgs i val = do
  rndrVal <- render val
  let index = parens $ "index" <+> int i <+> rndrVal
  pure (i, "|>" <+> requiredContents <+> index)

instance HasDecoder ReasonValue where
  render (ReasonRef name) = pure $ "decode" <> stext name
  render (ReasonPrimitiveRef primitive) = renderRef primitive
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> dy
  render (ReasonField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- render value
    return $ "|> required" <+> dquotes (stext (fieldModifier name)) <+> dv
  render ReasonEmpty = pure (stext "")
  
instance HasDecoderRef ReasonPrimitive where
  renderRef (RList (ReasonPrimitive RChar)) = pure "string"
  renderRef (RList datatype) = do
    dt <- renderRef datatype
    return . parens $ "list" <+> dt
  renderRef (RDict key value) = do
    d <- renderRef (RList (ReasonPrimitive (RTuple2 (ReasonPrimitive key) value)))
    return . parens $ "map Dict.fromList" <+> d
  renderRef (RMaybe datatype) = do
    dt <- renderRef datatype
    return . parens $ "maybe" <+> dt
  renderRef (RTuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return . parens $
      "map2 (,)" <+> parens ("index 0" <+> dx) <+> parens ("index 1" <+> dy)
  renderRef RUnit = pure $ parens "succeed ()"
  renderRef RDate = pure "decodeDate"
  renderRef RInt = pure "int"
  renderRef RBool = pure "bool"
  renderRef RChar = pure "char"
  renderRef RFloat = pure "float"
  renderRef RString = pure "string"

toReasonDecoderRefWith :: ReasonType a => Options -> a -> T.Text
toReasonDecoderRefWith options x = pprinter $ runReader (renderRef (toReasonType x)) options

toReasonDecoderRef :: ReasonType a => a -> T.Text
toReasonDecoderRef = toReasonDecoderRefWith defaultOptions

toReasonDecoderSourceWith :: ReasonType a => Options -> a -> T.Text
toReasonDecoderSourceWith options x = pprinter $ runReader (render (toReasonType x)) options

toReasonDecoderSource :: ReasonType a => a -> T.Text
toReasonDecoderSource = toReasonDecoderSourceWith defaultOptions

