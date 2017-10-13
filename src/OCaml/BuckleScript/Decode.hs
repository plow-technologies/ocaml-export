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
import           Data.Monoid
import qualified Data.Text as T
import           OCaml.Common
import           OCaml.Type
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasDecoder a where
  render :: a -> Reader Options Doc

class HasDecoderRef a where
  renderRef :: a -> Reader Options Doc

instance HasDecoder OCamlDatatype where
  render d@(OCamlDatatype name constructor) = do
    fnName <- renderRef d
    ctor <- render constructor
    return $
      (fnName <+> ": Decoder" <+> stext name) <$$>
      (fnName <+> "=" <$$> indent 4 ctor)
  render (OCamlPrimitive primitive) = renderRef primitive

instance HasDecoderRef OCamlDatatype where
  renderRef (OCamlDatatype name _) = pure $ "decode" <> stext name
  renderRef (OCamlPrimitive primitive) = renderRef primitive

instance HasDecoder OCamlConstructor where
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
renderSum :: OCamlConstructor -> Reader Options Doc
renderSum (NamedConstructor name OCamlEmpty) = renderSumCondition name mempty
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
renderConstructorArgs :: Int -> OCamlValue -> Reader Options (Int, Doc)
renderConstructorArgs i (Values l r) = do
  (iL, rndrL) <- renderConstructorArgs i l
  (iR, rndrR) <- renderConstructorArgs (iL + 1) r
  pure (iR, rndrL <$$> rndrR)
renderConstructorArgs i val = do
  rndrVal <- render val
  let index = parens $ "index" <+> int i <+> rndrVal
  pure (i, "|>" <+> requiredContents <+> index)

instance HasDecoder OCamlValue where
  render (OCamlRef name) = pure $ "decode" <> stext name
  render (OCamlPrimitiveRef primitive) = renderRef primitive
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> dy
  render (OCamlField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- render value
    return $ "|> required" <+> dquotes (stext (fieldModifier name)) <+> dv
  render OCamlEmpty = pure (stext "")
  
instance HasDecoderRef OCamlPrimitive where
  renderRef (OList (OCamlPrimitive OChar)) = pure "string"
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
    return . parens $
      "map2 (,)" <+> parens ("index 0" <+> dx) <+> parens ("index 1" <+> dy)
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

