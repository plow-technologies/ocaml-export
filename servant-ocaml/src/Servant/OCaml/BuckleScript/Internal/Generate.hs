{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.OCaml.BuckleScript.Internal.Generate where

import           Prelude                      hiding ((<$>))
import           Control.Lens                 (to, (^.))
import           Data.List                    (nub)
import           Data.Maybe                   (catMaybes)
import           Data.Proxy                   (Proxy)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as L
import qualified Data.Text.Encoding           as T
import           OCaml.BuckleScript.Types
  (OCamlDatatype(..), OCamlPrimitive(..), toOCamlType)
import qualified OCaml.Export as OCaml
import qualified OCaml.Internal.Common as OCaml
import qualified OCaml.BuckleScript.Encode as OCaml
import qualified OCaml.BuckleScript.Decode as OCaml
import qualified OCaml.BuckleScript.Record as OCaml
import Servant.API (NoContent (..))
import Servant.OCaml.BuckleScript.Internal.Foreign (LangOCaml, getEndpoints)
import Servant.OCaml.BuckleScript.Internal.Orphan ()
import qualified Servant.Foreign              as F
import Text.PrettyPrint.Leijen.Text


data OCamlOptions = OCamlOptions
  { {- | The protocol, host and any path prefix to be used as the base for all
    requests.
    Example: @Static "https://mydomain.com/api/v1"@
    When @Dynamic@, the generated OCaml functions take the base URL as the first
    argument.
    -}
    urlPrefix                 :: UrlPrefix
  , ocamlExportOptions :: OCaml.Options
    -- ^ Options to pass to ocaml-export
  , emptyResponseOCamlTypes     :: [OCamlDatatype]
    -- ^ Types that represent an empty Http response.
  , stringOCamlTypes   :: [OCamlDatatype]
    -- ^ Types that represent a String.
}

data UrlPrefix
  = Static T.Text
  | Dynamic


{-|
Default options for generating OCaml code.

The default options are:

> { urlPrefix =
>     Static ""
> , ocamlExportOptions =
>     OCaml.defaultOptions
> , emptyResponseOCamlTypes =
>     [ toOCamlType NoContent ]
> , stringOCamlTypes =
>     [ toOCamlType "" ]
> }
-}
defOCamlOptions :: OCamlOptions
defOCamlOptions = OCamlOptions
  { urlPrefix = Static ""
  , ocamlExportOptions = OCaml.defaultOptions
  , emptyResponseOCamlTypes =
      [ toOCamlType NoContent
      , toOCamlType ()
      ]
  , stringOCamlTypes =
      [ toOCamlType ("" :: String)
      , toOCamlType ("" :: T.Text)
      ]
  }


{-|
Default imports required by generated OCaml code.

You probably want to include this at the top of your generated OCaml module.

The default required imports are:

> import Json.Decode exposing (..)
> import Json.Decode.Pipeline exposing (..)
> import Json.Encode
> import Http
> import String
-}
defOCamlImports :: Text
defOCamlImports =
  T.unlines
    [--  "import Json.Decode exposing (..)"
    -- , "import Json.Decode.Pipeline exposing (..)"
    -- , "import Json.Encode"
    -- , "import Http"
    -- , "import String"
    ]


{-|
Generate OCaml code for the API with default options.

Returns a list of OCaml functions to query your Servant API from OCaml.

You could spit these out to a file and call them from your OCaml code, but you
would be better off creating a 'Spec' with the result and using 'specsToDir',
which handles the module name for you.
-}
generateOCamlForAPI
  :: ( F.HasForeign LangOCaml OCamlDatatype api
     , F.GenerateList OCamlDatatype (F.Foreign OCamlDatatype api))
  => Proxy api
  -> [Text]
generateOCamlForAPI =
  generateOCamlForAPIWith defOCamlOptions


{-|
Generate OCaml code for the API with custom options.
-}
generateOCamlForAPIWith
  :: ( F.HasForeign LangOCaml OCamlDatatype api
     , F.GenerateList OCamlDatatype (F.Foreign OCamlDatatype api))
  => OCamlOptions
  -> Proxy api
  -> [Text]
generateOCamlForAPIWith opts =
  nub . map docToText . map (generateOCamlForRequest opts) . getEndpoints

i :: Int
i = 4

{-|
Generate an OCaml function for one endpoint.
-}
generateOCamlForRequest :: OCamlOptions -> F.Req OCamlDatatype -> Doc
generateOCamlForRequest opts request =
  funcDef
  where
    funcDef =
      vsep
        [ fnName <+> ":" <+> typeSignature
        , fnName <+> args <+> equals
        , case letParams of
            Just params ->
              indent i
              (vsep ["let"
                    , indent i params
                    , "in"
                    , indent i ocamlRequest
                    ])
            Nothing ->
              indent i ocamlRequest
        ]

    fnName =
      request ^. F.reqFuncName . to (T.replace "-" "" . F.camelCase) . to stext

    typeSignature =
      mkTypeSignature opts request

    args =
      mkArgs opts request

    letParams =
      mkLetParams opts request

    ocamlRequest =
      mkRequest opts request

mkTypeSignature :: OCamlOptions -> F.Req OCamlDatatype -> Doc
mkTypeSignature opts request =
  (hsep . punctuate " ->" . concat)
    [ catMaybes [urlPrefixType]
    , headerTypes
    , urlCaptureTypes
    , queryTypes
    , catMaybes [bodyType, returnType]
    ]
  where
    urlPrefixType :: Maybe Doc
    urlPrefixType =
        case (urlPrefix opts) of
          Dynamic -> Just "String"
          Static _ -> Nothing

    ocamlTypeRef :: OCamlDatatype -> Doc
    ocamlTypeRef ocamlType =
      -- stext (OCaml.toOCamlType ocamlType)
      stext (OCaml.toOCamlTypeSourceWith (ocamlExportOptions opts) ocamlType)

    headerTypes :: [Doc]
    headerTypes =
      [ header ^. F.headerArg . F.argType . to ocamlTypeRef
      | header <- request ^. F.reqHeaders
      , isNotCookie header
      ]

    urlCaptureTypes :: [Doc]
    urlCaptureTypes =
        [ F.captureArg capture ^. F.argType . to ocamlTypeRef
        | capture <- request ^. F.reqUrl . F.path
        , F.isCapture capture
        ]

    queryTypes :: [Doc]
    queryTypes =
      [ arg ^. F.queryArgName . F.argType . to ocamlTypeRef
      | arg <- request ^. F.reqUrl . F.queryStr
      ]

    bodyType :: Maybe Doc
    bodyType =
        fmap ocamlTypeRef $ request ^. F.reqBody

    returnType :: Maybe Doc
    returnType = do
      result <- fmap ocamlTypeRef $ request ^. F.reqReturnType
      pure ("Http.Request" <+> parens result)


ocamlHeaderArg :: F.HeaderArg OCamlDatatype -> Doc
ocamlHeaderArg header =
  "header_" <>
  header ^. F.headerArg . F.argName . to (stext . T.replace "-" "_" . F.unPathSegment)


ocamlCaptureArg :: F.Segment OCamlDatatype -> Doc
ocamlCaptureArg segment =
  "capture_" <>
  F.captureArg segment ^. F.argName . to (stext . F.unPathSegment)


ocamlQueryArg :: F.QueryArg OCamlDatatype -> Doc
ocamlQueryArg arg =
  "query_" <>
  arg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


ocamlBodyArg :: Doc
ocamlBodyArg =
  "body"


isNotCookie :: F.HeaderArg f -> Bool
isNotCookie header =
   header
     ^. F.headerArg
      . F.argName
      . to ((/= "cookie") . T.toLower . F.unPathSegment)


mkArgs
  :: OCamlOptions
  -> F.Req OCamlDatatype
  -> Doc
mkArgs opts request =
  (hsep . concat) $
    [ -- Dynamic url prefix
      case urlPrefix opts of
        Dynamic -> ["urlBase"]
        Static _ -> []
    , -- Headers
      [ ocamlHeaderArg header
      | header <- request ^. F.reqHeaders
      , isNotCookie header
      ]
    , -- URL Captures
      [ ocamlCaptureArg segment
      | segment <- request ^. F.reqUrl . F.path
      , F.isCapture segment
      ]
    , -- Query params
      [ ocamlQueryArg arg
      | arg <- request ^. F.reqUrl . F.queryStr
      ]
    , -- Request body
      maybe [] (const [ocamlBodyArg]) (request ^. F.reqBody)
    ]


mkLetParams :: OCamlOptions -> F.Req OCamlDatatype -> Maybe Doc
mkLetParams opts request =
  if null (request ^. F.reqUrl . F.queryStr) then
    Nothing
  else
    Just $ "params =" <$>
           indent i ("List.filter (not << String.isEmpty)" <$>
                      indent i (ocamlList params))
  where
    params :: [Doc]
    params = map paramToDoc (request ^. F.reqUrl . F.queryStr)

    paramToDoc :: F.QueryArg OCamlDatatype -> Doc
    paramToDoc qarg =
      -- something wrong with indentation here...
      case qarg ^. F.queryArgType of
        F.Normal ->
          let
            argType = qarg ^. F.queryArgName . F.argType
            wrapped = isOCamlMaybeType argType
            -- Don't use "toString" on OCaml Strings, otherwise we get extraneous quotes.
            toStringSrc =
              if isOCamlStringType opts argType || isOCamlMaybeStringType opts argType then
                ""
              else
                "toString >> "
          in
              (if wrapped then ocamlName else "Just" <+> ocamlName) <$>
              indent 4 ("|> Maybe.map" <+> parens (toStringSrc <> "Http.encodeUri >> (++)" <+> dquotes (name <> equals)) <$>
                        "|> Maybe.withDefault" <+> dquotes empty)

        F.Flag ->
            "if" <+> ocamlName <+> "then" <$>
            indent 4 (dquotes (name <> equals)) <$>
            indent 2 "else" <$>
            indent 4 (dquotes empty)

        F.List ->
            ocamlName <$>
            indent 4 ("|> List.map" <+> parens (backslash <> "val ->" <+> dquotes (name <> "[]=") <+> "++ (val |> toString |> Http.encodeUri)") <$>
                      "|> String.join" <+> dquotes "&")
      where
        ocamlName = ocamlQueryArg qarg
        name = qarg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


mkRequest :: OCamlOptions -> F.Req OCamlDatatype -> Doc
mkRequest opts request =
  "Http.request" <$>
  indent i
    (ocamlRecord
       [ "method =" <$>
         indent i (dquotes method)
       , "headers =" <$>
         indent i
           (ocamlListOfMaybes headers)
       , "url =" <$>
         indent i url
       , "body =" <$>
         indent i body
       , "expect =" <$>
         indent i expect
       , "timeout =" <$>
         indent i "Nothing"
       , "withCredentials =" <$>
         indent i "False"
       ])
  where
    method =
       request ^. F.reqMethod . to (stext . T.decodeUtf8)

    mkHeader header =
      let headerName = header ^. F.headerArg . F.argName . to (stext . F.unPathSegment)
          headerArgName = ocamlHeaderArg header
          argType = header ^. F.headerArg . F.argType
          wrapped = isOCamlMaybeType argType
          toStringSrc =
            if isOCamlMaybeStringType opts argType || isOCamlStringType opts argType then
              mempty
            else
              " << toString"
      in
        "Maybe.map" <+> parens (("Http.header" <+> dquotes headerName <> toStringSrc))
        <+>
        (if wrapped then headerArgName else parens ("Just" <+> headerArgName))

    headers =
      [ mkHeader header
      | header <- request ^. F.reqHeaders
      , isNotCookie header
      ]

    url =
      mkUrl opts (request ^. F.reqUrl . F.path)
       <> mkQueryParams request

    body =
      case request ^. F.reqBody of
        Nothing ->
          "Http.emptyBody"

        Just ocamlTypeExpr ->
          let
            encoderName =
              OCaml.toOCamlEncoderSourceWith (ocamlExportOptions opts) ocamlTypeExpr
          in
            "Http.jsonBody" <+> parens (stext encoderName <+> ocamlBodyArg)

    expect =
      case request ^. F.reqReturnType of
        Just ocamlTypeExpr | isEmptyType opts ocamlTypeExpr ->
          let ocamlConstructor =
                -- OCaml.toOCamlType ocamlTypeExpr
                OCaml.toOCamlTypeSourceWith (ocamlExportOptions opts) ocamlTypeExpr
          in
            "Http.expectStringResponse" <$>
            indent i (parens (backslash <> braces " body " <+> "->" <$>
                              indent i ("if String.isEmpty body then" <$>
                                        indent i "Ok" <+> stext ocamlConstructor <$>
                                        "else" <$>
                                        indent i ("Err" <+> dquotes "Expected the response body to be empty")) <> line))


        Just ocamlTypeExpr ->
          -- "Http.expectJson" <+> stext (OCaml.toOCamlDecoderRefWith (ocamlExportOptions opts) ocamlTypeExpr)
          "Http.expectJson" <+> stext (OCaml.toOCamlDecoderSourceWith (ocamlExportOptions opts) ocamlTypeExpr)

        Nothing ->
          error "mkHttpRequest: no reqReturnType?"


mkUrl :: OCamlOptions -> [F.Segment OCamlDatatype] -> Doc
mkUrl opts segments =
  "String.join" <+> dquotes "/" <$>
  (indent i . ocamlList)
    ( case urlPrefix opts of
        Dynamic -> "urlBase"
        Static url -> dquotes (stext url)
      : map segmentToDoc segments)
  where

    segmentToDoc :: F.Segment OCamlDatatype -> Doc
    segmentToDoc s =
      case F.unSegment s of
        F.Static path ->
          dquotes (stext (F.unPathSegment path))
        F.Cap arg ->
          let
            -- Don't use "toString" on OCaml Strings, otherwise we get extraneous quotes.
            toStringSrc =
              if isOCamlStringType opts (arg ^. F.argType) then
                empty
              else
                " |> toString"
          in
            (ocamlCaptureArg s) <> toStringSrc <> " |> Http.encodeUri"


mkQueryParams
  :: F.Req OCamlDatatype
  -> Doc
mkQueryParams request =
  if null (request ^. F.reqUrl . F.queryStr) then
    empty
  else
    line <> "++" <+> align ("if List.isEmpty params then" <$>
                            indent i (dquotes empty) <$>
                            "else" <$>
                            indent i (dquotes "?" <+> "++ String.join" <+> dquotes "&" <+> "params"))


{- | Determines whether we construct an OCaml function that expects an empty
response body.
-}
isEmptyType :: OCamlOptions -> OCamlDatatype -> Bool
isEmptyType opts ocamlTypeExpr =
  ocamlTypeExpr `elem` emptyResponseOCamlTypes opts


{- | Determines whether we call `toString` on URL captures and query params of
this type in OCaml.
-}
isOCamlStringType :: OCamlOptions -> OCamlDatatype -> Bool
isOCamlStringType opts ocamlTypeExpr =
  ocamlTypeExpr `elem` stringOCamlTypes opts

{- | Determines whether a type is 'Maybe a' where 'a' is something akin to a 'String'.
-}
isOCamlMaybeStringType :: OCamlOptions -> OCamlDatatype -> Bool
isOCamlMaybeStringType opts (OCamlPrimitive (OOption ocamlTypeExpr)) = ocamlTypeExpr `elem` stringOCamlTypes opts
isOCamlMaybeStringType _ _ = False

isOCamlMaybeType :: OCamlDatatype -> Bool
isOCamlMaybeType (OCamlPrimitive (OOption _)) = True
isOCamlMaybeType _ = False


-- Doc helpers


docToText :: Doc -> Text
docToText =
  L.toStrict . displayT . renderPretty 0.4 100

stext :: Text -> Doc
stext = text . L.fromStrict

ocamlRecord :: [Doc] -> Doc
ocamlRecord = encloseSep (lbrace <> space) (line <> rbrace) (comma <> space)

ocamlList :: [Doc] -> Doc
ocamlList [] = lbracket <> rbracket
ocamlList ds = lbracket <+> hsep (punctuate (line <> comma) ds) <$> rbracket

ocamlListOfMaybes :: [Doc] -> Doc
ocamlListOfMaybes [] = lbracket <> rbracket
ocamlListOfMaybes ds = "List.filterMap identity" <$> indent 4 (ocamlList ds)
