{-# LANGUAGE OverloadedStrings #-}

module Reason.Common where

import           Data.Char (toLower)
import           Data.Monoid
import           Data.Text  (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Formatting hiding (text)
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

data Options = Options
  { fieldLabelModifier :: Text -> Text
  }

defaultOptions :: Options
defaultOptions = Options {fieldLabelModifier = id}

cr :: Format r r
cr = now "\n"

mintercalate :: Monoid m => m -> [m] -> m
mintercalate _ [] = mempty
mintercalate _ [x] = x
mintercalate separator (x:xs) = x <> separator <> mintercalate separator xs

mintercalatefinish :: Monoid m => m -> m -> [m] -> m
mintercalatefinish _ _ [] = mempty
mintercalatefinish _ f [x] = x <> f
mintercalatefinish separator f (x:xs) = x <> separator <> (mintercalatefinish separator f xs)

pprinter :: Doc -> Text
pprinter = LT.toStrict . displayT . renderPretty 0.4 100

stext :: Data.Text.Text -> Doc
stext = text . LT.fromStrict

spaceparens :: Doc -> Doc
spaceparens doc = "(" <+> doc <+> ")"

lowercaseFirst :: String -> String
lowercaseFirst (hd:tl) = toLower hd : tl
lowercaseFirst [] = []

textLowercaseFirst :: Text -> Text
textLowercaseFirst = T.pack . lowercaseFirst . T.unpack
