{-# LANGUAGE OverloadedStrings #-}

module OCaml.Common where

import           Data.Char (toLower,toUpper)
import qualified Data.List as L
import           Data.Monoid
import           Data.Text  (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Formatting hiding (stext, text)
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

data Options = Options
  { includeOCamlInterface :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {includeOCamlInterface = False}

cr :: Format r r
cr = now "\n"

mintercalate :: Monoid m => m -> [m] -> m
mintercalate _ [] = mempty
mintercalate _ [x] = x
mintercalate separator (x:xs) = x <> separator <> mintercalate separator xs

msuffix :: Monoid m => m -> [m] -> m
msuffix _ [] = mempty
msuffix suffix [x] = x <> suffix
msuffix suffix (x:xs) = x <> suffix <> msuffix suffix xs

mconcatWith :: Monoid m => m -> m -> [m] -> m
mconcatWith _   _   []     = mempty
mconcatWith pre suf [x]    = pre <> x <> suf
mconcatWith pre suf (x:xs) = pre <> x <> suf <> mconcatWith pre suf xs

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


-- | Parentheses of which the right parenthesis exists on a new line
newlineparens :: Doc -> Doc
newlineparens doc = "(" <> doc <$$> ")"

-- | An empty line, regardless of current indentation
emptyline :: Doc
emptyline = nest minBound linebreak

-- | Like <$$>, but with an empty line in between
(<$+$>) :: Doc -> Doc -> Doc
l <$+$> r = l <> emptyline <$$> r

squarebracks :: Doc -> Doc
squarebracks doc = "[" <+> doc <+> "]"

arraybrackets :: Doc -> Doc
arraybrackets doc = "[|" <+> doc <+> "|]"

pair :: Doc -> Doc -> Doc
pair l r = spaceparens $ l <> comma <+> r

lowercaseFirst :: String -> String
lowercaseFirst (hd:tl) = toLower hd : tl
lowercaseFirst [] = []

uppercaseFirst :: String -> String
uppercaseFirst (hd:tl) = toUpper hd : tl
uppercaseFirst [] = []

textLowercaseFirst :: Text -> Text
textLowercaseFirst = T.pack . lowercaseFirst . T.unpack

textUppercaseFirst :: Text -> Text
textUppercaseFirst = T.pack . uppercaseFirst . T.unpack

mkDocList :: [Doc] -> Doc
mkDocList ds =
  foldl (<>) "" $
    if length ds > 1
      then ["("] <> (L.intersperse ", " ds) <> [")"]
      else ds
