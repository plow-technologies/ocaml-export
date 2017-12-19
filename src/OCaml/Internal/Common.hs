{-|
Module      : OCaml.Internal.Common
Description : Internal utility functions
Copyright   : Plow Technologies, 2017
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : experimental

-}


{-# LANGUAGE OverloadedStrings #-}

module OCaml.Internal.Common where

-- base
import Data.Char (toLower,toUpper)
import qualified Data.List as L
import Data.Monoid

-- aeson
import qualified Data.Aeson.Types as Aeson (Options(..), defaultOptions)

-- containers
import qualified Data.Map.Strict as Map

-- formatting
import Formatting hiding (stext, text)

-- ocaml-export
import OCaml.BuckleScript.Types (HaskellTypeMetaData, OCamlTypeMetaData)

-- text
import Data.Text  (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- wl-pprint
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))


-- | For URLs and POSIX systems.
(</>) :: Text -> Text -> Text
(</>) a b =
    if hasEndingSeparator sA
    then
      if hasLeadingSeparator sB
      then T.pack $ sA ++ drop 1 sB
      else T.pack $ sA ++ sB
    else
      T.pack $ sA ++ "/" ++ sB
  where
    sA = T.unpack a
    sB = T.unpack b
    
    hasEndingSeparator :: String -> Bool
    hasEndingSeparator lst@(_hd:_tl) = (==) '/' . last $ lst
    hasEndingSeparator [] = False

    hasLeadingSeparator :: String -> Bool
    hasLeadingSeparator (hd:_tl) = hd == '/'
    hasLeadingSeparator [] = False

infix 5 </>


data Options = Options
  { includeOCamlInterface :: Bool
  , aesonOptions :: Aeson.Options
  , dependencies :: Map.Map HaskellTypeMetaData OCamlTypeMetaData
  }

defaultOptions :: Options
defaultOptions = Options {includeOCamlInterface = False, aesonOptions = Aeson.defaultOptions, dependencies = Map.empty}

data TypeMetaData = TypeMetaData
  { topLevelOCamlTypeMetaData :: Maybe OCamlTypeMetaData -- Just if not primitive
  , userOptions :: Options
  }

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

linesBetween :: [Doc] -> Doc
linesBetween docs =
  let extra = if length docs > 0 then line else ""
  in foldl (<>) "" $ (L.intersperse (line <> line) docs) <> [extra]

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

foldMod :: [Text] -> Text
foldMod = T.intercalate "."
