{-# LANGUAGE OverloadedStrings #-}

module Metai.Parse where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Normalize as Text
import Metai.Extra (Parser)
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Line a = Line { lineBook :: Natural, lineVerse :: Natural, lineText :: a }
  deriving (Show)

parseLines :: Text.Text -> Maybe [Line Text.Text]
parseLines = parseMaybe (many line)
  where
    line :: Parser (Line Text.Text)
    line =
        Line
            <$> (decimal <* char '.')
            <*> (decimal <* space)
            <*> ((normalize <$> Text.pack <$> (anySingle `someTill` eol)))

normalize :: Text.Text -> Text.Text
normalize =
    Text.replace "sz" "š"
        . Text.replace "cz" "č"
        . Text.replace "ż" "ž"
        . Text.replace "ź" "ž"
        . Text.replace "ž" "ž"
        . Text.replace "cź" "č"
        . Text.replace "sź" "š"
        . Text.replace "cż" "č"
        . Text.replace "ſ" "s"
        . Text.replace "ʒ" "z"
        . Text.replace "ů" "uo"
        . Text.replace "ë" "ie"
        . Text.replace "ı" "i"
        . Text.replace "'" ""
        . Text.replace "]" ""
        . Text.replace "[" ""
        . Text.replace "\"" ""
        . Text.normalize Text.NFD
        . Text.toLower

metaiLines :: IO [Line Text.Text]
metaiLines =
    maybe (error "lines did not parse") return
        =<< parseLines <$> Text.readFile "metai.txt"
