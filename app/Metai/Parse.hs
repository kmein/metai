{-# LANGUAGE OverloadedStrings #-}

module Metai.Parse where

import qualified Data.Text as Text
import qualified Data.Text.Normalize as Text
import Metai.Extra (debug)
import Numeric.Natural (Natural)
import Data.Csv
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as ByteString


data Line = Line { lineBook :: Natural, lineVerse :: Natural, lineText :: Text.Text }
  deriving (Show)

instance FromNamedRecord Line where
  parseNamedRecord l = Line <$> (l .: "Book") <*> (l .: "Line") <*> (normalize <$> l .: "Text")

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
        . Text.replace " -" "-"
        . Text.replace " /" "/"
        . Text.normalize Text.NFD
        . Text.toLower

metaiLines :: IO [Line]
metaiLines = do
    csvData <- ByteString.readFile "metai.csv"
    case decodeByName csvData :: Either String (Header, Vector.Vector Line) of
        Left err -> error err
        Right (_header, v) ->
          let _ = debug "header" _header `seq` ()
           in return $ Vector.toList v
