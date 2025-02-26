{-# LANGUAGE OverloadedStrings #-}

module Metai.Parse where

import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import qualified Data.Text as Text
import qualified Data.Text.ICU.Replace as Regex
import qualified Data.Text.Normalize as Text
import qualified Data.Vector as Vector
import Metai.Extra (debug)
import Numeric.Natural (Natural)

data Line = Line {lineId :: Text.Text, lineBook :: Natural, lineVerse :: Text.Text, lineText :: Text.Text}
    deriving (Show)

instance FromNamedRecord Line where
    parseNamedRecord l = Line <$> (l .: "ID") <*> (l .: "Book") <*> (l .: "Line") <*> (normalize <$> l .: "Text")

normalize :: Text.Text -> Text.Text
normalize =
    Text.replace "sz" "š"
        . Text.replace "cz" "č"
        . Text.replace "dz" "dž"
        . Text.replace "ż" "ž"
        . Text.replace "ź" "ž"
        . Text.replace "cź" "č"
        . Text.replace "sź" "š"
        . Text.replace "ſ" "s"
        . Text.replace "ʒ" "z"
        . Text.replace "w" "v"
        . Text.replace "ů" "uo"
        . Text.replace "å" "ă" -- PL 611
        . Text.replace "ë" "ie"
        . Text.replace "ı" "i"
        . Text.replace "n\x304" "nn" -- nn abbreviation with macron, e.g. PL 595
        . Text.normalize Text.NFD
        . Text.toLower
        . Text.replace "A., B, C." "A, Bė, Cė" -- PL 348
        . Text.replace "`I" "Ì" -- RG ŽR
        . Text.replace "’" ""
        . Text.replace "n\x306’i" "n’i\x306" -- PL 239
        . Regex.replaceAll "\\s(;|/|\\[|]|\\||–)" ","
        . Regex.replaceAll "(^: |[<>{}„“])" ""

metaiLines :: IO [Line]
metaiLines = do
    csvData <- ByteString.getContents
    case decodeByName csvData :: Either String (Header, Vector.Vector Line) of
        Left err -> error err
        Right (_header, v) ->
            let _ = debug "header" _header `seq` ()
             in return $ Vector.toList v
