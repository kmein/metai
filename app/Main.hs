{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import Data.List (intercalate)
import qualified Data.Text as Text
import Metai.Hexameter (Foot (..), analyse)
import Metai.Parse (Line (..), metaiLines)
import Metai.Syllable (syllabify)
import Metai.Token (tokenize)

main :: IO ()
main = do
    let renderFoot = \case
            Dactyl -> 'D'
            Spondee -> 'S'
    allLines <- metaiLines
    ByteString.putStr $
        encode $
            map
                ( \line@Line{..} ->
                    ( lineBook
                    , lineVerse
                    , lineText
                    , length . concatMap (syllabify . tokenize) $ Text.words lineText
                    , case analyse line of
                        Just xs -> intercalate "|" (map (map renderFoot) xs)
                        Nothing -> "invalid"
                    )
                )
                allLines
