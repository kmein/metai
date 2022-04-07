{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Metai.Parse (Line (..), metaiLines)
import Metai.Syllable (Syllable, renderPattern, syllabify)
import Metai.Token (tokenize)

evaluate :: ([Syllable] -> [Maybe Bool]) -> Line -> [String]
evaluate pattern =
    map (renderPattern . pattern . syllabify . fromJust . tokenize) . Text.words . lineText

main :: IO ()
main = do
    let countSyllables :: Line -> Int
        countSyllables = length . concatMap (syllabify . fromJust . tokenize) . Text.words . lineText
    allLines <- metaiLines
    let syllableCounts = fmap countSyllables allLines
    ByteString.putStr $
        encode $
            map (\(syllables, Line{..}) -> (lineBook, lineVerse, lineText, syllables)) $
                zip syllableCounts allLines
