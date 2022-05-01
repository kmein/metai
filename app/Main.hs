{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import Data.List (intercalate)
import qualified Data.Text as Text
import Metai.Hexameter (Foot (..), analyse, score, weightPattern, metrePattern, stressPattern, footToPattern)
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
                  let
                    syllables = concatMap (syllabify . tokenize) $ Text.words lineText
                    analysis = analyse line
                    feet = map (concatMap footToPattern) <$> (analysis)
                  in
                    ( lineBook
                    , lineVerse
                    , lineText
                    , length syllables
                    , maybe "NA" (intercalate "|" . map (map renderFoot)) analysis
                    , maybe "NA" (intercalate "|" . map (show . score (metrePattern syllables))) feet
                    , maybe "NA" (intercalate "|" . map (show . score (stressPattern syllables))) feet
                    , maybe "NA" (intercalate "|" . map (show . score (weightPattern syllables))) feet
                    )
                )
                allLines
