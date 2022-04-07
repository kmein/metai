{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (genericLength)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector
import Metai.Extra
import Metai.Parse (Line (lineBook, lineText), metaiLines)
import Metai.Syllable (Syllable, renderPattern, syllables)
import Metai.Token (TextToken, tokenize)
import Statistics.Sample

data Summary = Summary
    { summaryMin :: Double
    , summaryMax :: Double
    , summaryMean :: Double
    , summaryStdDev :: Double
    }

instance Show Summary where
    show summary =
        unwords
            [ "min"
            , show (summaryMin summary)
            , "max"
            , show (summaryMax summary)
            , "mean"
            , show (summaryMean summary)
            , "stddev"
            , show (summaryStdDev summary)
            ]

data Statistic a = Statistic
    { overall :: a
    , perBook :: (a, a, a, a)
    }

instance Show a => Show (Statistic a) where
    show statistic =
        let (book1, book2, book3, book4) = perBook statistic
         in unlines ["Book 1: " ++ show book1, "Book 2: " ++ show book2, "Book 3: " ++ show book3, "Book 4: " ++ show book4, "Total: " ++ show (overall statistic)]

summarize :: Sample -> Summary
summarize sample =
    Summary
        { summaryMin = Vector.minimum sample
        , summaryMax = Vector.maximum sample
        , summaryMean = mean sample
        , summaryStdDev = stdDev sample
        }

calculate :: ([Line Text.Text] -> a) -> IO (Statistic a)
calculate measurement = do
    allLines <- metaiLines
    let linesOfBook n = filter ((== n) . lineBook) allLines
    return
        Statistic
            { overall = measurement allLines
            , perBook =
                ( measurement $ linesOfBook 1
                , measurement $ linesOfBook 2
                , measurement $ linesOfBook 3
                , measurement $ linesOfBook 4
                )
            }

evaluate :: ([Syllable] -> [Maybe Bool]) -> Line Text.Text -> [String]
evaluate pattern =
    map (renderPattern . pattern . syllables . fromJust . tokenize) . Text.words . lineText

main :: IO ()
main = do
    let syllablesOfLine :: Line Text.Text -> [[Syllable]]
        syllablesOfLine = map (syllables . fromJust . tokenize) . Text.words . lineText
        alertMe line = if length (concat (syllablesOfLine line)) < 12 then debug "less than 12 syllables" line else line
    print =<< calculate (\theLines -> summarize $ Vector.fromList $ map (genericLength . concat . syllablesOfLine . alertMe) theLines)
