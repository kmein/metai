{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Metai.Parse (Line (lineText))
import Metai.Syllable (Syllable, renderPattern, syllables)
import Metai.Token (TextToken, tokenize)

evaluate :: ([Syllable] -> [Maybe Bool]) -> Line Text.Text -> [String]
evaluate pattern =
    map (renderPattern . pattern . syllables . fromJust . tokenize) . Text.words . lineText

main :: IO ()
main = do
    return ()
