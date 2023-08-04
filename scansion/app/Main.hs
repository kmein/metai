{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Vector as Vector
import Metai.Caesura (allCaesuras, caesuras)
import Metai.Hexameter (Foot (..), analyse, distance, footToPattern, metrePattern, stressPattern, weightPattern)
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
        encodeByName
            ( [ "book"
              , "verse"
              , "text"
              , "words"
              , "syllables"
              , "scansion"
              ]
                <> Vector.fromList (map (encodeUtf8 . pack . show) allCaesuras)
                <> [ "metre"
                   , "metreConflict"
                   , "stress"
                   , "stressConflict"
                   , "weight"
                   , "weightConflict"
                   ]
            )
            $ map
                ( \line@Line{..} ->
                    let lineWords = Text.words lineText
                        syllables = map (syllabify . tokenize) lineWords
                        analysis = analyse line
                        feet = map (concatMap footToPattern) <$> (analysis)
                        display f = pack . maybe "NA" (intercalate "|" . map f)
                        weights = concatMap weightPattern syllables
                        metres = concatMap metrePattern syllables
                        stresses = concatMap stressPattern syllables
                        lineCaesuras = caesuras analysis line
                     in [ ("book", pack $ show lineBook)
                        , ("verse", lineVerse)
                        , ("text", lineText)
                        , ("syllables", pack $ show $ length $ concat syllables)
                        , ("words", pack $ show $ length lineWords)
                        , ("scansion", display (map renderFoot) analysis)
                        ]
                            `Map.union` Map.fromList (map (\c -> (pack $ show c, display show (map (elem c) <$> lineCaesuras))) allCaesuras)
                            `Map.union` [
                                          -- , ("caesuras", display show $ caesuras analysis line)
                                          ("metre", pack $ show metres)
                                        , ("metreConflict", display (show . distance metres) feet)
                                        , ("stress", pack $ show stresses)
                                        , ("stressConflict", display (show . distance stresses) feet)
                                        , ("weight", pack $ show weights)
                                        , ("weightConflict", display (show . distance weights) feet)
                                        ] ::
                            Map Text Text
                )
                allLines
