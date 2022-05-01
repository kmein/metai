{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import Data.List (intercalate)
import Data.Map (Map)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Metai.Caesura (caesuras)
import Metai.Hexameter (Foot (..), analyse, footToPattern, metrePattern, score, stressPattern, weightPattern)
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
            [ "book"
            , "verse"
            , "text"
            , "syllables"
            , "scansion"
            , "caesuras"
            , "metre"
            , "metreConflict"
            , "stress"
            , "stressConflict"
            , "weight"
            , "weightConflict"
            ]
            $ map
                ( \line@Line{..} ->
                    let syllables = concatMap (syllabify . tokenize) $ Text.words lineText
                        analysis = analyse line
                        feet = map (concatMap footToPattern) <$> (analysis)
                        display f = pack . maybe "NA" (intercalate "|" . map f)
                        weights = weightPattern syllables
                        metres = metrePattern syllables
                        stresses = stressPattern syllables
                     in [ ("book", pack $ show lineBook)
                        , ("verse", pack $ show lineVerse)
                        , ("text", lineText)
                        , ("syllables", pack $ show $ length syllables)
                        , ("scansion", display (map renderFoot) analysis)
                        , ("caesuras", display show $ caesuras analysis line)
                        , ("metre", pack $ show metres)
                        , ("metreConflict", display (show . score metres) feet)
                        , ("stress", pack $ show stresses)
                        , ("stressConflict", display (show . score stresses) feet)
                        , ("weight", pack $ show weights)
                        , ("weightConflict", display (show . score weights) feet)
                        ] ::
                            Map Text Text
                )
                allLines
