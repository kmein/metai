{-# LANGUAGE LambdaCase #-}

module Metai.Caesura where

import Data.List (intercalate)
import qualified Data.Text as Text
import Metai.Hexameter (Foot (..), footToPattern)
import Metai.Parse (Line (..))
import Metai.Syllable (Syllable, syllabify)
import Metai.Token (tokenize)

data Caesura = Trithemimeres | Penthemimeres | KataTritonTrochaion | Hephthemimeres | PostQuartumTrochaeum | BucolicDiaeresis
    deriving (Eq, Bounded, Enum)

allCaesuras :: [Caesura]
allCaesuras = [minBound .. maxBound]

instance Show Caesura where
    show = \case
        Trithemimeres -> "3h"
        Penthemimeres -> "5h"
        Hephthemimeres -> "7h"
        BucolicDiaeresis -> "bd"
        KataTritonTrochaion -> "ktt"
        PostQuartumTrochaeum -> "pqt"
    showList xs x = intercalate "+" (map show xs) ++ x

versePosition :: Caesura -> [Foot] -> Int
versePosition caesura feet = case caesura of
    Trithemimeres -> countSyllables (take 1 feet) + 1
    Penthemimeres -> countSyllables (take 2 feet) + 1
    Hephthemimeres -> countSyllables (take 3 feet) + 1
    BucolicDiaeresis -> countSyllables (take 4 feet)
    KataTritonTrochaion -> versePosition Penthemimeres feet + 1
    PostQuartumTrochaeum -> versePosition Hephthemimeres feet + 1
  where
    countSyllables = sum . map (length . footToPattern)

listBoundaryAfter :: Int -> [[a]] -> Bool
listBoundaryAfter nElements =
    elem 0 . scanl (\remainingSyllables word -> remainingSyllables - length word) nElements

hasCaesura :: [Foot] -> [[Syllable]] -> Caesura -> Bool
hasCaesura scansion syllables caesura =
    (if caesura == KataTritonTrochaion then scansion !! 2 == Dactyl else True)
        && (if caesura == PostQuartumTrochaeum then scansion !! 3 == Dactyl else True)
        && listBoundaryAfter (versePosition caesura scansion) syllables

caesuras :: Maybe [[Foot]] -> Line -> Maybe [[Caesura]]
caesuras analysis line =
    map
        ( \scansion ->
            filter
                (hasCaesura scansion syllables)
                [Trithemimeres, Penthemimeres, KataTritonTrochaion, Hephthemimeres, PostQuartumTrochaeum, BucolicDiaeresis]
        )
        <$> analysis
  where
    syllables = map (syllabify . tokenize) $ Text.words $ lineText line
