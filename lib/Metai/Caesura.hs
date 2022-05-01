{-# LANGUAGE LambdaCase #-}

module Metai.Caesura where

import Data.List (intercalate, sort)
import qualified Data.Text as Text
import Metai.Hexameter (Foot (..), footToPattern)
import Metai.Parse (Line (..))
import Metai.Syllable (syllabify)
import Metai.Token (tokenize)

data Caesura = Trithemimeres | Penthemimeres | KataTritonTrochaion | Hephthemimeres | BucolicDiaeresis
  deriving (Eq, Ord)

instance Show Caesura where
    show = \case
        Trithemimeres -> "3H"
        Penthemimeres -> "5H"
        Hephthemimeres -> "7H"
        BucolicDiaeresis -> "BD"
        KataTritonTrochaion -> "KTT"
    showList xs x = intercalate "+" (map show xs) ++ x

versePosition :: Caesura -> [Foot] -> Int
versePosition caesura feet = case caesura of
    Trithemimeres -> countSyllables (take 1 feet) + 1
    Penthemimeres -> countSyllables (take 2 feet) + 1
    Hephthemimeres -> countSyllables (take 3 feet) + 1
    BucolicDiaeresis -> countSyllables (take 4 feet)
    KataTritonTrochaion -> versePosition Penthemimeres feet + 1
  where
    countSyllables = sum . map (length . footToPattern)

listBoundaryAfter :: Int -> [[a]] -> Bool
listBoundaryAfter nElements xsxss =
    case xsxss of
        [] -> False
        (xs : xss)
            | lengthFirst == nElements -> True
            | lengthFirst > nElements -> False
            | lengthFirst < nElements -> listBoundaryAfter (nElements - lengthFirst) xss
            | otherwise -> error "derp"
          where
            lengthFirst = length xs

caesuras :: Maybe [[Foot]] -> Line -> Maybe [[Caesura]]
caesuras analysis line =
    map
        ( \scansion ->
          sort
            (filter
                (\caesura -> listBoundaryAfter (versePosition caesura scansion) syllables)
                [Trithemimeres, Penthemimeres, Hephthemimeres, BucolicDiaeresis]
                ++ if (scansion !! 2 == Dactyl) && listBoundaryAfter (versePosition KataTritonTrochaion scansion) syllables
                    then [KataTritonTrochaion]
                    else [])
        )
        <$> analysis
  where
    syllables = map (syllabify . tokenize) $ Text.words $ lineText line
