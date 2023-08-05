{-# LANGUAGE LambdaCase #-}

module Metai.Caesura where

import Data.List (intercalate)
import Metai.Hexameter (Foot (..), footToPattern)
import Metai.Syllable (Syllable, hasPunctuation)

data Caesura = Trithemimeres | Penthemimeres | KataTritonTrochaion | Hephthemimeres | PostQuartumTrochaeum | BucolicDiaeresis
    deriving (Eq, Ord, Bounded, Enum)

data CaesuraMarked = Unmarked | Marked deriving (Show)

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

hasCaesura :: [Foot] -> [[Syllable]] -> Caesura -> Maybe CaesuraMarked
hasCaesura scansion syllables caesura =
  if hasCaesura'
     then if hasPunctuation (concat syllables !! (caesuraPosition - 1))
             then Just Marked
             else Just Unmarked
     else Nothing
  where
    caesuraPosition = versePosition caesura scansion
    hasCaesura' =
      not ((caesura == KataTritonTrochaion) && (scansion !! 2 /= Dactyl))
          && not ((caesura == PostQuartumTrochaeum) && (scansion !! 3 /= Dactyl))
          && listBoundaryAfter caesuraPosition syllables
