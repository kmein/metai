{-# LANGUAGE LambdaCase #-}

module Metai.Hexameter where

import Control.Applicative (liftA2)
import Data.Function (on)
import Data.List (genericLength, groupBy, intercalate, nub, permutations, sortOn)
import Data.Maybe (isNothing, listToMaybe)
import qualified Data.Text as Text
import Metai.Extra (debug, groupOn, minimumOn)
import Metai.Parse (Line (..))
import Metai.Syllable (Syllable, nucleus, rhyme, segments, syllabify)
import Metai.Token (Diacritic (..), hasDiacritics, tokenize)
import Numeric.Natural (Natural)

data Length = Long | Short | Unknown
    deriving (Eq)

instance Show Length where
    show Long = "–"
    show Short = "⏑"
    show Unknown = "×"
    showList xs x = concatMap show xs ++ x

data Foot = Dactyl | Spondee
    deriving (Eq)

footToPattern = \case
    Dactyl -> [Long, Short, Short]
    Spondee -> [Long, Unknown]

instance Show Foot where
    show Dactyl = "–⏑⏑"
    show Spondee = "––"
    showList xs x = intercalate "|" (map show xs) ++ x

--------------------------------------------------------------------------------
-- Deducing syllable weights and word stress
--------------------------------------------------------------------------------

metrePattern :: [Syllable] -> [Length]
metrePattern = map deriveMetricalCharacter
  where
    deriveMetricalCharacter syllable
        | any (hasDiacritics [Breve]) (nucleus syllable) = Short
        | otherwise = Unknown

weightPattern :: [Syllable] -> [Length]
weightPattern = map deriveWeight
  where
    deriveWeight syllable
        | any (hasDiacritics [Dot, Acute, Circumflex, Ogonek]) core = Long
        | length (rhyme syllable) > 1 = Long
        | otherwise = Short
      where
        core = nucleus syllable

stressPattern :: [Syllable] -> [Length]
stressPattern =
    disambiguateStresses
        . map deriveStress
  where
    deriveStress syllable
        | any (hasDiacritics [Grave, Acute, Circumflex]) core = Long
        | otherwise = Unknown
      where
        core = nucleus syllable
    disambiguateStresses stresses =
        map
            ( \stress ->
                if stress == Unknown
                    then
                        if any (== Long) stresses
                            then Short
                            else
                                if length (filter (== Unknown) stresses) == 1 -- are we the only undecided syllable
                                    then Long
                                    else stress
                    else stress
            )
            stresses

---------------------------------------------------------------------
-- Scansion
---------------------------------------------------------------------

analyse :: Line -> Maybe [[Foot]]
analyse line =
    let debug' t = debug (show (lineBook line) ++ "." ++ show (lineVerse line) ++ " " ++ t)
        syllables = map (syllabify . tokenize) $ Text.words $ lineText line
        possibleShapes = possibleHexameterShapes $ genericLength $ concat syllables
        analyseWith name pattern shapes =
            fmap snd $
                debug' (name ++ " best") $
                    minimumOn fst $
                        groupOn
                            ( \analysis ->
                                debug' (name ++ " score   ") $
                                    score
                                        (debug' (name ++ " analysis") $ concatMap footToPattern analysis)
                                        (debug' (name ++ " pattern ") $ concatMap pattern syllables)
                            )
                            $ shapes
     in analyseWith "metre" metrePattern possibleShapes
            >>= analyseWith "stress" stressPattern
            >>= analyseWith "weight" weightPattern

score :: [Length] -> [Length] -> Natural
score x y = sum (zipWith scoreOne x y)
  where
    scoreOne Long Short = 1
    scoreOne Short Long = 1
    scoreOne _ _ = 0

possibleHexameterShapes :: Natural -> [[Foot]]
possibleHexameterShapes = \case
    17 -> generateShapes 5 1
    16 -> generateShapes 4 2
    15 -> generateShapes 3 3
    14 -> generateShapes 2 4
    13 -> generateShapes 1 5
    12 -> generateShapes 0 6
    _ -> []
  where
    dactyl = Dactyl
    spondee = Spondee
    generateShapes :: Int -> Int -> [[Foot]]
    generateShapes nDactyls nSpondees
        | nDactyls + nSpondees == 6 =
            map ((++ [spondee])) $
                nub $
                    permutations $
                        replicate nDactyls dactyl ++ replicate (nSpondees -1) spondee
        | otherwise = []
