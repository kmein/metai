{-# LANGUAGE LambdaCase #-}

module Metai.Hexameter where

import Data.List (genericLength, intercalate, nub, permutations)
import qualified Data.Text as Text
import Metai.Extra (debug, groupOn, minimumOn)
import Metai.Parse (Line (..))
import Metai.Syllable (Syllable, nucleus, rhyme, syllabify)
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

footToPattern :: Foot -> [Length]
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
        | any (hasDiacritics [Dot, Acute, Circumflex, Ogonek]) (nucleus syllable) = Long
        | length (rhyme syllable) > 1 = Long
        | otherwise = Short

stressPattern :: [Syllable] -> [Length]
stressPattern = disambiguateStresses . map deriveStress
  where
    deriveStress syllable
        | any (hasDiacritics [Grave, Acute, Circumflex]) (nucleus syllable) = Long
        | otherwise = Unknown
    disambiguateStresses stresses =
        map
            ( \stress ->
                if stress == Unknown && any (== Long) stresses
                    then Short
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
                                debug' (name ++ " distance") $
                                    distance
                                        (debug' (name ++ " analysis") $ concatMap footToPattern analysis)
                                        (debug' (name ++ " pattern ") $ concatMap pattern syllables)
                            )
                            $ shapes
     in analyseWith "metre" metrePattern possibleShapes
            >>= analyseWith "stress" stressPattern
            >>= analyseWith "weight" weightPattern

distance :: [Length] -> [Length] -> Natural
distance x y = sum (zipWith distanceOne x y)
  where
    distanceOne Long Short = 1
    distanceOne Short Long = 1
    distanceOne _ _ = 0

possibleHexameterShapes :: Natural -> [[Foot]]
possibleHexameterShapes syllables
    | nDactyls > 0 && nSpondees > 0 && nDactyls + nSpondees == 6 =
        filter ((== Spondee) . last) . nub . permutations $
            replicate nDactyls Dactyl ++ replicate nSpondees Spondee
    | otherwise = []
  where
    -- these fall out of the system of equations: 3d + 2s = σ | d + s = 6
    nDactyls = fromIntegral syllables - 12
    nSpondees = 18 - fromIntegral syllables
