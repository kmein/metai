{-# LANGUAGE LambdaCase #-}

module Metai.Syllable (Syllable (segments), syllables, weightPattern, stressPattern, renderPattern) where

import Data.List.Split (splitOn)
import Data.Maybe (isNothing, mapMaybe)
import qualified Data.Text as Text
import Metai.Token (Diacritic (..), TextToken (..), hasDiacritics, isVowel, tokenIsVowel)

-------------------------------------------------------------------------------
-- Syllable definition + helper functions
-------------------------------------------------------------------------------

newtype Syllable = Syllable {segments :: [TextToken]}

onset, rhyme, nucleus, coda :: Syllable -> [TextToken]
onset = takeWhile (not . tokenIsVowel) . segments
rhyme = dropWhile (not . tokenIsVowel) . segments
nucleus = takeWhile tokenIsVowel . rhyme
coda = dropWhile tokenIsVowel . rhyme

-------------------------------------------------------------------------------
-- Syllabification algorithm
-------------------------------------------------------------------------------

syllables :: [TextToken] -> [Syllable]
syllables = killNonInitialExtrasyllabic . killInitialExtrasyllabic . maximizeOnset

killInitialExtrasyllabic :: [Syllable] -> [Syllable]
killInitialExtrasyllabic = \case
    syllables@(s1 : s2 : ss)
        | isExtrasyllabic s1 -> Syllable (segments s1 ++ segments s2) : ss
        | otherwise -> syllables
    syllables -> syllables

killNonInitialExtrasyllabic :: [Syllable] -> [Syllable]
killNonInitialExtrasyllabic = \case
    s1 : s2 : ss
      | isExtrasyllabic s2 -> Syllable (segments s1 ++ segments s2) : killNonInitialExtrasyllabic ss
      | otherwise -> s1 : killNonInitialExtrasyllabic (s2 : ss)
    ss -> ss

maximizeOnset :: [TextToken] -> [Syllable]
maximizeOnset tokens =
    map Syllable $
        splitOn [Space] $
            concatMap
                ( \case
                    (Sound classPrevious _, current@(Sound classCurrent _), Sound classNext _)
                        | classPrevious >= classCurrent && classCurrent < classNext -> [Space, current]
                    (_, current, _) -> [current]
                )
                $ zip3 (Space : tokens) tokens (tail tokens ++ [Space])

-- a "syllable" is extrasyllabic if it does not have a vocalic nucleus
isExtrasyllabic :: Syllable -> Bool
isExtrasyllabic syllable = case classes $ segments syllable of
    [] -> False
    xs -> not $ isVowel $ maximum xs
  where
    classes = mapMaybe $ \case
        Sound theClass _ -> Just theClass
        _ -> Nothing

-------------------------------------------------------------------------------
-- Syllable weight and word stress patterns
-------------------------------------------------------------------------------

weightPattern :: [Syllable] -> [Maybe Bool]
weightPattern = map deriveWeight
  where
    deriveWeight syllable
        | any (hasDiacritics [Dot, Acute, Circumflex, Ogonek]) (segments syllable) = Just True
        | length (rhyme syllable) > 1 = Just True
        | otherwise = Just False

stressPattern :: [Syllable] -> [Maybe Bool]
stressPattern =
    disambiguateStresses
        . map deriveStress
  where
    deriveStress (Syllable syllableSounds)
        | any (hasDiacritics [Grave, Acute, Circumflex]) syllableSounds = Just True
        | any (hasDiacritics [Breve]) syllableSounds = Just False
        | otherwise = Nothing
    disambiguateStresses stresses =
        map
            ( \stress ->
                if isNothing stress
                    then
                        if any (== Just True) stresses
                            then Just False
                            else
                                if length (filter isNothing stresses) == 1 -- are we the only undecided syllable
                                    then Just True
                                    else stress
                    else stress
            )
            stresses

renderPattern :: [Maybe Bool] -> String
renderPattern = map $ \case
    Just False -> '-'
    Just True -> '+'
    Nothing -> '?'
