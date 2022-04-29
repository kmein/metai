{-# LANGUAGE LambdaCase #-}

module Metai.Syllable (Syllable, syllabify, segments, onset, rhyme, nucleus, coda) where

import Data.List.Split (splitOn)
import Data.Maybe (isNothing, mapMaybe)
import Metai.Token (Diacritic (..), TextToken (..), hasDiacritics, isVowel, tokenIsVowel)

-------------------------------------------------------------------------------
-- Syllable definition + helper functions
-------------------------------------------------------------------------------

newtype Syllable = Syllable {getSyllable :: [TextToken]}
  deriving (Show)

segments, onset, rhyme, nucleus, coda :: Syllable -> [TextToken]
segments = filter (\case
  Sound _ _ -> True
  _ -> False) . getSyllable
onset = takeWhile (not . tokenIsVowel) . segments
rhyme = dropWhile (not . tokenIsVowel) . segments
nucleus = takeWhile tokenIsVowel . rhyme
coda = dropWhile tokenIsVowel . rhyme

-------------------------------------------------------------------------------
-- Syllabification algorithm
-------------------------------------------------------------------------------

syllabify :: [TextToken] -> [Syllable]
syllabify = killNonInitialExtrasyllabic . killInitialExtrasyllabic . maximizeOnset

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
                $ zip3 (Space : tokens') tokens' (tail tokens' ++ [Space])
    where tokens' = map (\case
              SyllableBreak -> Space -- manual syllable separator is -
              x -> x) tokens

-- a "syllable" is extrasyllabic if it does not have a vocalic nucleus
isExtrasyllabic :: Syllable -> Bool
isExtrasyllabic syllable = case classes $ segments syllable of
    [] -> False
    xs -> not $ isVowel $ maximum xs
  where
    classes = mapMaybe $ \case
        Sound theClass _ -> Just theClass
        _ -> Nothing
