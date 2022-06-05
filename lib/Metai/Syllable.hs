{-# LANGUAGE LambdaCase #-}

module Metai.Syllable (Syllable, syllabify, segments, onset, rhyme, nucleus, coda, syllable) where

import Data.Maybe (maybeToList)
import Metai.Token (TextToken (..), Phonology(..), tokenIsVowel)
import Text.Megaparsec
import Data.Void (Void)

-------------------------------------------------------------------------------
-- Syllable definition + helper functions
-------------------------------------------------------------------------------

newtype Syllable = Syllable {getSyllable :: [TextToken]}
    deriving (Show)

segments, onset, rhyme, nucleus, coda :: Syllable -> [TextToken]
segments =
    filter
        ( \case
            Sound _ _ -> True
            _ -> False
        )
        . getSyllable
onset = takeWhile (not . tokenIsVowel) . segments
rhyme = dropWhile (not . tokenIsVowel) . segments
nucleus = takeWhile tokenIsVowel . rhyme
coda = dropWhile tokenIsVowel . rhyme

-------------------------------------------------------------------------------
-- Syllabification algorithm
-------------------------------------------------------------------------------

syllabify :: [TextToken] -> [Syllable]
syllabify toks = case parse (syllable `someTill` eof) "" (reverse toks) of
  Right syls -> reverse syls
  Left e -> error (show e) -- reverse . fromJust . parseMaybe (syllable `someTill` eof) . reverse

-- parse in reverse: figure out syllables from right to left
syllable :: Parsec Void [TextToken] Syllable
syllable = do
  afterPunctuation <- many $ satisfy (== Punctuation)
  finalS <- optional $ satisfy $ \case
    Sound Sibilant x | x == 's' -> True -- special case for 1.314
    _ -> False
  imperativeOrInfinitive <- optional $ satisfy $ \case
    Sound Plosive x | x == 'k' || x == 't' -> True
    _ -> False
  codaSibilant <- optionalTwice sibilant
  codaPlosive <- optionalTwice plosive
  codaResonant <- optionalTwice resonant -- sometimes written twice; e.g. 1.33
  vowels <- some vowel
  onglide <- optional $ satisfy (== Sound Resonant 'j') -- needed for 1.15 Kurmjei
  onsetResonant <- optional resonant
  onsetPlosive <- optional plosive
  onsetSibilant <- optional sibilant
  _ <- optional (satisfy (== SyllableBreak))
  beforePunctuation <- many $ satisfy (== Punctuation)
  return $ Syllable $ concat
    [ beforePunctuation
    , maybeToList onsetSibilant
    , maybeToList onsetPlosive
    , maybeToList onsetResonant
    , maybeToList onglide
    , reverse vowels
    , codaResonant
    , codaPlosive
    , codaSibilant
    , maybeToList imperativeOrInfinitive
    , maybeToList finalS
    , afterPunctuation
    ]
  where
    optionalTwice parser = do
      x <- optional parser
      y <- optional parser
      return $ maybeToList x ++ maybeToList y
    vowel = satisfy tokenIsVowel
    plosive = satisfy $ \case
      Sound Plosive _ -> True
      _ -> False
    sibilant = satisfy $ \case
      Sound Sibilant _ -> True
      _ -> False
    resonant = satisfy $ \case
      Sound Resonant _ -> True
      _ -> False
