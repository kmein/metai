{-# LANGUAGE LambdaCase #-}

module Metai.Syllable (Syllable, syllabify, segments, onset, rhyme, nucleus, coda, syllable) where

import Data.Maybe (maybeToList)
import Data.Void (Void)
import Metai.Token (Phonology (..), TextToken (..), tokenIsVowel)
import Text.Megaparsec

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
    finalS <- optional $
        satisfy $ \case
            Sound Sibilant x | x == 's' -> True -- special case for 1.314
            _ -> False
    imperativeOrInfinitive <- optional $
        satisfy $ \case
            Sound Occlusive x | x == 'k' || x == 't' -> True
            _ -> False
    codaSibilant <- optionalTwice sibilant
    codaOcclusive <- optionalTwice occlusive
    codaResonant <- optionalTwice resonant -- sometimes written twice; e.g. 1.33
    vowels <- some vowel
    onglide <- optional $ satisfy (== Sound Resonant 'j') -- needed for spellings such as Kurmjei, pirmjaus
    onsetResonant <- optional resonant
    onsetOcclusive <- optional occlusive
    onsetSibilant <- optional sibilant -- 1.423 kàd wiſſ miegôt'
    _ <- optional (satisfy (== SyllableBreak))
    beforePunctuation <- many $ satisfy (== Punctuation)
    return $
        Syllable $
            concat
                [ beforePunctuation
                , maybeToList onsetSibilant
                , maybeToList onsetOcclusive
                , maybeToList onsetResonant
                , maybeToList onglide
                , reverse vowels
                , codaResonant
                , codaOcclusive
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
    occlusive = satisfy $ \case
        Sound Occlusive _ -> True
        _ -> False
    sibilant = satisfy $ \case
        Sound Sibilant _ -> True
        _ -> False
    resonant = satisfy $ \case
        Sound Resonant _ -> True
        _ -> False
