{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join)
import Data.Function (on)
import Data.List (intercalate, intersect, tails)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing, mapMaybe, fromJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Normalize as Text
import Data.Void (Void)
import Debug.Trace (traceShow)
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol, punctuationChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

debug :: Show a => a -> a
debug = join traceShow

type Parser = Parsec Void Text.Text

type Line a = ((Natural, Natural), a)

parseLines :: Text.Text -> Maybe [Line Text.Text]
parseLines = parseMaybe (many line)
  where
    line :: Parser (Line Text.Text)
    line = (\book verse line -> ((book, verse), line)) <$> (decimal <* char '.') <*> (decimal <* space) <*> ((normalize <$> Text.pack <$> (anySingle `someTill` eol)))

normalize :: Text.Text -> Text.Text
normalize =
    Text.replace "sz" "š"
        . Text.replace "cz" "č"
        . Text.replace "ż" "ž"
        . Text.replace "ź" "ž"
        . Text.replace "ž" "ž"
        . Text.replace "cź" "č"
        . Text.replace "sź" "š"
        . Text.replace "cż" "č"
        . Text.replace "ſ" "s"
        . Text.replace "ʒ" "z"
        . Text.replace "ů" "uo"
        . Text.replace "ë" "ie"
        . Text.replace "ı" "i"
        . Text.replace "'" ""
        . Text.replace "]" ""
        . Text.replace "[" ""
        . Text.replace "\"" ""
        . Text.normalize Text.NFD
        . Text.toLower

data TextToken = Space | Punctuation | Sound Class Char
    deriving (Show, Eq)

renderTokens :: [TextToken] -> String
renderTokens = map $ \case
    Sound _ c -> c
    Punctuation -> '/'
    Space -> ' '

data Class
    = Vowel [Diacritic]
    | Glide
    | Rhotic
    | Lateral
    | Nasal
    | Affricate Voice
    | Fricative Voice
    | Plosive Voice
    deriving (Show, Eq)

data Voice = Voiced | Unvoiced
    deriving (Show, Eq)

instance Ord Class where
    compare =
        compare `on` \case
            Plosive _ -> 1
            Affricate _ -> 2
            Fricative Unvoiced -> 2
            Fricative Voiced -> 3
            Nasal -> 4
            Lateral -> 5
            Rhotic -> 5
            Glide -> 6
            Vowel _ -> 7

syllables :: [TextToken] -> [[TextToken]]
syllables = killNonInitialExtrasyllabic . killInitialExtrasyllabic . syllabify
  where
    killInitialExtrasyllabic = \case
        s1 : s2 : ss -> if isExtrasyllabic s1 then (s1 ++ s2) : ss else s1 : s2 : ss
        ss -> ss
    killNonInitialExtrasyllabic = \case
        s1 : s2 : ss -> if isExtrasyllabic s2 then (s1 ++ s2) : killNonInitialExtrasyllabic ss else s1 : killNonInitialExtrasyllabic (s2 : ss)
        ss -> ss
    syllabify tokens =
        splitOn [Space] $
            concatMap
                ( \case
                    (Sound classPrevious _, current@(Sound classCurrent _), Sound classNext _)
                        | classPrevious >= classCurrent && classCurrent < classNext -> [Space, current]
                    (_, current, _) -> [current]
                )
                $ zip3 (Space : tokens) tokens (tail tokens ++ [Space])

-- a "syllable" is extrasyllabic if it does not have a vocalic nucleus
isExtrasyllabic :: [TextToken] -> Bool
isExtrasyllabic syllable = case classes syllable of
    [] -> False
    xs -> not $ isVowel $ maximum xs
  where
    classes = mapMaybe $ \case
        Sound theClass _ -> Just theClass
        _ -> Nothing

isVowel :: Class -> Bool
isVowel = \case
    Vowel _ -> True
    _ -> False

tokenIsVowel :: TextToken -> Bool
tokenIsVowel = \case
    Sound (Vowel _) _ -> True
    _ -> False

data Diacritic
    = Ogonek
    | Dot
    | Acute
    | Grave
    | Circumflex
    | Breve
    deriving (Show, Eq)

tokenize :: Text.Text -> Maybe [TextToken]
tokenize = parseMaybe (textToken `someTill` eof)

weightPattern :: [[TextToken]] -> [Maybe Bool]
weightPattern = map deriveWeight
  where
    deriveWeight syllableSounds
        | any (hasDiacritics [Dot, Acute, Circumflex, Ogonek]) syllableSounds = Just True
        | length rhyme > 1 = Just True
        | otherwise = Just False
      where
        rhyme = dropWhile (not . tokenIsVowel) syllableSounds

stressPattern :: [[TextToken]] -> [Maybe Bool]
stressPattern =
    disambiguateStresses
        . map deriveStress
  where
    deriveStress syllableSounds
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

hasDiacritics :: [Diacritic] -> TextToken -> Bool
hasDiacritics diacritics = \case
    Sound (Vowel ds) _ -> not $ null $ intersect diacritics ds
    _ -> False

renderPattern = map $ \case
    Just False -> '-'
    Just True -> '+'
    Nothing -> '?'

evaluate :: ([[TextToken]] -> [Maybe Bool]) -> Line Text.Text -> [String]
evaluate pattern (address, line) =
   map (renderPattern . pattern . syllables . fromJust . tokenize) $ Text.words line

textToken :: Parser TextToken
textToken =
    foldr1
        (<|>)
        [ do
            vowel <- oneOf ['a', 'e', 'i', 'o', 'u', 'y']
            diacritics <-
                many $
                    foldr1 (<|>) $
                        [ Ogonek <$ oneOf ['\x328', '\x31C']
                        , Acute <$ char '\x301'
                        , Dot <$ char '\x307'
                        , Breve <$ oneOf ['\x306', '\x303']
                        , Circumflex <$ char '\x302'
                        , Grave <$ char '\x300'
                        ]
            return $ Sound (Vowel diacritics) vowel
        , Sound Lateral <$> char 'l'
        , Sound Rhotic <$> (char 'r' <* optional (char '\x302'))
        , Sound Glide <$> char 'j'
        , Sound (Affricate Unvoiced) <$> oneOf ['c', 'č']
        , Sound (Fricative Unvoiced) <$> oneOf ['s', 'š']
        , Sound (Fricative Voiced) <$> (('v' <$ char 'w') <|> oneOf ['v', 'z', 'ž'])
        , Sound (Plosive Unvoiced) <$> oneOf ['p', 't', 'k']
        , Sound (Plosive Voiced) <$> oneOf ['b', 'd', 'g']
        , Sound Nasal <$> (('n' <$ string "ň") <|> oneOf ['m', 'n'])
        , Punctuation <$ oneOf ("/()!,?.:;-" :: [Char])
        , Space <$ oneOf (" \n" :: [Char])
        ]

metaiLines :: IO [Line Text.Text]
metaiLines =
    maybe (error "lines did not parse") return
        =<< parseLines <$> Text.readFile "metai.txt"

-- metaiFullText = do
--   allLines <- metaiLines
--   map (fmap analyzeLine) allLines
--     where analyzeLine :: Line Text.Text -> _
--           analyzeLine fmap syllables . tokenize

main :: IO ()
main = do
    return ()
