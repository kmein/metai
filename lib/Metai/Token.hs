{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Metai.Token where

import Data.Function (on)
import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.List (intersect)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)

--------------------------------------------------------------------------------
-- Token type + helper functions
--------------------------------------------------------------------------------

data TextToken = Space | Punctuation | Sound SonorityClass Char | SyllableBreak
    deriving (Show, Eq)

data SonorityClass
    = Vowel [Diacritic]
    | Glide
    | Rhotic
    | Lateral
    | Nasal
    | Affricate Voice
    | Fricative Voice
    | Plosive Voice
    deriving (Show, Eq)

data Diacritic = Ogonek | Dot | Acute | Grave | Circumflex | Breve
    deriving (Show, Eq)

data Voice = Voiced | Unvoiced
    deriving (Show, Eq)

instance Ord SonorityClass where
    compare =
        compare `on` \case
            Plosive _ -> 1 :: Int
            Affricate _ -> 2
            Fricative Unvoiced -> 2
            Fricative Voiced -> 3
            Nasal -> 4
            Lateral -> 5
            Rhotic -> 5
            Glide -> 6
            Vowel _ -> 7

renderTokens :: [TextToken] -> String
renderTokens = map $ \case
    Sound _ c -> c
    Punctuation -> '/'
    SyllableBreak -> '.'
    Space -> ' '

isVowel :: SonorityClass -> Bool
isVowel = \case
    Vowel _ -> True
    _ -> False

tokenIsVowel :: TextToken -> Bool
tokenIsVowel = \case
    Sound sonority _ -> isVowel sonority
    _ -> False

hasDiacritics :: [Diacritic] -> TextToken -> Bool
hasDiacritics diacritics = \case
    Sound (Vowel ds) _ -> not $ null $ intersect diacritics ds
    _ -> False

--------------------------------------------------------------------------------
-- Tokenization
--------------------------------------------------------------------------------

tokenize :: Text.Text -> [TextToken]
tokenize = fromJust . parseMaybe (textToken `someTill` eof)

textToken :: Parsec Void Text.Text TextToken
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
        , SyllableBreak <$ char '|'
        , Space <$ oneOf (" \n" :: [Char])
        ]
