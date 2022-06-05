{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Metai.Token where

import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)

--------------------------------------------------------------------------------
-- Token type + helper functions
--------------------------------------------------------------------------------

data TextToken = Space | Punctuation | Sound Phonology Char | SyllableBreak
    deriving (Show, Eq, Ord)

data Phonology = Sibilant | Plosive | Resonant | Vowel [Diacritic]
  deriving (Show, Eq, Ord)

data Diacritic = Ogonek | Dot | Acute | Grave | Circumflex | Breve
    deriving (Show, Eq, Ord)

instance VisualStream [TextToken] where
  showTokens _ (t :| ts) = renderTokens (t:ts)

instance TraversableStream [TextToken] where
  -- https://hackage.haskell.org/package/megaparsec-9.2.1/docs/src/Text.Megaparsec.Stream.html#reachOffsetNoLine%27
  reachOffsetNoLine o PosState {..} =
      ( PosState
          { pstateInput = post,
            pstateOffset = max pstateOffset o,
            pstateSourcePos = spos,
            pstateTabWidth = pstateTabWidth,
            pstateLinePrefix = pstateLinePrefix
          }
      )
      where
        spos = case pstateSourcePos of
          (SourcePos n l c) -> SourcePos n l (c <> pos1)
        post = drop (o - pstateOffset) pstateInput

renderTokens :: [TextToken] -> String
renderTokens = map $ \case
    Sound _ c -> c
    Punctuation -> '/'
    SyllableBreak -> '.'
    Space -> ' '

isVowel :: Phonology -> Bool
isVowel = \case
    Vowel _ -> True
    _ -> False

tokenIsVowel :: TextToken -> Bool
tokenIsVowel = \case
    Sound phonology _ -> isVowel phonology
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
        , Sound Plosive <$> (try ('ǳ' <$ string "dz") <|> ('ʤ' <$ string "dž") <|> ('ʦ' <$ char 'c') <|> ('ʧ' <$ char 'č') <|> oneOf ['p', 't', 'k', 'b', 'd', 'g'])
        , Sound Sibilant <$> (oneOf ['s', 'z'] <|> ('ʒ' <$ char 'ž') <|> ('ʃ' <$ char 'š'))
        , Sound Resonant <$> (('n' <$ string "ň") <|> oneOf ['m', 'n', 'j', 'l', 'v'] <|> ('v' <$ char 'w') <|> (char 'r' <* optional (char '\x302')))
        , Punctuation <$ oneOf ("/()!,?.:;-" :: [Char])
        , SyllableBreak <$ char '|'
        , Space <$ oneOf (" \n" :: [Char])
        ]
