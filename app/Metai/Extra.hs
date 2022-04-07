module Metai.Extra (Parser, debug) where

import Text.Megaparsec (Parsec)
import Data.Void (Void)
import Data.Text (Text)
import Debug.Trace (trace)

type Parser = Parsec Void Text

debug :: Show a => String -> a -> a
debug message value = trace (message ++ ": " ++ show value) value
