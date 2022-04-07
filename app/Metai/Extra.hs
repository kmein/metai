module Metai.Extra (debug) where

import Debug.Trace (trace)

debug :: Show a => String -> a -> a
debug message value = trace (message ++ ": " ++ show value) value
