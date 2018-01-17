module Error where

import Alias

data Error = UndefinedSymbolError Symbol
           deriving (Eq, Show)
