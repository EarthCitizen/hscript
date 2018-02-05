module Error where

import Alias

data Error = VarRedefinitionError Identifier
           | VarUndefinedError    Identifier
           deriving (Eq, Show)
