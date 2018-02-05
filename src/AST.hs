module AST where

import Alias
import Error
import Data.Coerce (coerce)
import Data.List ((\\), intersect, sort)

data Parsed
data Sorted
data Validated

data Loc = FileLoc FileName Line Column
         deriving (Eq, Show)

data Expr = IntLit Int
          | StrLit String
          | VarRef Identifier
          | BinOp  String Expr Expr
          deriving (Eq, Show)

data Stmt = Let Identifier Expr
          deriving (Eq, Show)

data Block a = Block [Stmt] deriving (Eq, Show)

class GetVarRefs a where
    getVarRefs :: a -> [Identifier]

class GetVarDefs a where
    getVarDefs :: a -> [Identifier]

instance GetVarDefs (Block a) where
    getVarDefs (Block stmts) = concat $ getVarDefs <$> stmts

instance GetVarRefs (Block a) where
    getVarRefs (Block stmts) = concat $ getVarRefs <$> stmts

instance GetVarDefs Stmt where
    getVarDefs (Let var _) = [var]

instance GetVarRefs Expr where
    getVarRefs (VarRef s) = [s]
    getVarRefs (BinOp _ l r) = getVarRefs l ++ getVarRefs r
    getVarRefs _ = []

instance GetVarRefs Stmt where
    getVarRefs (Let _ expr) = getVarRefs expr

sortByVarDef :: (GetVarDefs a, GetVarRefs a) => a -> a -> Ordering
sortByVarDef x y =
    let i = getVarDefs x `intersect` getVarRefs y
     in if length i > 0
        then LT
        else GT

-------------------------------------------------------------------
-------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------
-------------------------------------------------------------------

validate :: Block Sorted -> Either Error (Block Validated)
validate b = coerce $ validateAllVarsDefined b >>= validateVarsDefinedOnce

validateAllVarsDefined :: Block Sorted -> Either Error (Block Sorted)
validateAllVarsDefined block =
    let varDefs = getVarDefs block
        varRefs = getVarRefs block
        undefs  = varRefs \\ varDefs
     in case undefs of
            [] -> Right block
            _  -> Left $ VarUndefinedError $ head undefs

validateVarsDefinedOnce :: Block Sorted -> Either Error (Block Sorted)
validateVarsDefinedOnce block@(Block stmts) =
    case go [] stmts of
        Just s  -> Left $ VarRedefinitionError s
        Nothing -> Right block
    where go :: [Identifier] -> [Stmt] -> Maybe Identifier
          go _ [] = Nothing
          go preDefs (s:ss) =
              let stmtDefs = getVarDefs s
                  dupDefs = intersect preDefs stmtDefs
               in if dupDefs == []
                  then go (preDefs ++ stmtDefs) ss
                  else Just $ head dupDefs
