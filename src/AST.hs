module AST where

import Alias
import Error
-- import Data.Either.Validation (Validation(..))
import Data.List (intersect, sort)

data Loc = FileLoc FileName Line Column
         deriving (Eq, Show)

data Expr = IntVal Int
          | StrVal String
          | SymVal Symbol
          | BinOp String Expr Expr
          deriving (Eq, Show)

data Stmt = Let Symbol Expr
          deriving (Eq, Show)

data Block = Block [Stmt] deriving (Eq, Show)

class GetSymRefs a where
    getSymRefs :: a -> [Symbol]

class GetSymDef a where
    getSymDef :: a -> Symbol

class GetSymDefs a where
    getSymDefs :: a -> [Symbol]

instance GetSymDefs Block where
    getSymDefs (Block stmts) = concat $ getSymDefs <$> stmts

instance GetSymRefs Block where
    getSymRefs (Block stmts) = concat $ getSymRefs <$> stmts

instance GetSymDefs Stmt where
    getSymDefs (Let sym _) = [sym]

instance GetSymRefs Expr where
    getSymRefs (SymVal s) = [s]
    getSymRefs (BinOp _ l r) = getSymRefs l ++ getSymRefs r
    getSymRefs _ = []

instance GetSymRefs Stmt where
    getSymRefs (Let _ expr) = getSymRefs expr

splitSymRefs :: Stmt -> ([Symbol], Stmt)
splitSymRefs stmt = (getSymRefs stmt, stmt)

data SplitSyms = SplitSyms
   { getSplSymDefs :: [Symbol]
   , getSplSymRefs :: [Symbol]
   , getSlpStmt    :: Stmt
   } deriving (Eq, Show)

instance Ord SplitSyms where
    (<=) (SplitSyms defs _ _) (SplitSyms _ refs _) =
        let i = defs `intersect` refs
         in length i > 0

splitSyms :: Stmt -> SplitSyms
splitSyms stmt = SplitSyms (getSymDefs stmt) (getSymRefs stmt) stmt

sortBySymDef :: [Stmt] -> [Stmt]
sortBySymDef stmts =
    let splSyms = splitSyms <$> stmts
        sorted  = sort splSyms
     in getSlpStmt <$> sorted

-------------------------------------------------------------------
-------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------
-------------------------------------------------------------------

validateAllSymbolsDefined :: Block -> Either Error Block
validateAllSymbolsDefined block = undefined

-- findUndefSyms :: [Symbol] -> [Stmt] -> [([Symbol], Stmt)]
-- findUndefSyms allSymDefs stmts =
--     let splSymRefs = splitSymRefs <$> stmts
--         isUndefSym = (\s -> notElem s allSymDefs)
--         slpWthUndef = filter (\spl -> any isUndefSym $ fst spl) splSymRefs
--         onlyUndefSym = (\(syms, sts) -> (filter isUndefSym syms, sts)) <$> slpWthUndef
--      in filter (\spl -> fst spl /= []) onlyUndefSym
