module AST where

import Data.List (sort)

type Column   = Integer
type Line     = Integer
type FileName = String

data Loc = FileLoc FileName Line Colum
         deriving (Eq, Show)

type Symbol = String

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

instance GetSymRefs Expr where
    getSymRefs (SymVal s) = [s]
    getSymRefs (BinOp _ l r) = getSymRefs l ++ getSymRefs r
    getSymRefs _ = []

instance GetSymRefs Stmt where
    getSymRefs (Let _ expr) = getSymRefs expr

instance GetSymDef Stmt where
    getSymDef (Let sym _) = sym

splitSymRefs :: Stmt -> ([Symbol], Stmt)
splitSymRefs stmt = (getSymRefs stmt, stmt)

data SplitSyms = SplitSyms
   { getSplSymDef  :: Symbol
   , getSplSymRefs :: [Symbol]
   , getSlpStmt    :: Stmt
   } deriving (Eq, Show)

instance Ord SplitSyms where
    (<=) x@(SplitSyms def _ _) y@(SplitSyms _ refs _) =
        elem def refs

splitSyms :: Stmt -> SplitSyms
splitSyms stmt = SplitSyms (getSymDef stmt) (getSymRefs stmt) stmt

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


-- findUndefSyms :: [Symbol] -> [Stmt] -> [([Symbol], Stmt)]
-- findUndefSyms allSymDefs stmts =
--     let splSymRefs = splitSymRefs <$> stmts
--         isUndefSym = (\s -> notElem s allSymDefs)
--         slpWthUndef = filter (\spl -> any isUndefSym $ fst spl) splSymRefs
--         onlyUndefSym = (\(syms, sts) -> (filter isUndefSym syms, sts)) <$> slpWthUndef
--      in filter (\spl -> fst spl /= []) onlyUndefSym
