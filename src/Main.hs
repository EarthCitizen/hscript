module Main where

import AST
import Control.Monad (join)

l1 = Let "x" $ BinOp "+" (VarRef "y") (VarRef "z")
l2 = Let "z" $ BinOp "-" (VarRef "a") (IntLit 5)
l3 = Let "a" $ IntLit 7
l4 = Let "x" $ BinOp "+" (VarRef "Q_") (VarRef "P_")

main :: IO ()
main = do
  let stmts = [l1, l2, l3, l4]
  return ()
  -- let allVarDefs = getVarDef <$> stmts
  -- print allVarDefs
  -- print $ findUndefVars allVarDefs stmts
