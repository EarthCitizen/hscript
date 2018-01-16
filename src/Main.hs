module Main where

import AST
import Control.Monad (join)

l1 = Let "x" $ BinOp "+" (SymVal "y") (SymVal "z")
l2 = Let "z" $ BinOp "-" (SymVal "a") (IntVal 5)
l3 = Let "a" $ IntVal 7
l4 = Let "x" $ BinOp "+" (SymVal "Q_") (SymVal "P_")

main :: IO ()
main = do
  let stmts = [l1, l2, l3, l4]
  return ()
  -- let allSymDefs = getSymDef <$> stmts
  -- print allSymDefs
  -- print $ findUndefSyms allSymDefs stmts
