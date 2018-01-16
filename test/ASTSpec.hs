module ASTSpec where

import AST
import Control.Monad (forM_)
import Test.Hspec

spec :: Spec
spec = do
    describe "Expr" $ do
        describe "GetSymRefs" $ do
            context "when Expr contains no symbol refs" $ do
                describe "getSymRefs" $ do

                    let exprsWithoutSymRefs = [IntVal 5, StrVal "Test", BinOp "+" (IntVal 5) (IntVal 6)]

                    it "gives an empty list" $ forM_  exprsWithoutSymRefs $
                        \expr -> getSymRefs expr `shouldBe` []
            context "when Expr contains symbol refs" $ do
                describe "getSymRefs" $ do

                    let exprsWithSymRefs = [(SymVal "x", ["x"]), ((BinOp "+" (BinOp "-" (SymVal "y") (IntVal 5)) (SymVal "x")), ["y", "x"])]

                    it "gives the symbols referenced" $ forM_ exprsWithSymRefs $
                        \(expr, symRefs) -> getSymRefs expr `shouldBe` symRefs

    describe "Stmt" $ do
        describe "GetSymDef" $ do
            describe "getSymDef" $ do
                it "gives the symbol defined" $ do
                    getSymDef (Let "x" (IntVal 10)) `shouldBe` "x"
        describe "GetSymRefs" $ do
            context "when Stmt contains no symbol refs" $ do
                describe "getSymRefs" $ do

                    let stmtsWithoutSymRefs = [Let "x" (IntVal 5)]

                    it "gives an empty list" $ forM_  stmtsWithoutSymRefs $
                        \expr -> getSymRefs expr `shouldBe` []
            context "when Stmt contains symbol refs" $ do
                describe "getSymRefs" $ do

                    let stmtWithSymRefs = [(Let "x" (SymVal "y"), ["y"])]

                    it "gives the symbols referenced" $ forM_ stmtWithSymRefs $
                        \(expr, symRefs) -> getSymRefs expr `shouldBe` symRefs

    describe "sortBySymDef" $ do
        it "sorts statements that reference symbols after statemtns that define them" $ do
            let testStmtList = [Let "y" (SymVal "x"), Let "z" (BinOp "+" (SymVal "x") (SymVal "y")), Let "x" (IntVal 5)]
                expectedList = [Let "x" (IntVal 5), Let "y" (SymVal "x"), Let "z" (BinOp "+" (SymVal "x") (SymVal "y"))]
             in sortBySymDef testStmtList `shouldBe` expectedList
