module ASTSpec where

import AST
import Control.Monad (forM_)
import Test.Hspec

spec :: Spec
spec = do
    describe "Block" $ do
        describe "GetSymDefs" $ do
            context "when Block contains no symbol defs" $ do
                describe "getSymDefs" $ do

                    let blockWithoutSymDefs = Block []

                    it "gives an empty list" $ do
                        getSymDefs blockWithoutSymDefs `shouldBe` []

            context "when Block contains symbol defs" $ do

                    let blockWithSymDefs = Block [Let "x" (IntVal 5), Let "z" (StrVal "Test")]

                    it "gives the symbols defined" $ do
                        getSymDefs blockWithSymDefs `shouldBe` ["x", "z"]

        describe "GetSymRefs" $ do
            context "when Block contains no symbol refs" $ do
                describe "getSymRefs" $ do

                    let blockWithoutSymRefs = Block [Let "x" (IntVal 5), Let "z" (StrVal "Test")]

                    it "gives an empty list" $ do
                        getSymRefs blockWithoutSymRefs `shouldBe` []
            context "when Block contains symbol refs" $ do
                describe "getSymRefs" $ do

                    let blockWithSymRefs = Block [ Let "a" (SymVal "x")
                                                 , Let "b" (BinOp "+" (BinOp "-" (SymVal "y") (IntVal 5)) (SymVal "x"))
                                                 ]
                    let expectatiopn = ["x", "y", "x"]

                    it "gives the symbols referenced" $ do
                        getSymRefs blockWithSymRefs `shouldBe` expectatiopn

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
        describe "GetSymDefs" $ do
            describe "getSymDefs" $ do
                it "gives the symbols defined" $ do
                    getSymDefs (Let "x" (IntVal 10)) `shouldBe` ["x"]
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
