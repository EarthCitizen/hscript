module ASTSpec where

import AST
import Error
import Control.Monad (forM_)
import Data.List (sortBy)
import Test.Hspec

spec :: Spec
spec = do
    describe "Block" $ do
        describe "GetVarDefs" $ do
            context "when Block contains no Identifier defs" $ do
                describe "getVarDefs" $ do

                    let blockWithoutVarDefs = Block []

                    it "gives an empty list" $ do
                        getVarDefs blockWithoutVarDefs `shouldBe` []

            context "when Block contains Identifier defs" $ do

                    let blockWithVarDefs = Block [Let "x" (IntLit 5), Let "z" (StrLit "Test")]

                    it "gives the Identifiers defined" $ do
                        getVarDefs blockWithVarDefs `shouldBe` ["x", "z"]

        describe "GetVarRefs" $ do
            context "when Block contains no Identifier refs" $ do
                describe "getVarRefs" $ do

                    let blockWithoutVarRefs = Block [Let "x" (IntLit 5), Let "z" (StrLit "Test")]

                    it "gives an empty list" $ do
                        getVarRefs blockWithoutVarRefs `shouldBe` []
            context "when Block contains Identifier refs" $ do
                describe "getVarRefs" $ do

                    let blockWithVarRefs = Block [ Let "a" (VarRef "x")
                                                 , Let "b" (BinOp "+" (BinOp "-" (VarRef "y") (IntLit 5)) (VarRef "x"))
                                                 ]
                    let expectatiopn = ["x", "y", "x"]

                    it "gives the Identifiers referenced" $ do
                        getVarRefs blockWithVarRefs `shouldBe` expectatiopn

    describe "Expr" $ do
        describe "GetVarRefs" $ do
            context "when Expr contains no Identifier refs" $ do
                describe "getVarRefs" $ do

                    let exprsWithoutVarRefs = [IntLit 5, StrLit "Test", BinOp "+" (IntLit 5) (IntLit 6)]

                    it "gives an empty list" $ forM_  exprsWithoutVarRefs $
                        \expr -> getVarRefs expr `shouldBe` []
            context "when Expr contains Identifier refs" $ do
                describe "getVarRefs" $ do

                    let exprsWithVarRefs = [(VarRef "x", ["x"]), ((BinOp "+" (BinOp "-" (VarRef "y") (IntLit 5)) (VarRef "x")), ["y", "x"])]

                    it "gives the Identifiers referenced" $ forM_ exprsWithVarRefs $
                        \(expr, varRefs) -> getVarRefs expr `shouldBe` varRefs

    describe "Stmt" $ do
        describe "GetVarDefs" $ do
            describe "getVarDefs" $ do
                it "gives the Identifiers defined" $ do
                    getVarDefs (Let "x" (IntLit 10)) `shouldBe` ["x"]
        describe "GetVarRefs" $ do
            context "when Stmt contains no Identifier refs" $ do
                describe "getVarRefs" $ do

                    let stmtsWithoutVarRefs = [Let "x" (IntLit 5)]

                    it "gives an empty list" $ forM_  stmtsWithoutVarRefs $
                        \expr -> getVarRefs expr `shouldBe` []
            context "when Stmt contains Identifier refs" $ do
                describe "getVarRefs" $ do

                    let stmtWithVarRefs = [(Let "x" (VarRef "y"), ["y"])]

                    it "gives the Identifiers referenced" $ forM_ stmtWithVarRefs $
                        \(expr, varRefs) -> getVarRefs expr `shouldBe` varRefs

    describe "sortByVarDef" $ do
        it "sorts statements that reference Identifiers after statemtns that define them" $ do
            let testStmtList = [Let "y" (VarRef "x"), Let "z" (BinOp "+" (VarRef "x") (VarRef "y")), Let "x" (IntLit 5)]
                expectedList = [Let "x" (IntLit 5), Let "y" (VarRef "x"), Let "z" (BinOp "+" (VarRef "x") (VarRef "y"))]
                sortedList = sortBy sortByVarDef testStmtList
             in sortedList `shouldBe` expectedList

    describe "validateAllVarsDefined" $ do
        context "when all Identifiers defined" $ do

            let blockAllVarsDefined = Block $ [Let "x" (IntLit 5), Let "y" (VarRef "x")]

            it "gives the same block from parameter" $ do
                validateAllVarsDefined blockAllVarsDefined `shouldBe` Right blockAllVarsDefined

        context "when not all Identifiers defined" $ do

            let blockNotAllVarsDefined = Block $ [Let "x" (IntLit 5), Let "y" (VarRef "z")]

            it "gives error with undefined Identifier" $ do
                validateAllVarsDefined blockNotAllVarsDefined `shouldBe` Left (VarUndefinedError $ "z")

    describe "validateVarsDefinedOnce" $ do
        context "when all Identifiers defined once" $ do

            let blockVarsDefOnce = Block $ [Let "x" (IntLit 5), Let "y" (VarRef "x")]

            it "gives the same block from parameter" $ do
                validateVarsDefinedOnce blockVarsDefOnce `shouldBe` Right blockVarsDefOnce

        context "when var defined more than once in scope" $ do

            let blockVarMoreThanOnce = Block $ [Let "x" (IntLit 5), Let "x" (IntLit 10)]

            it "gives error with duplicate Identifier" $ do
                validateVarsDefinedOnce blockVarMoreThanOnce `shouldBe` Left (VarRedefinitionError "x")
