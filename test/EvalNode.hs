module EvalNode
       where
import LangData
import LangProc

import Control.Monad.Trans.State
import Data.Tree
import qualified Data.Map.Strict as StrMap

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

exprAdd :: Expression
exprAdd = Node{rootLabel=Left Add, subForest=[
           Node{rootLabel=Right $ DatI 5, subForest=[]},
           Node{rootLabel=Right $ DatI 3, subForest=[]}]}
testAdd =
  let res = evalState (evalNode exprAdd) (StrMap.empty, StrMap.empty)
  in "simple addition" ~: DatI 8 ~=? res

exprSubt :: Expression
exprSubt = Node{rootLabel=Left Subt, subForest=[
           Node{rootLabel=Right $ DatD 42.42, subForest=[]},
           Node{rootLabel=Right $ DatD 84.42, subForest=[]}]}
testSubt =
  let res = evalState (evalNode exprSubt) (StrMap.empty, StrMap.empty)
  in "simple subtraction" ~: DatD (-42.0) ~=? res

exprMul :: Expression
exprMul = Node{rootLabel=Left Mul, subForest=[
           Node{rootLabel=Right $ DatI 5, subForest=[]},
           Node{rootLabel=Right $ DatI 3, subForest=[]}]}
testMul =
  let res = evalState (evalNode exprMul) (StrMap.empty, StrMap.empty)
  in "simple multiplication" ~: DatI 15 ~=? res

exprDiv :: Expression
exprDiv = Node{rootLabel=Left Div, subForest=[
           Node{rootLabel=Right $ DatI 16, subForest=[]},
           Node{rootLabel=Right $ DatI 3, subForest=[]}]}
testDiv =
  let res = evalState (evalNode exprDiv) (StrMap.empty, StrMap.empty)
  in "simple division" ~: DatI 5 ~=? res

exprGt :: Expression
exprGt = Node{rootLabel=Left Gt, subForest=[
           Node{rootLabel=Right $ DatI 16, subForest=[]},
           Node{rootLabel=Right $ DatI 3, subForest=[]}]}
testGt =
  let res = evalState (evalNode exprGt) (StrMap.empty, StrMap.empty)
  in "simple greater than" ~: DatB True ~=? res

exprLt :: Expression
exprLt = Node{rootLabel=Left Lt, subForest=[
           Node{rootLabel=Right $ DatI 16, subForest=[]},
           Node{rootLabel=Right $ DatI 3, subForest=[]}]}
testLt =
  let res = evalState (evalNode exprLt) (StrMap.empty, StrMap.empty)
  in "simple less than" ~: DatB False ~=? res

exprEq :: Expression
exprEq = Node{rootLabel=Left Eq, subForest=[
           Node{rootLabel=Right $ ListDat DatIVar [DatI 16,DatI 42], subForest=[]},
           Node{rootLabel=Right $ ListDat DatIVar [DatI 16,DatI 42], subForest=[]}]}
testEq =
  let res = evalState (evalNode exprEq) (StrMap.empty, StrMap.empty)
  in "simple equality" ~: DatB True ~=? res

exprAnd :: Expression
exprAnd = Node{rootLabel=Left And, subForest=[
           Node{rootLabel=Right $ DatB True, subForest=[]},
           Node{rootLabel=Right $ DatB False, subForest=[]}]}
testAnd =
  let res = evalState (evalNode exprAnd) (StrMap.empty, StrMap.empty)
  in "simple AND" ~: DatB False ~=? res

exprOr :: Expression
exprOr = Node{rootLabel=Left Or, subForest=[
           Node{rootLabel=Right $ DatB True, subForest=[]},
           Node{rootLabel=Right $ DatB False, subForest=[]}]}
testOr =
  let res = evalState (evalNode exprOr) (StrMap.empty, StrMap.empty)
  in "simple OR" ~: DatB True ~=? res

exprNot :: Expression
exprNot = Node{rootLabel=Left Not, subForest=[
           Node{rootLabel=Right $ DatB False, subForest=[]}]}
testNot =
  let res = evalState (evalNode exprNot) (StrMap.empty, StrMap.empty)
  in "simple NOT" ~: DatB True ~=? res

exprIf :: Expression
exprIf = Node{rootLabel=Left If, subForest=[
           Node{rootLabel=Right $ DatB True, subForest=[]},
           Node{rootLabel=Right $ DatI 16, subForest=[]},
           Node{rootLabel=Right $ DatI 3, subForest=[]}]}
testIf =
  let res = evalState (evalNode exprIf) (StrMap.empty, StrMap.empty)
  in "simple If" ~: DatI 16 ~=? res

exprDfn :: Expression
exprDfn = Node{rootLabel=Left Dfn, subForest=[
           Node{rootLabel=Left If, subForest=[
            Node{rootLabel=Right $ FuncArg 0, subForest=[]},
            Node{rootLabel=Right $ DatI 16, subForest=[]},
            Node{rootLabel=Right $ FuncArg 1, subForest=[]}]}]}
exprCall :: Expression
exprCall = Node{rootLabel=Left Call, subForest=[
            Node{rootLabel=Right $ FuncId 0, subForest=[]},
            Node{rootLabel=Right $ DatB False, subForest=[]},
            Node{rootLabel=Right $ DatI 21, subForest=[]}]}
testCall =
  let (fTable,_) = execState (evalNode exprDfn) (StrMap.empty, StrMap.empty)
      res = evalState (evalNode exprCall) (fTable, StrMap.empty)
  in "simple Call" ~: DatI 21 ~=? res

exprMapFunc :: Expression
exprMapFunc = Node{rootLabel=Left Dfn, subForest=[
               Node{rootLabel=Left If, subForest=[
                Node{rootLabel=Left Gt, subForest=[
                 Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                 Node{rootLabel=Right $ DatI 10, subForest=[]}]},
                Node{rootLabel=Left ToDouble, subForest=[
                 Node{rootLabel=Right $ FuncArg 0, subForest=[]}]},
                Node{rootLabel=Right $ DatD 0.0, subForest=[]}]}]}
exprMap :: Expression
exprMap = Node{rootLabel=Left Map, subForest=[
           Node{rootLabel=Right $ FuncId 0, subForest=[]},
           Node{rootLabel=Right $ ListDat DatDVar [DatI 42, DatI 15, DatI 5, DatI 9], subForest=[]}]}
testMap =
  let (fTable,_) = execState (evalNode exprMapFunc) (StrMap.empty, StrMap.empty)
      res = evalState (evalNode exprMap) (fTable, StrMap.empty)
  in "simple Map" ~: ListDat DatDVar [DatD 42.0, DatD 15.0, DatD 0.0, DatD 0.0] ~=? res
exprEmptyMap :: Expression
exprEmptyMap = Node{rootLabel=Left Map, subForest=[
                Node{rootLabel=Right $ FuncId 0, subForest=[]},
                Node{rootLabel=Right $ ListDat DatDVar [], subForest=[]}]}
testEmptyMap =
  let (fTable,_) = execState (evalNode exprMapFunc) (StrMap.empty, StrMap.empty)
      res = evalState (evalNode exprEmptyMap) (fTable, StrMap.empty)
  in "simple Map" ~: ListDat DatDVar [] ~=? res

exprFoldFunc :: Expression
exprFoldFunc = Node{rootLabel=Left Dfn, subForest=[
                Node{rootLabel=Left If, subForest=[
                 Node{rootLabel=Left Gt, subForest=[
                  Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                  Node{rootLabel=Right $ DatI 10, subForest=[]}]},
                 Node{rootLabel=Left Add, subForest=[
                  Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                  Node{rootLabel=Right $ FuncArg 1, subForest=[]}]},
                 Node{rootLabel=Right $ FuncArg 1, subForest=[]}]}]}
exprFold :: Expression
exprFold = Node{rootLabel=Left Fold, subForest=[
            Node{rootLabel=Right $ FuncId 0, subForest=[]},
            Node{rootLabel=Right $ DatI 2, subForest=[]},
            Node{rootLabel=Right $ ListDat DatIVar [DatI 42, DatI 15, DatI 5, DatI 9], subForest=[]}]}
testFold =
  let (fTable,_) = execState (evalNode exprFoldFunc) (StrMap.empty, StrMap.empty)
      res = evalState (evalNode exprFold) (fTable, StrMap.empty)
  in "simple Fold" ~: DatI 59 ~=? res

exprMkList :: Expression
exprMkList = Node{rootLabel=Left MkList, subForest=[
              Node{rootLabel=Right $ DatI 5, subForest=[]}]}
testMkList =
  let res = evalState (evalNode exprMkList) (StrMap.empty, StrMap.empty)
  in "simple MkList" ~: ListDat DatIVar [DatI 5] ~=? res

exprConsList :: Expression
exprConsList = Node{rootLabel=Left ConsList, subForest=[
                Node{rootLabel=Right $ DatB True, subForest=[]},
                Node{rootLabel=Right $ ListDat DatBVar [DatB False,DatB False], subForest=[]}]}
testConsList =
  let res = evalState (evalNode exprConsList) (StrMap.empty, StrMap.empty)
  in "simple ConsList" ~: ListDat DatBVar [DatB True,DatB False,DatB False] ~=? res

exprAppLists :: Expression
exprAppLists = Node{rootLabel=Left AppLists, subForest=[
                Node{rootLabel=Right $ ListDat DatBVar [DatB True,DatB True], subForest=[]},
                Node{rootLabel=Right $ ListDat DatBVar [DatB False,DatB False], subForest=[]}]}
testAppLists =
  let res = evalState (evalNode exprAppLists) (StrMap.empty, StrMap.empty)
  in "simple AppLists" ~: ListDat DatBVar [DatB True,DatB True,DatB False,DatB False] ~=? res

exprCeil :: Expression
exprCeil = Node{rootLabel=Left Ceil, subForest=[
           Node{rootLabel=Right $ DatD 5.6, subForest=[]}]}
testCeil =
  let res = evalState (evalNode exprCeil) (StrMap.empty, StrMap.empty)
  in "simple Ceil" ~: DatI 6 ~=? res

exprFloor :: Expression
exprFloor = Node{rootLabel=Left Floor, subForest=[
           Node{rootLabel=Right $ DatD 5.6, subForest=[]}]}
testFloor =
  let res = evalState (evalNode exprFloor) (StrMap.empty, StrMap.empty)
  in "simple Floor" ~: DatI 5 ~=? res

exprToDouble :: Expression
exprToDouble = Node{rootLabel=Left ToDouble, subForest=[
           Node{rootLabel=Right $ DatI 5, subForest=[]}]}
testToDouble =
  let res = evalState (evalNode exprToDouble) (StrMap.empty, StrMap.empty)
  in "simple ToDouble" ~: DatD 5.0 ~=? res

evalNodeTests = TestList [testAdd
                         ,testSubt
                         ,testMul
                         ,testDiv
                         ,testLt
                         ,testGt
                         ,testEq
                         ,testAnd
                         ,testOr
                         ,testNot
                         ,testIf
                         ,testCall
                         ,testMap
                         ,testEmptyMap 
                         ,testFold
                         ,testMkList
                         ,testConsList
                         ,testAppLists
                         ,testCeil
                         ,testFloor
                         ,testToDouble]
