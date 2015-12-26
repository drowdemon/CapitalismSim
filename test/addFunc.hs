module Main
       where
import Data.Tree
import LangData
import EvalNode
import Control.Monad.Trans.State
import qualified Data.Map.Strict as StrMap
import Test.HUnit

testExpr1 :: Expression
testExpr1 = Node{rootLabel=Left Dfn, subForest=
                [Node{rootLabel=Left Add, subForest=[
                  Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                  Node{rootLabel=Right $ FuncArg 1, subForest=[]}]}]}
addFuncTest1 :: Test
addFuncTest1 =
  let (res,_) = execState (evalNode testExpr1) (StrMap.empty, StrMap.empty)
  in res ~?= (StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 1), subForest = []}]}, retType = GenType (NumVar,0), argType = StrMap.fromList [(0,GenType (NumVar,0)),(1,GenType (NumVar,0))]})])

testExpr2 :: Expression
testExpr2 = Node{rootLabel=Left Dfn, subForest=
                [Node{rootLabel=Left Add, subForest=[
                  Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                  Node{rootLabel=Left Subt, subForest=[
                   Node{rootLabel=Right $ FuncArg 1, subForest=[]},
                   Node{rootLabel=Right $ DatI 0, subForest=[]}]}]}]}
addFuncTest2 :: Test
addFuncTest2 =
  let (res,_) = execState (evalNode testExpr2) (StrMap.empty, StrMap.empty)
  in res ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Left Subt, subForest = [Node {rootLabel = Right (FuncArg 1), subForest = []},Node {rootLabel = Right (DatI 0), subForest = []}]}]}, retType = SpecType DatIVar, argType = StrMap.fromList [(0,SpecType DatIVar),(1,SpecType DatIVar)]})]

testExpr3 :: Expression
testExpr3 = Node{rootLabel=Left Dfn, subForest=
                [Node{rootLabel=Left If, subForest=[
                  Node{rootLabel=Left Gt, subForest=[
                   Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                   Node{rootLabel=Right $ FuncArg 1, subForest=[]}]},
                  Node{rootLabel=Left Mul, subForest=[
                   Node{rootLabel=Right $ FuncArg 2, subForest=[]},
                   Node{rootLabel=Right $ FuncArg 3, subForest=[]}]},
                  Node{rootLabel=Left Div, subForest=[
                   Node{rootLabel=Right $ FuncArg 3, subForest=[]},
                   Node{rootLabel=Right $ FuncArg 3, subForest=[]}]}]}]}
addFuncTest3 :: Test
addFuncTest3 =
  let (res, _) = execState (evalNode testExpr3) (StrMap.empty, StrMap.empty)
  in res ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left If, subForest = [Node {rootLabel = Left Gt, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 1), subForest = []}]},Node {rootLabel = Left Mul, subForest = [Node {rootLabel = Right (FuncArg 2), subForest = []},Node {rootLabel = Right (FuncArg 3), subForest = []}]},Node {rootLabel = Left Div, subForest = [Node {rootLabel = Right (FuncArg 3), subForest = []},Node {rootLabel = Right (FuncArg 3), subForest = []}]}]}, retType = GenType (NumVar,1), argType = StrMap.fromList [(0,GenType (NumVar,0)),(1,GenType (NumVar,0)),(2,GenType (NumVar,1)),(3,GenType (NumVar,1))]})]
     
testExpr4 :: Expression
testExpr4 = Node{rootLabel=Left Dfn, subForest=[
             Node{rootLabel=Left MkList, subForest=[
               Node{rootLabel=Left Add, subForest=[
                Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                Node{rootLabel=Right $ FuncArg 1, subForest=[]}]}]}]}
addFuncTest4 :: Test
addFuncTest4 =
  let (res, _) = execState (evalNode testExpr4) (StrMap.empty, StrMap.empty)
  in res ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left MkList, subForest = [Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 1), subForest = []}]}]}, retType = ListType (GenType (NumVar,0)), argType = StrMap.fromList [(0,GenType (NumVar,0)),(1,GenType (NumVar,0))]})]

testExpr5 :: Expression
testExpr5 = Node{rootLabel=Left Dfn, subForest=[
                 Node{rootLabel=Left AppLists, subForest=[
                  Node{rootLabel=Left ConsList, subForest=[
                   Node{rootLabel=Right $ FuncArg 2, subForest=[]},
                   Node{rootLabel=Left MkList, subForest=[
                    Node{rootLabel=Left Add, subForest=[
                     Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                     Node{rootLabel=Right $ FuncArg 1, subForest=[]}]}]}]},
                  Node{rootLabel=Left MkList, subForest=[
                   Node{rootLabel=Right $ FuncArg 3, subForest=[]}]}]}]}
addFuncTest5 :: Test
addFuncTest5 =
  let (res, _) = execState (evalNode testExpr5) (StrMap.empty, StrMap.empty)
  in res ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left AppLists, subForest = [Node {rootLabel = Left ConsList, subForest = [Node {rootLabel = Right (FuncArg 2), subForest = []},Node {rootLabel = Left MkList, subForest = [Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 1), subForest = []}]}]}]},Node {rootLabel = Left MkList, subForest = [Node {rootLabel = Right (FuncArg 3), subForest = []}]}]}, retType = ListType (GenType (NumVar,0)), argType = StrMap.fromList [(0,GenType (NumVar,0)),(1,GenType (NumVar,0)),(2,GenType (NumVar,0)),(3,GenType (NumVar,0))]})]

testExpr6 :: Expression
testExpr6 = Node{rootLabel=Left Dfn, subForest=[
               Node{rootLabel=Left Call, subForest=[
                Node{rootLabel=Right $ FuncId 0, subForest=[]},
                Node{rootLabel=Left Add, subForest=[
                 Node{rootLabel=Right $ FuncArg 1, subForest=[]},
                 Node{rootLabel=Right $ FuncArg 2, subForest=[]}]},
                Node{rootLabel=Right $ FuncArg 0, subForest=[]}]}]}
addFuncTest6 :: Test
addFuncTest6 =
  let (funcDesc1, _) = execState (evalNode testExpr1) (StrMap.empty, StrMap.empty)
      (funcDesc2, _) = execState (evalNode testExpr6) (funcDesc1, StrMap.empty)
  in funcDesc2 ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 1), subForest = []}]}, retType = GenType (NumVar,0), argType = StrMap.fromList [(0,GenType (NumVar,0)),(1,GenType (NumVar,0))]}),(1,FuncDesc {body = Node {rootLabel = Left Call, subForest = [Node {rootLabel = Right (FuncId 0), subForest = []},Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (FuncArg 1), subForest = []},Node {rootLabel = Right (FuncArg 2), subForest = []}]},Node {rootLabel = Right (FuncArg 0), subForest = []}]}, retType = GenType (NumVar,0), argType = StrMap.fromList [(0,GenType (NumVar,0)),(1,GenType (NumVar,0)),(2,GenType (NumVar,0))]})]

testExpr7 :: Expression
testExpr7 = Node{rootLabel=Left Dfn, subForest=[
              Node{rootLabel=Left ConsList, subForest=[
               Node{rootLabel=Right $ FuncArg 1, subForest=[]},
               Node{rootLabel=Left Call, subForest=[
                Node{rootLabel=Right $ FuncId 0, subForest=[]},
                Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                Node{rootLabel=Right $ FuncArg 2, subForest=[]}]}]}]}
addFuncTest7 :: Test
addFuncTest7 =
  let (funcDesc1, _) = execState (evalNode testExpr4) (StrMap.empty, StrMap.empty)
      (funcDesc2, _) = execState (evalNode testExpr7) (funcDesc1, StrMap.empty)
  in funcDesc2 ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left MkList, subForest = [Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 1), subForest = []}]}]}, retType = ListType (GenType (NumVar,0)), argType = StrMap.fromList [(0,GenType (NumVar,0)),(1,GenType (NumVar,0))]}),(1,FuncDesc {body = Node {rootLabel = Left ConsList, subForest = [Node {rootLabel = Right (FuncArg 1), subForest = []},Node {rootLabel = Left Call, subForest = [Node {rootLabel = Right (FuncId 0), subForest = []},Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 2), subForest = []}]}]}, retType = ListType (GenType (NumVar,0)), argType = StrMap.fromList [(0,GenType (NumVar,0)),(1,GenType (NumVar,0)),(2,GenType (NumVar,0))]})]

testExpr8 :: Expression
testExpr8 = Node{rootLabel=Left Dfn, subForest=[
             Node{rootLabel=Left AppLists, subForest=[
              Node{rootLabel=Right $ FuncArg 0, subForest=[]},
              Node{rootLabel=Right $ FuncArg 0, subForest=[]}]}]}
addFuncTest8 :: Test
addFuncTest8 =
  let (funcDesc, _) = execState (evalNode testExpr8) (StrMap.empty, StrMap.empty)
  in funcDesc ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left AppLists, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 0), subForest = []}]}, retType = ListType (GenType (GenVar,1)), argType = StrMap.fromList [(0,ListType (GenType (GenVar,1)))]})]

testExpr9 :: Expression
testExpr9 = Node{rootLabel=Left Dfn, subForest=[
              Node{rootLabel=Left ConsList, subForest=[
               Node{rootLabel=Left Add, subForest=[
                Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                Node{rootLabel=Right $ FuncArg 1, subForest=[]}]},
               Node{rootLabel=Left Call, subForest=[
                Node{rootLabel=Right $ FuncId 0, subForest=[]},
                Node{rootLabel=Right $ FuncArg 0, subForest=[]}]}]}]}
addFuncTest9 :: Test
addFuncTest9 =
  let (funcDesc1, _) = execState (evalNode testExpr8) (StrMap.empty, StrMap.empty)
      (funcDesc2, _) = execState (evalNode testExpr9) (funcDesc1, StrMap.empty)
  in funcDesc2 ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left AppLists, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 0), subForest = []}]}, retType = ListType (GenType (GenVar,1)), argType = StrMap.fromList [(0,ListType (GenType (GenVar,1)))]}),(1,FuncDesc {body = Node {rootLabel = Left ConsList, subForest = [Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 1), subForest = []}]},Node {rootLabel = Left Call, subForest = [Node {rootLabel = Right (FuncId 0), subForest = []},Node {rootLabel = Right (FuncArg 0), subForest = []}]}]}, retType = ListType (GenType (NumVar,0)), argType = StrMap.fromList [(0,GenType (NumVar,0)),(1,GenType (NumVar,0))]})]

testExpr10 :: Expression
testExpr10 = Node{rootLabel=Left Dfn, subForest=[
              Node{rootLabel=Left Map, subForest=[
               Node{rootLabel=Right $ FuncId 0, subForest=[]},
               Node{rootLabel=Right $ FuncArg 0, subForest=[]}]}]}
addFuncTest10 :: Test
addFuncTest10 =
  let expr0 = Node{rootLabel=Left Dfn, subForest=[
               Node{rootLabel=Left MkList, subForest=[
                Node{rootLabel=Left Add, subForest=[
                 Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                 Node{rootLabel=Right $ FuncArg 0, subForest=[]}]}]}]}
      (funcDesc1, _) = execState (evalNode expr0) (StrMap.empty, StrMap.empty)
      (funcDesc2, _) = execState (evalNode testExpr10) (funcDesc1, StrMap.empty)
  in funcDesc2 ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left MkList, subForest = [Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 0), subForest = []}]}]}, retType = ListType (GenType (NumVar,0)), argType = StrMap.fromList [(0,GenType (NumVar,0))]}),(1,FuncDesc {body = Node {rootLabel = Left Map, subForest = [Node {rootLabel = Right (FuncId 0), subForest = []},Node {rootLabel = Right (FuncArg 0), subForest = []}]}, retType = ListType (ListType (GenType (NumVar,0))), argType = StrMap.fromList [(0,ListType (GenType (NumVar,0)))]})]

testExpr11 :: Expression
testExpr11 = Node{rootLabel=Left Dfn, subForest=[
              Node{rootLabel=Left Fold, subForest=[
               Node{rootLabel=Right $ FuncId 0, subForest=[]},
               Node{rootLabel=Right $ FuncArg 0, subForest=[]},
               Node{rootLabel=Right $ FuncArg 1, subForest=[]}]}]}
addFuncTest11 :: Test
addFuncTest11 =
  let expr0 = Node{rootLabel=Left Dfn, subForest=[
               Node{rootLabel=Left ConsList, subForest=[
                Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                Node{rootLabel=Right $ FuncArg 1, subForest=[]}]}]}
      (funcDesc1, _) = execState (evalNode expr0) (StrMap.empty, StrMap.empty)
      (funcDesc2, _) = execState (evalNode testExpr11) (funcDesc1, StrMap.empty)
  in funcDesc2 ~?= StrMap.fromList [(0,FuncDesc {body = Node {rootLabel = Left ConsList, subForest = [Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 1), subForest = []}]}, retType = ListType (GenType (GenVar,1)), argType = StrMap.fromList [(0,GenType (GenVar,1)),(1,ListType (GenType (GenVar,1)))]}),(1,FuncDesc {body = Node {rootLabel = Left Fold, subForest = [Node {rootLabel = Right (FuncId 0), subForest = []},Node {rootLabel = Right (FuncArg 0), subForest = []},Node {rootLabel = Right (FuncArg 1), subForest = []}]}, retType = ListType (GenType (GenVar,1)), argType = StrMap.fromList [(0,ListType (GenType (GenVar,1))),(1,ListType (GenType (GenVar,1)))]})]

tests = TestList [addFuncTest1, addFuncTest2, addFuncTest3, addFuncTest4, addFuncTest5, addFuncTest6, addFuncTest7, addFuncTest8, addFuncTest8, addFuncTest10, addFuncTest11]
--main :: IO ()
main = runTestTT tests
  {-do
  putStrLn $ show $ addFuncTest1
  putStrLn $ show $ addFuncTest2
  putStrLn $ show $ addFuncTest3
  putStrLn $ show $ addFuncTest4
  putStrLn $ show $ addFuncTest5
  putStrLn $ show $ addFuncTest6
  putStrLn $ show $ addFuncTest7
  putStrLn $ show $ addFuncTest8
  putStrLn $ show $ addFuncTest9
  putStrLn $ show $ addFuncTest10
  putStrLn $ show $ addFuncTest11-}
  -- initialize some people, some goods
  -- map run allPeople
