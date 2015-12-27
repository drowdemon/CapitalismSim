module Main
       where
import LangProc
import LangData

import Control.Monad.Trans.State
import Data.Tree
import qualified Data.Map.Strict as StrMap

--main = undefined

exprMapFunc :: Expression
exprMapFunc = Node{rootLabel=Left Dfn, subForest=[
               Node{rootLabel=Left If, subForest=[
                Node{rootLabel=Left Gt, subForest=[
                 Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                 Node{rootLabel=Right $ DatI 10, subForest=[]}]},
                Node{rootLabel=Right $ FuncArg 0, subForest=[]},
                Node{rootLabel=Right $ DatI 0, subForest=[]}]}]}
exprMap :: Expression
exprMap = Node{rootLabel=Left Map, subForest=[
           Node{rootLabel=Right $ FuncId 0 , subForest=[]},
           Node{rootLabel=Right $ ListDat DatIVar [DatI 42, DatI 15, DatI 5, DatI 9], subForest=[]}]}
testMap =
  let (fTable,_) = execState (evalNode exprMapFunc) (StrMap.empty, StrMap.empty)
      res = evalState (evalNode exprMap) (fTable, StrMap.empty)
  in res

main = putStrLn (show testMap)
