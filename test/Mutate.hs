module Mutate
       where
import LangData
import LangProc

--import Control.Monad.Trans.State
import Data.Tree
import qualified Data.Map.Strict as StrMap

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

testAdd =
  let res = getInsertableExprFunc
              FuncDesc{
                body=(Node (Left Add) [
                 (Node (Right $ FuncArg 0) []),
                 (Node (Right $ FuncArg 1) [])]),
                retType=GenType (NumVar, 0),
                argType=StrMap.fromList [(0,GenType (NumVar, 0)),(1,GenType (NumVar, 0))]}
              Add (GenType (NumVar,0))
  in "simple addition" ~: res ~?= Just [Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (DatI 0), subForest = []},Node {rootLabel = Right (DatI 0), subForest = []}]},Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (DatD 0.0), subForest = []},Node {rootLabel = Right (DatD 0.0), subForest = []}]}]

mutateTests = TestList [testAdd]
