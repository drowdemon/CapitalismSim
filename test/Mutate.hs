module Mutate
       where
import LangData
import OperatorDescriptions
import LangProc

--import Control.Monad.Trans.State
import Data.Tree
import qualified Data.Map.Strict as StrMap
import Control.Monad.Reader
import Data.Maybe
import Data.Tree.Zipper as Zp

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

testAdd =
  let readerRes = getInsertableExprFunc (GenType (NumVar,0)) (fromJust $ (StrMap.lookup Add opMap)) (Left Add)
      res = runReader readerRes (StrMap.empty,[])
  in "simple addition" ~: res ~?= Just [(Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (DatI 0), subForest = []},Node {rootLabel = Right (DatI 0), subForest = []}]},Weight 10,StrMap.empty),(Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (DatD 0.0), subForest = []},Node {rootLabel = Right (DatD 0.0), subForest = []}]},Weight 10,StrMap.empty)]
testOps =
  let loc = fromJust . Zp.firstChild . Zp.fromTree $ Node (Left Eq) [Node (Right $ DatI 5) [], Node (Right $ FuncArg 0) []]
      readerRes = getInsertMutationsAtPoint loc 1
      res = runReader readerRes (StrMap.empty,[])
  in "simple genvar getInsertableExprFunc test" ~: res ~?= []

mutateTests = TestList [testAdd
                       ,testOps]

