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

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

testAdd =
  let readerRes = getInsertableExprFunc (GenType (NumVar,0)) (fromJust $ (StrMap.lookup Add opMap)) (Left Add)
      res = runReader readerRes StrMap.empty
  in "simple addition" ~: res ~?= Just [Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (DatI 0), subForest = []},Node {rootLabel = Right (DatI 0), subForest = []}]},Node {rootLabel = Left Add, subForest = [Node {rootLabel = Right (DatD 0.0), subForest = []},Node {rootLabel = Right (DatD 0.0), subForest = []}]}]

mutateTests = TestList [testAdd]
