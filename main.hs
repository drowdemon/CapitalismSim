module Main
       where
--import InternalProg
import Data.Tree
import BasicData
import EvalNode
import Control.Monad.Trans.State
import qualified Data.Map.Strict as StrMap

main :: IO ()
main = do
  putStrLn $ show $ ret  
  putStrLn $ show $ fTable
  -- initialize some people, some goods
  -- map run allPeople
  where expr = Node{rootLabel=Left Dfn, subForest=
                     [Node{rootLabel=Right $ FuncArg 0, subForest=[]}]}
        (ret,fTable) = runState (evalNode expr) StrMap.empty
