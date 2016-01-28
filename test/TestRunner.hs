module Main
       where
import AddFunc
import EvalNode
import Mutate
import TestUtil

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = (hUnitTestToTests $ TestList [TestLabel "AddFunc: " addFuncTests
                                     ,TestLabel "EvalNode: " evalNodeTests
                                     ,TestLabel "Mutate: " mutateTests]
         ) ++ testNubOnKeep 

main = defaultMain tests
