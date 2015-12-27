module Main
       where
import AddFunc
import EvalNode

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = hUnitTestToTests $ TestList [TestLabel "AddFunc: " addFuncTests
                                    ,TestLabel "EvalNode: " evalNodeTests]

main = defaultMain tests
