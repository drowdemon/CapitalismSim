module Main
       where
import AddFunc
import EvalNode
import Mutate

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = hUnitTestToTests $ TestList [TestLabel "AddFunc: " addFuncTests
                                    ,TestLabel "EvalNode: " evalNodeTests
                                    ,TestLabel "Mutate: " mutateTests]

main = defaultMain tests
