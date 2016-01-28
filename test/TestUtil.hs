module TestUtil
       where
import Util
import qualified Data.List as Ls
import Data.Maybe

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

comp1 :: Int -> Int -> DupsEqWhich
comp1 x y =
  if x `mod` 19 == y `mod` 19
    then if x > y
           then DupL
           else DupR
    else DupNEQ

prop_Uniq :: (Int -> Int -> DupsEqWhich) -> [Int] -> Bool
prop_Uniq f xs =
  let res = nubOnKeep f xs
    in res == Ls.nub res

prop_comp1Representitive :: [Int] -> Bool
prop_comp1Representitive xs =
  let res = nubOnKeep comp1 xs
  in all (\x -> (1==) . length $ Ls.filter (\r -> case comp1 x r of {DupR -> True; _ -> False}) res) xs

{-checkOrderAppearance :: [Int] -> [Int] -> Bool
checkOrderAppearance [] _ = True
checkOrderAppearance _ [] = False --ran out of origs before result
checkOrderAppearance (x:xs) origs =
  checkOrderAppearance xs $ dropWhile (/=x) origs-}
  
testNubOnKeep = [testGroup "nubOnKeep" [
                    testProperty "comp1 uniq" (prop_Uniq comp1)
                   ,testProperty "comp1 Asc Range" prop_comp1Representitive
                   ]]
