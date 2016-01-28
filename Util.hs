module Util
       where
import qualified Data.List as Ls
{-import qualified Data.Set as S
import qualified Data.Map.Strict as StrMap

--this one is getting too complicated, memory inefficient, high constants.
data NubEqWhich = NubNEQ | NubL | NubR
nubOrdOnKeep :: Ord a => (a -> a -> NubEqWhich) -> [a] -> [a]
nubOrdOnKeep outF outXs = StrMap.elems $ go S.empty StrMap.empty outF outXs
  where
    go :: Ord a => S.Set a -> StrMap.Map Int a -> (a -> a -> NubEqWhich) -> [a] -> StrMap.Map Int a
    go _ res _ [] = res
    go seen res f (x:xs) =
      case x `S.lookupIndex` seen of
        Nothing -> go (x `S.insert` seen) (StrMap.insert (StrMap.size res) x res) f xs
        Just i  -> case f x (S.elemAt i seen) of
                     NubL -> go (S.insert x $ S.deleteAt i seen) (
                     NubR -> 
-}

--runs in O(n*log(n))*O(f), though O(f) ought to always be constant time
--removes duplicates, changing list order. When it sees a duplicate, keeps it based on the given function
--except I can't use this, since my order is not compatible with my equality...
data DupsEqWhich = DupNEQ | DupL | DupR deriving Show
rmDupsOnKeep :: Ord a => (a -> a -> DupsEqWhich) -> [a] -> [a]
rmDupsOnKeep outF outXs =
  rmDups' outF $ Ls.sort outXs
  where
    rmDups' _ []       = []
    rmDups' _ (x:[])   = [x]
    rmDups' f (x:y:xs) =
      case f x y of
        DupNEQ -> x : rmDups' f (y:xs)
        DupL   -> x : rmDups' f xs
        DupR   -> rmDups' f (y:xs)

--runs in O(n^2)*O(f), though O(f) ought to always be constant time
--turns out I don't actually need this to maintain the order
--removes duplicates, maintaining list order. When it sees a duplicate, keeps it based on the given function
nubOnKeep :: (a -> a -> DupsEqWhich) -> [a] -> [a]
nubOnKeep comp outXs = nubOnKeep' [] comp outXs
 where
   nubOnKeep' :: [a] -> (a -> a -> DupsEqWhich) -> [a] -> [a]
   nubOnKeep' seen _ [] = seen
   nubOnKeep' seen f (x:xs) = nubOnKeep' (potentialUpdate' seen f x) f xs
   potentialUpdate' :: [a] -> (a -> a -> DupsEqWhich) -> a -> [a]
   potentialUpdate' [] _ x       = [x]
   potentialUpdate' (s:seen) f x =
     case f x s of
       DupNEQ -> s : potentialUpdate' seen f x
       DupL   -> x : seen
       DupR   -> s : seen
